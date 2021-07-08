open Lwt.Infix

[@@@part "0"]

(* ~~~ Irmin Store ~~~~ *)
module Client = struct
  include Cohttp_lwt_jsoo.Client

  let ctx () = None
end

[@@@part "1"]

module Store =
  Irmin_git.Generic
    (Irmin_indexeddb.Content_store)
    (Irmin_indexeddb.Branch_store)
    (Irmin.Contents.String)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)

[@@@part "2"]

(* No ocaml-git server... so using HTTP remote... *)
module Remote = Irmin_http.Client (Client) (Store)
module Sync = Irmin.Sync (Store)

[@@@part "3"]

type t = { main : Store.t; staging : Store.t; uri : Uri.t }

let info message () =
  Irmin.Info.v
    ~date:(Unix.gettimeofday () |> Int64.of_float)
    ~author:"omditor-client" message

let local_commit t k v =
  Store.set ~info:(info "some message goes here") t.staging k v >|= function
  | Ok () -> Brr.Console.log [ Jstr.v "Successful commit" ]
  | Error _ -> Brr.Console.warn [ Jstr.v "Set error" ]

let local_get t k = Store.get t.staging k

let sync ?(merge = true) t =
  ignore merge;
  let config = Irmin_http.config t.uri in
  let main = t.main in
  Remote.Repo.v config >>= fun repo ->
  Remote.master repo >>= fun remote ->
  Sync.pull_exn main ~depth:1 (Irmin.remote_store (module Remote) remote) `Set
  >>= fun _ ->
  if merge then
    Store.merge_into ~info:(info "update staging") ~into:t.staging main
  else Lwt_result.return ()

(* We're only using a one-level hierarchy so this is sufficient *)
let list t =
  Store.list t.staging [] >>= fun lst -> Lwt.return @@ List.map fst lst

let push ?(message = "merge") t =
  let config = Irmin_http.config t.uri in
  Remote.Repo.v config >>= fun repo ->
  Remote.master repo >>= fun remote ->
  sync t >>= fun _ ->
  let main = t.main in
  Store.merge_into ~info:(info message) ~into:main t.staging >>= fun _ ->
  Sync.push_exn main (Irmin.remote_store (module Remote) remote)

let repo = "/tmp/irmin-adventures"

let compare_commit a b =
  let a = Store.Commit.info a in
  let b = Store.Commit.info b in
  Int64.compare (Irmin.Info.date a) (Irmin.Info.date b)

let init uri =
  let config =
    Irmin_git.config ~bare:true
      ~config:(Irmin_indexeddb.config "client-db")
      repo
  in
  Store.Repo.v config >>= fun repo ->
  Store.master repo >>= fun main ->
  (* Abusing the API here a little for this one off case... *)
  sync ~merge:false { main; staging = main; uri } >>= fun _ ->
  Store.Branch.find repo "staging" >>= fun commit ->
  match commit with
  | None ->
      Store.clone ~src:main ~dst:"staging" >>= fun staging ->
      Lwt.return { main; staging; uri }
  | Some c ->
      Store.of_branch repo "staging" >>= fun staging ->
      Store.Head.get main >>= fun head ->
      if compare_commit head c < 0 then Lwt.return { main; staging; uri }
      else Lwt.return { main; staging; uri }

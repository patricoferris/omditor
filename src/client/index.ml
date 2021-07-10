(* Name clash with Brr.Uri *)
module URI = Uri
open Brr
open Brr_note
open Note
open Js_of_ocaml_lwt [@@part "0"]

(* Model *)
type t = { file : string; files : string list; editor : Jstr.t }

type action =
  [ `Update of bool * Jstr.t
  | `LocalCommit
  | `Push
  | `Sync
  | `ChangeFile of string ]

(* Async update event -- are global events like this a bad idea? I 
   need something to trigger on async calls like to the Irmin store? *)
let (refresh : action event), refresh_send = E.create ()

let update_editable t =
  match Document.find_el_by_id G.document @@ Jstr.v "editor" with
  | Some editable ->
      El.to_jv editable |> fun jv ->
      Jv.set jv "innerText" (Jv.of_jstr t.editor);
      t
  | _ -> t

let update (b, s) t =
  let t = { t with editor = s } in
  if b then update_editable t else t

let commit s t =
  Lwt_js_events.async (fun () ->
      Store.local_commit s [ t.file ] (t.editor |> Jstr.to_string |> Omd.of_string));
  t

let sync store t =
  Lwt_js_events.async (fun () -> Lwt.map ignore @@ Store.sync store);
  t

let push store t =
  Lwt_js_events.async (fun () -> Lwt.map ignore @@ Store.push store);
  t

let change_file store s t =
  let open Lwt.Infix in
  let f = { t with file = s } in
  Lwt_js_events.async (fun () ->
      Store.local_get store [ s ] >|= fun l ->
      refresh_send (`Update (true, Jstr.v l)));
  f

let reducer (store : Store.t) (action : [> action ]) =
  match action with
  | `Update s -> update s
  | `LocalCommit -> commit store
  | `Push -> push store
  | `Sync -> sync store
  (* Changing the file first locally commits it so you don't lose your changes! *)
  | `ChangeFile file -> fun t -> change_file store file (commit store t)
  | `UpdateEditable -> update_editable

(* Rendering *)
let set_inner_html el s =
  let jv = El.to_jv el in
  Jv.set jv "innerHTML" (Jv.of_jstr s)

let editor_ui t =
  let open Brr_note_kit in
  let (commit, commit_btn) : [> action ] event * El.t =
    let btn = Ui.Button.v (S.const [ El.txt' "Commit" ]) "commit" in
    let action = E.map (fun _ -> `LocalCommit) @@ Ui.Button.action btn in
    (action, Ui.Button.el btn)
  in
  let (push, push_btn) : [> action ] event * El.t =
    let btn = Ui.Button.v (S.const [ El.txt' "Push" ]) "push" in
    let action = E.map (fun _ -> `Push) @@ Ui.Button.action btn in
    (action, Ui.Button.el btn)
  in
  let (file, file_sel) : [> action ] event * El.t =
    let sel =
      Ui.Value_selector.Menu.v Jstr.v (S.const t.files) (S.const t.file)
    in
    let action =
      E.filter_map (fun s -> if s = t.file then None else Some (`ChangeFile s))
      @@ Ui.Value_selector.Menu.action sel
    in
    (action, Ui.Value_selector.Menu.el sel)
  in
  (E.select [ commit; file; push ], [ file_sel; commit_btn; push_btn ])

let editor t =
  match Document.find_el_by_id G.document @@ Jstr.v "editor" with
  | Some editable ->
      let editor_actions, editor_hdr = editor_ui t in
      let get_inner tgt =
        El.to_jv tgt |> fun t -> Jv.get t "innerText" |> Jv.to_jstr
      in
      let keys = Evr.on_el Ev.keyup Evr.unit editable in
      let str = E.map (fun _ -> `Update (false, get_inner editable)) keys in
      let viewer = El.div ~at:[ At.class' @@ Jstr.v "markdown" ] [] in
      let () =
        set_inner_html viewer
          (Jstr.v @@ Omd.to_html (Omd.of_string (Jstr.to_string t.editor)))
      in
      (E.select [ str; editor_actions ], editor_hdr @ [ viewer ])
  | _ -> failwith "arf!"

let main ed =
  let make_editor s = editor s in
  S.l1 ~eq:( == ) make_editor ed

let ui : store:Store.t -> initial:t -> t signal * El.t =
 fun ~store ~initial ->
  let def editor =
    let main = main editor in
    let action = E.swap @@ S.map ~eq:( == ) fst main in
    let action = E.select [ action; refresh ] in
    let items = S.map ~eq:( == ) snd main in
    let el = El.div ~at:[ At.class' @@ Jstr.v "markdown" ] [] in
    let () = Elr.def_children el items in
    let do_action = E.map (reducer store) action in
    let counter' = S.accum (S.value editor) do_action in
    (counter', (counter', el))
  in
  S.fix initial def

let editor ~store ~initial =
  let f, children = ui ~store ~initial in
  Logr.(hold @@ S.log f (fun _ -> ()));
  children

let init () = Store.init (URI.of_string "http://localhost:8080")

let app () =
  let open Lwt.Infix in
  let id = Jstr.v "app" in
  match Document.find_el_by_id G.document id with
  | None -> Console.(error [ str "No element with id '%s' found"; id ])
  | Some el ->
      let editable =
        El.div
          ~at:
            [
              At.class' @@ Jstr.v "text-editor";
              At.id @@ Jstr.v "editor";
              At.contenteditable true;
            ]
          []
      in
      let start () =
        init () >>= fun store ->
        Store.list store >>= fun files ->
        let file = List.hd files in
        Store.local_get store [ file ] >>= fun content ->
        El.set_children editable [ El.txt' content ];
        El.set_children el [ editable ];
        let initial = { file; files; editor = Jstr.v content } in
        El.append_children el [ editor ~store ~initial ];
        Lwt.return ()
      in
      Lwt_js_events.async start

let () = app ()

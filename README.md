# Omditor

*Status: WIP & Experimental*

Omditor is a incredibly simple markdown editor that uses [Omd][omd] for the preview and [Irmin][irmin] for the client-server storage. Omditor is offline-first meaning even without a connection you can use the application to edit your markdown and later use Irmin to sync and push your changes. Thanks to Irmin's mergeable and branchable structure we get a collaborative, offline-first web application almost for free.

What follows is an explanation of this repository, bare in mind this is all proof-of-concept not some production service.

## What's Included?

This is a very simple markdown editor with everything written in OCaml. It provides a git-like interface to staging changes to a set of predefined files. Given time constraints there's lots of missing functionality like adding files, having hierarchical file structure, adding a proper code editor not just a `contenteditable` div etc.

## How it works?

The client uses [Js_of_ocaml][jsoo] and [brr + note][brr] (my first foray into *functional reactive programming*). The server uses [cohttp][cohttp] and [crunches][crunch] the Javascript to be served. **Everything** uses [irmin][irmin].

### Server

The server has two main roles: to serve the web applications and to provide the HTTP endpoint for the [irmin][irmin] stores to communicate. Before doing that it first sets up a Unix, git, key-value store. 

<!-- $MDX file=src/server/main.ml,part=0 -->
```ocaml
module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)
module Sync = Irmin.Sync (Store)
module Http = Irmin_http.Server (Cohttp_lwt_unix.Server) (Store)
```

Layered over this store is the HTTP server endpoint. To combine this with an existing server I've had to do a bit of Irmin hacking to expose the callback rather than the [cohttp][cohttp] server (hence [irmin][irmin] is submoduled). I don't think it is possible to combine two cohttp servers once they are set up (I could be wrong). With the callback in hand we can set up the whole server callback which includes sending the web app down the line. 

<!-- $MDX file=src/server/main.ml,part=3 -->
```ocaml
let callback repo conn req body =
  let uri = Cohttp.Request.resource req in
  match uri with
  | "" | "/" | "/index.html" ->
      Server.respond_string ~status:`OK ~body:Html.html ()
  | "/static/index.js" ->
      Server.respond_string ~status:`OK
        ~body:(Assets.read "index.js" |> Option.get)
        ()
  | _irmin_path -> Http.callback repo conn req body
```

Whilst we're at it we can also pre-populate the [irmin][irmin] store with some contrived data. 

<!-- $MDX file=src/server/main.ml,part=2 -->
```ocaml
let store () =
  let config = Irmin_git.config ~bare:true repo in
  let* repo = Store.Repo.v config in
  let* t = Store.master repo in
  let* () = Store.set_exn ~info:(info "commit 1") t [ "hello.md" ] "# Hello!" in
  let* () = Store.set_exn ~info:(info "commit 2") t [ "salut.md" ] "# Salut!" in
  let+ () = Store.set_exn ~info:(info "commit 3") t [ "hola.md" ] "# Hola!" in
  repo
```

And finally we run the server at `http://localhost:8080`. 

<!-- $MDX file=src/server/main.ml,part=4 -->
```ocaml
let serve repo = Server.create (Server.make ~callback:(callback repo) ())

let main () =
  let* repo = store () in
  serve repo

let () = Lwt_main.run @@ main ()
```

### Client 

*TODO*

[irmin]: https://github.com/mirage/irmin
[cohttp]: https://github.com/mirage/ocaml-cohttp
[brr]: https://erratique.ch/software/brr
[jsoo]: https://github.com/ocsigen/js_of_ocaml
[omd]: https://github.com/ocaml/omd
[crunch]: https://github.com/mirage/ocaml-crunch

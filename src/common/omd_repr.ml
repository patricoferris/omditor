open Omd

type t = Omd.doc

let inline : 'a Irmin.Type.t -> 'a inline Irmin.Type.t = assert false

let attributes : attributes Irmin.Type.t = Repr.(list (pair string string))

let list_type : list_type Irmin.Type.t =
  let open Irmin.Type in
  variant "list_type" (fun ordered bullet -> function
    | Ordered (t1, t2) -> ordered (t1, t2) | Bullet t1 -> bullet t1)
  |~ case1 "Ordered" (pair int char) (fun (a, b) -> Ordered (a, b))
  |~ case1 "Bullet" char (fun c -> Bullet c)
  |> sealv

let list_spacing : list_spacing Irmin.Type.t =
  let open Irmin.Type in
  variant "list_spacing" (fun loose tight -> function
    | Loose -> loose | Tight -> tight)
  |~ case0 "Loose" Loose |~ case0 "Tight" Tight |> sealv

let def_elt : 'a Irmin.Type.t -> 'a def_elt Irmin.Type.t =
 fun t ->
  let open Irmin.Type in
  record "def_elt" (fun term defs -> { term; defs })
  |+ field "term" (inline t) (fun t -> t.term)
  |+ field "defs" (list (inline t)) (fun t -> t.defs)
  |> sealr

let rec block : 'a Irmin.Type.t -> 'a block Irmin.Type.t =
 fun attr_t ->
  let open Irmin.Type in
  variant "block" (fun para list blockquote thematic heading code html def ->
    function
    | Paragraph (t1, t2) -> para (t1, t2)
    | List (attr, t1, t2, t3) -> list (attr, t1, t2, t3)
    | Blockquote (attr, t1) -> blockquote (attr, t1)
    | Thematic_break attr -> thematic attr
    | Heading (attr, t1, t2) -> heading (attr, t1, t2)
    | Code_block (attr, t1, t2) -> code (attr, t1, t2)
    | Html_block (attr, t1) -> html (attr, t1)
    | Definition_list (attr, t1) -> def (attr, t1))
  |~ case1 "Paragraph"
       (pair attr_t (inline attr_t))
       (fun (a, b) -> Paragraph (a, b))
  |~ case1 "List"
       (quad attr_t list_type list_spacing (list (list (block attr_t))))
       (fun (a, b, c, d) -> List (a, b, c, d))
  |~ case1 "Blockquote"
       (pair attr_t (list (block attr_t)))
       (fun (a, b) -> Blockquote (a, b))
  |~ case1 "Thematic_break" attr_t (fun a -> Thematic_break a)
  |~ case1 "Heading"
       (triple attr_t int (inline attr_t))
       (fun (a, b, c) -> Heading (a, b, c))
  |~ case1 "Code_block" (triple attr_t string string) (fun (a, b, c) ->
         Code_block (a, b, c))
  |~ case1 "Html_block" (pair attr_t string) (fun (a, b) -> Html_block (a, b))
  |~ case1 "Definition_list"
       (pair attr_t (list (def_elt attr_t)))
       (fun (a, b) -> Definition_list (a, b))
  |> sealv

let t : t Irmin.Type.ty = Repr.list (block attributes)

(* open Irmin

let merge_attributes : attributes Merge.t =
  Merge.(alist Type.string Type.string (fun _ -> option string)) *)

(* TODO: Custom merge *)
let merge = Irmin.Merge.(option (idempotent t))

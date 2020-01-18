open Core

let (%) f g x = f (g x)
(** The things that exist. *)
type ontic_array = Bitv.t
type world = Bitv.t (* XXX: protect *)
type term = ontic_array -> ontic_array

(** The number of entities that can exist in the world *)
let world_size = 10000

(** How many objects exist *)
let n_objects = ref 0

(** In the beginning, there was nothing: [0,0,0,...] *)
let world = Bitv.create world_size false

let generate : unit -> int option =
  fun () ->
    if !n_objects > world_size
    then None
    else
      let () =
        Bitv.set world !n_objects true;
        incr(n_objects);
      in
      Some(!n_objects)

module Thing :
sig
  type t
  val create  : string -> t option
  val declare : string -> t
  val exists  : t -> bool
  val index   : t -> int
  val name    : t -> string
  val by_index : int -> t
end = struct
  exception Hubris

  type t =
    { name : string
    ; index : int }

  (** Named entities *)
  let things : string option Array.t =
    let f _ = None in
    Array.init world_size ~f

  let create name =
    let open Option.Monad_infix in
    generate ()
    >>= fun index -> Some {name; index}
    >>| fun thing ->
    Array.set things index (Some name);
    thing

  let declare name =
    match create name with
    | None -> raise Hubris
    | Some t -> t

  let exists t = Bitv.get world t.index
  let index t  = t.index
  let name t   = t.name

  let by_index index =
    match Array.get things index with
    | Some name -> {name; index}
    | None      -> {name = ""; index}
end

module type Operators = sig
  type t
  val ( ++ ) : t -> t -> t
  val ( ** ) : t -> t -> t
end

module Pred
  : sig
    type t
    module Infix : Operators with type t = t
    include Operators with type t := t
    val declare : unit -> t
    val nothing : t
    val define  : Thing.t list -> t
    val is_inhabited : t -> bool
    val cardinality : t -> int
    val extension : t -> Thing.t list
  end
= struct
  module T = struct
    type t = Bitv.t
    let declare () = Bitv.create world_size false
    let nothing = declare ()
    let define things =
      let pred = declare () in
      let f t = Bitv.set pred Thing.(index t) true in
      List.iter ~f things;
      pred

    let is_inhabited p = Bitv.to_int_us p > 0
    let cardinality p  = Bitv.to_int_s p
    let extension p =
      let f things index exists =
        if exists
        then Thing.by_index index :: things
        else things
      in
      Bitv.foldi_left f [] p
  end

  module Infix = struct
    type t = T.t
    let ( ++ ) = Bitv.bw_or
    let ( ** ) = Bitv.bw_and
  end
  include T
  include (Infix : Operators with type t := t)
end

module Term :
sig
  type t
  module Infix : Operators with type t = t
  include Operators with type t := t
  val is_thing : t -> bool
  val is_pred : t -> bool
  val sing : Thing.t -> t
  val to_pred : t -> Pred.t
  val sum : t -> t -> t
  val prod : t -> t -> t
  val thing : string -> t
  val pred : t list -> t
  val union : t list -> t
  val intersection : t list -> t
  val some : t -> t -> bool
  val all : t -> t -> bool
  val extension : t -> Thing.t list
  val enumerate : t -> unit
end = struct

  module T = struct
    type t =
      | P of Pred.t
      | T of Thing.t

    let is_thing = function
      | T _ -> true
      | _ -> false

    let is_pred = function
      | P _ -> true
      | _ -> false

    exception Impossible

    let sing x = P (Pred.define [x])
    let nothing = P (Pred.nothing)

    let to_pred t =
      (match t with
       | T x -> sing x
       | x   -> x)
      |>
      function
      | P p -> p
      | _ -> raise Impossible

    let call_on_preds f a b =
      let a = to_pred a
      and b = to_pred b
      in
      f a b

    let sum a b  = P(call_on_preds Pred.( ++ ) a b)
    let prod a b = P(call_on_preds Pred.Infix.( ** ) a b)

    let union terms =
      Option.value (List.reduce ~f:sum terms) ~default:nothing
    let intersection terms =
      Option.value (List.reduce ~f:prod terms) ~default:nothing

    let thing name = T (Thing.declare name)
    let pred terms = union terms

    let some a b = prod a b |> to_pred |> Pred.is_inhabited
    let all a b =
      let n_a = to_pred a |> Pred.cardinality
      and n_ab = prod a b |> to_pred |> Pred.cardinality in
      n_a = n_ab

    let extension = function
      | T t -> [t]
      | P p -> Pred.extension p

    let enumerate term =
      let f = print_endline % Thing.name in
      term
      |> extension
      |> List.iter ~f
  end

  module Infix = struct
    type t = T.t
    open T
    let ( ++ ) = sum
    let ( ** ) = prod
  end
  include T
  include (Infix : Operators with type t := t)
end

(* Singular things *)
let socrates  = Term.thing "socrates"
let aristotle = Term.thing "aristotle"
let socks     = Term.thing "socks"
let ferny     = Term.thing "ferny"
let dingus    = Term.thing "dingus"
let azrael    = Term.thing "azrael"
let lucifer   = Term.thing "lucifer"
let god       = Term.thing "god"

(* Predicates *)
let human    = Term.pred [socrates; aristotle]
let animal   = Term.pred [socks; human]
let plant    = Term.pred [ferny]
let mortal   = Term.pred [animal; plant]
let angel    = Term.pred [lucifer; azrael]
let immortal = Term.pred [angel; god]
let person   = Term.pred [human; angel]

let all_humans_are_mortal   = Term.(all human mortal)
let some_socrates_is_mortal = Term.(some socrates mortal)
let all_socrates_are_mortal = Term.(all socrates mortal)

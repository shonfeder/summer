open Core

(** The number of entities that can exist in the world *)
let world_size = 10000

(** The things that exist. *)
type ontic_array = Bitv.t
type world = ontic_array (* XXX: protect *)
type predicate = ontic_array
type term = ontic_array -> ontic_array

(** In the beginning, there was nothing: [0,0,0,...] *)
let world = Bitv.create world_size false

let particulars = String.Table.create () ~size:10000

let (++)
  : predicate -> predicate -> ontic_array
  = fun a b -> Bitv.bw_and a b

(** Find the first index not assigned to an entity *)
let find_free_index
  : unit -> int
  = fun () ->
    let rec searcher i =
      if not (Bitv.get world i)
      then i
      else searcher (i + 1)
    in
    searcher 0

(* let exists
 *   : string -> term
 *   = fun name ->
 *     let entity_index = find_free_index () in *)

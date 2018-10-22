(** 
   Representation of static monopoly data.
   This module represents the data stored in monopoly files, including current 
   balance and dice roll. 
*)

<<<<<<< HEAD
=======
(** The abstract type of values representing adventures. *)
type t

(** The type of location identifiers. *)
type location_id = int
                   <<<<<<< HEAD
  >>>>>>> 26f5937ae4f80acda3801e22ec69e38a242697e2
          =======

          (** [from_json j] is the adventure that [j] represents.
              Requires: [j] is a valid JSON monopoly board representation. *)
val from_json : Yojson.Basic.json -> t
  >>>>>>> 7aa02adafddc4dc90d51fcd214cec1e444b9fa7e

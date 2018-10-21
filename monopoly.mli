(** 
   Representation of static monopoly data.
   This module represents the data stored in monopoly files, including current 
   balance and dice roll. 
*)

(** The abstract type of values representing adventures. *)
type t

(** The type of location identifiers. *)
type location_id = int

(** [from_json j] is the adventure that [j] represents.
    Requires: [j] is a valid JSON monopoly board representation. *)
val from_json : Yojson.Basic.json -> t
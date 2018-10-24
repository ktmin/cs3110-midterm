(** 
   Representation of static Hogwarts data.
   This module represents the data stored in hogwarts files, including available 
   spells. 
*)


(** The abstract type of values representing hogwarts. *)
type t

(** [from_json j] is the organized spells that [j] represents.
    Requires: [j] is a valid JSON spell representation. *)
val from_json : Yojson.Basic.json -> t

(** [search s] looks for the spell [s] and returns the spell.
    Raises: UnknownSpellError  *)

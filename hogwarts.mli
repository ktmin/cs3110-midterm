(** 
   Representation of static Hogwarts data.
   This module represents the data stored in hogwarts files, including available 
   spells. 
*)


(** The abstract type of values representing hogwarts. *)
type t

(** The type of a spell name *)
type spell_name = string

(** The type of a spell's damage *)
type spell_damage = int 

(** The type of target *)
type target = string

(** The type of description *)
type description = string

(** The abstract type of spell  *)
type spell 

(** Raised when an unknown spell is encountered *)
exception UnknownSpell of spell

(** [from_json j] is the organized spells that [j] represents.
    Requires: [j] is a valid JSON spell representation. *)
val from_json : Yojson.Basic.json -> t

(** [shuffle t] shuffles the spells of howarts [t] *)
val shuffle : t -> t

(** [add_spell t s] adds the spell [s] to hogwarts [t]  *)
val add_spell : t -> spell -> t

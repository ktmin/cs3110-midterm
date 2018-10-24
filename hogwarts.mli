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

(** The type of spell  *)
type spell = {
  name: spell_name;
  damage: spell_damage;
  target: target;
  description: description;
}

(** Raised when an unknown spell is encountered *)
exception UnknownSpell of spell

(** [from_json j] is the organized spells that [j] represents.
    Requires: [j] is a valid JSON spell representation. *)
val from_json : Yojson.Basic.json -> t

(** [search spells spell] looks for the [spell] and returns it.
    Raises: UnknownSpell if [spell] is not found in [spells]  *)
val search : spell list -> spell_name -> spell

(**  *)


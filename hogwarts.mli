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
type damage = int 

(** The type of target *)
type target = string

(** The type of description *)
type description = string

(** The abstract type of spell information *)
type spell_info 

(** Raised when an unknown spell is encountered *)
exception UnknownSpell of spell_name

(** [from_json j] is the organized spells that [j] represents.
    Requires: [j] is a valid JSON spell representation. *)
val from_json : Yojson.Basic.json -> t

(** [search hogwarts spell] looks for the [spell] and returns it.
    Raises: UnknownSpell if [spell] is not found in [hogwarts]  *)
val search : t -> spell_name -> spell_info

(** [shuffle t] shuffles the spells of howarts [t] *)
val shuffle : t -> t

(** [add_spell t s] adds the spell [s] to hogwarts [t]  *)
val add_spell : t -> spell_info -> t

(** [spell_description h s] is the description of spell [s] in [h]. 
    Raises: UnknownSpell if spell [s] is not in [h] *)
val spell_description: t -> spell_name -> description

(** [spell_damage h s] is the amount of damage that spell [s] in [h] inflicts. 
    Raises: UnknownSpell if spell [s] is not in [h] *)
val spell_damage: t -> spell_name -> damage
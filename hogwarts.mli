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
val spell_damage: spell_info -> damage

(** [spell_name s] is the name of the spell [s] *)
val spell_name: spell_info -> spell_name

(** [get_spells t] is the list of spells in [t] *)
val get_spells: t -> spell_info list

(** [spell_target s] is the target for the spell [s] *)
val spell_target : spell_info -> target

(** [spell_level s] is the spell level for the spell [s] *)
val spell_level: spell_info -> int

(** [spell_dazd s] is the dazing effect of spell [s]. *)
val spell_daze : spell_info -> int

(** [spell_block s] is the blocking time for spell [s] *)
val spell_block : spell_info -> bool

(** [spell_remove_location s] is the location of removal after using spell [s]*)
val spell_remove_location : spell_info -> string

(** [spell_remove_amount s] is the amount of cards to remove after using spell 
    [s]. *)
val spell_remove_amount : spell_info -> int

(** [spell_long_effect s] is the long-term effect of using spell [s] *)
val spell_long_effect : spell_info -> int

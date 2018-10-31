(** 
   Representation of static Hogwarts data.
   This module represents the data stored in hogwarts files, including available 
   spells. 
*)


(** The abstract type of values representing hogwarts. *)
type t

(** The type of a spell name *)
type spell_name = string

(** The type of a character name *)
type character_name = string

(** The type of a spell's damage *)
type damage = int 

(** The type of a turns *)
type turns = int

(** The type of target *)
type target = string

(** The type of description *)
type description = string

(** The abstract type of spell information *)
type spell_info 

(** The abstract type of character information *)
type character_info

(** Raised when an unknown spell is encountered *)
exception UnknownSpell of spell_name

(** Raised when an unknown character is encountered *)
exception UnknownCharacter of character_name

(** [from_json j1 j2 ] is the organized spells that [j1] represents and the 
    characters that [j2] represents.
    Requires: [j1] and [j2] are valid JSON spell and character 
    representations. *)
val from_json : Yojson.Basic.json -> Yojson.Basic.json -> t

(** [search_spells t spell] looks for the [spell] and returns it.
    Raises: UnknownSpell if [spell] is not found in [t]  *)
val search_spells : t -> spell_name -> spell_info

(** [search_characters t c] looks for the character [c] and returns it.
    Raises: UnknownCharacter if [c] is not found in [t]  *)
val search_characters : t -> character_name -> character_info

(** [shuffle t] shuffles the spells of howarts [t] *)
val shuffle : t -> t

(** [add_spell t s] adds the spell [s] to hogwarts [t]  *)
val add_spell : t -> spell_info -> t

(** [spell_description h s] is the description of spell [s] in [h]. 
    Raises: UnknownSpell if spell [s] is not in [h] *)
val spell_description: t -> spell_name -> description

(** [character_description h c] is the description of character [c] in [h]. 
    Raises: UnknownCharacter if character [c] is not in [h] *)
val character_description: t -> character_name -> description

(** [spell_damage h s] is the amount of damage that spell [s] in [h] inflicts. 
    Raises: UnknownSpell if spell [s] is not in [h] *)
val spell_damage: spell_info -> damage

(** [spell_name s] is the name of the spell [s] *)
val spell_name: spell_info -> spell_name

(** [character_name c] is the name of the character [c] *)
val character_name: character_info -> character_name

(** [get_spells t] is the list of spells in [t] *)
val get_spells: t -> spell_info list

(** [get_characters t] is the list of characters in [t] *)
val get_characters: t -> character_info list

(** [spell_target s] is the target for the spell [s] *)
val spell_target : spell_info -> target

(** [spell_level s] is the spell level for the spell [s] *)
val spell_level: spell_info -> int

(** [character_level c] is the character level for character [c] *)
val character_level: character_info -> int

(** [spell_daze s] is the dazing effect of spell [s]. *)
val spell_daze : spell_info -> int

(** [spell_block s] is the blocking time for spell [s] *)
val spell_block : spell_info -> bool

(** [spell_remove s] is the removal effects of casting spell [s]. 
    Raises: UnknownSpell if spell [s] is does not have any information. *)
val spell_remove : spell_info -> string*int

(** [spell_long_effect s] is the long-term effect of using spell [s] *)
val spell_long_effect : spell_info -> damage*turns

(** [is_long_effect s] is whether [s] has a prolonged effect. *)
val is_long_effect : spell_info -> bool

(** [character_house c] is the house of character [c] *)
val character_house : character_info -> string

(** [character_hp c] is the hp of character [c] *)
val character_hp : character_info -> int
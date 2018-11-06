open Yojson.Basic.Util

type spell_name = string
type damage = int 
type target = string
type description = string
type turns = int

type character_name = string
type house = string

(** [remove] stores what will be romoved when casting a spell. Specifically from 
    where [location] and how many spell cards [amount]. *)
type remove = {location: string; amount: int}

(** [long_effect] stores the long-term effect of a spell. *)
type long_effect = {damage: damage; turns: turns}

(** [spell_info] stores each spell as a record. *)
type spell_info = {
  spell_name: spell_name;
  damage: damage;
  target: target;
  spell_description: description;
  spell_level: int;
  spell_type: string;
  daze: int;
  block: bool;
  remove: remove list;
  long_effect: long_effect list
}

(** [character_info] stores each character as a record. *)
type character_info = {
  character_name: character_name;
  house: house;
  character_level: int;
  character_description: description;
  hp: int
}

exception UnknownSpell of spell_name
exception UnknownCharacter of character_name

type t = {spells: spell_info list; characters: character_info list}

(** [extract_json j field f] is a helper to extract the information from [j] 
    with field name [field] and using function [f] to format correctly. *)
let extract_json j field f = 
  j |> member field |> f

(** [create_remove] extracts the remove information from [j]. *)
let create_remove j = 
  {
    location = extract_json j "loc" to_string;
    amount = extract_json j "count" to_int
  }

(** [create_long_effect j] extracts the long_effect information from [j]. *)
let create_long_effect j = 
  {
    damage = extract_json j "damage" to_int;
    turns = extract_json j "turns" to_int
  }

(** [create_spell j] extracts the spell information from [j]  *)
let create_spell j = 
  {
    spell_name = extract_json j "name" to_string |> String.lowercase_ascii;
    damage = extract_json j "damage" to_int;
    target = extract_json j "target" to_string;
    spell_description = extract_json j "description" to_string;
    spell_level = extract_json j "level" to_int;
    spell_type = extract_json j "type" to_string;
    daze = extract_json j "daze" to_int;
    block = extract_json j "block" to_bool;
    remove = (extract_json j "remove" to_list) |> List.map create_remove;
    long_effect = (extract_json j "long-effect" to_list) 
                  |> List.map create_long_effect
  }

(** [create_character j] extracts the character information from [j] *)
let create_character j =
  {
    character_name = extract_json j "name" to_string;
    house = extract_json j "house" to_string;
    character_level = extract_json j "level" to_int;
    character_description = extract_json j "description" to_string;
    hp = extract_json j "hp" to_int
  }

let from_json j1 j2 = 
  {
    spells = ((extract_json j1 "spells" to_list) |> List.map create_spell);
    characters = ((extract_json j2 "characters" to_list) 
                  |> List.map create_character);
  }

let get_spells hogwarts =
  hogwarts.spells

let get_characters hogwarts = 
  hogwarts.characters

(** [search_spells_helper spells spell] is a helper to search for [spell] in 
    [spells] 
    Raies UnknownSpell if [spell] is not found in [spells] *)
let rec search_spell_helper spells spell = 
  match spells with 
  | [] -> raise (UnknownSpell spell)
  | h::t -> if spell = h.spell_name then h else search_spell_helper t spell

(** [search_characters_helper chars char] is a helper to search for [char] in 
    [chars] 
    Raies UnknownCharacter if [char] is not found in [chars] *)
let rec search_characters_helper characters character = 
  match characters with 
  | [] -> raise (UnknownCharacter character)
  | h::t -> if character = h.character_name then h 
    else search_characters_helper t character

let search_spells hogwarts spell = 
  let spells = get_spells hogwarts in 
  search_spell_helper spells spell

let search_characters hogwarts character = 
  let characters = get_characters hogwarts in 
  search_characters_helper characters character

(**Implementation credit goes to A5 shuffle method provided to us.  *)
let shuffle hogwarts = 
  let spells = get_spells hogwarts in 
  {
    hogwarts with 
    spells = QCheck.Gen.(generate1 (shuffle_l spells))
  }

let add_spell hogwarts spell = 
  let spells = get_spells hogwarts in { hogwarts with spells = spell::spells}

let spell_description hogwarts spell = 
  let spell' = search_spells hogwarts spell in spell'.spell_description

let character_description character = 
  character.character_description

let spell_damage spell = 
  spell.damage

let spell_name spell =
  spell.spell_name

let character_name character = 
  character.character_name

let spell_target spell = 
  spell.target

let spell_level spell = 
  spell.spell_level

let character_level character = 
  character.character_level

let spell_type spell = 
  spell.spell_type

let spell_daze spell = 
  spell.daze

let spell_block spell = 
  spell.block

let spell_remove spell = 
  let remove_fields = spell.remove in 
  match remove_fields with 
  | [] -> raise (UnknownSpell spell.spell_name)
  | h::_ -> (h.location, h.amount)

let spell_long_effect spell = 
  let prolonged_effect = spell.long_effect in 
  match prolonged_effect with 
  | [] -> (0,0)
  | h::_ -> (h.damage,h.turns)

let is_long_effect spell = 
  let prolonged_effect = spell_long_effect spell in 
  if snd prolonged_effect <> 0 then true else false

let character_house character = 
  character.house

let character_hp character = 
  character.hp
open Yojson.Basic.Util

type spell_name = string
type damage = int 
type target = string
type description = string

(** [remove] stores what will be romoved when casting a spell. Specifically from 
    where [location] and how many spell cards [amount]. *)
type remove = {location: string; amount: int}

(** [spell] stores each spell as a record with fields name as a string, damage 
    as an int, target as a string, and description as a string  *)
type spell_info = {
  name: spell_name;
  damage: damage;
  target: target;
  description: description;
  level: int;
  spell_type: string;
  daze: int;
  block: int;
  remove: remove;
  long_effect: int
}

exception UnknownSpell of spell_name

type t = {spells: spell_info list}

(** [extract_json j field f] is a helper to extract the information from [j] 
    with field name [field] and using function [f] to format correctly. *)
let extract_json j field f = 
  j |> member field |> f

(** [create_reomve] extracts the remove information from [j]. *)
let create_remove j = 
  {
    location = extract_json j "location" to_string;
    amount = extract_json j "count" to_int
  }

(** [create_spell j] extracts the spell information from [j]  *)
let create_spell j = 
  {
    name = extract_json j "name" to_string |> String.lowercase_ascii;
    damage = extract_json j "damage" to_int;
    target = extract_json j "target" to_string;
    description = extract_json j "description" to_string;
    level = extract_json j "level" to_int;
    spell_type = extract_json j "type" to_string;
    daze = extract_json j "daze" to_int;
    block = extract_json j "block" to_int;
    remove = extract_json j "remove" create_remove;
    long_effect = extract_json j "long-effect" to_int
  }

let from_json j = 
  {
    spells = (j |> member "spells" |> to_list |> List.map create_spell)
  }

let get_spells hogwarts =
  hogwarts.spells



(** [search_helper spells spell] is a helper to search for [spell] in [spells] 
    Raies UnknownSpell if [spell] is not found in [spells] *)
let rec search_helper spells spell = 
  match spells with 
  | [] -> raise (UnknownSpell spell)
  | h::t -> if spell = h.name 
    then h else search_helper t spell

(** [search hogwarts spell] looks for the [spell] and returns it.
    Raises: UnknownSpell if [spell] is not found in [hogwarts]  *)
let search hogwarts spell = 
  let spells = get_spells hogwarts in 
  search_helper spells spell

(**Implementation credit goes to A5 shuffle method provided to us.  *)
let shuffle hogwarts = 
  let spells = get_spells hogwarts in 
  {
    spells = QCheck.Gen.(generate1 (shuffle_l spells))
  }

let add_spell hogwarts spell = 
  let spells = get_spells hogwarts in {spells = spell::spells}

let spell_description hogwarts spell = 
  let spell' = search hogwarts spell in spell'.description

let spell_damage spell = 
  spell.damage

let spell_name spell =
  spell.name

let spell_target spell = 
  spell.target

let spell_level spell = 
  spell.level

let spell_daze spell = 
  spell.daze

let spell_block spell = 
  spell.block

let spell_remove_location spell = 
  let remove_fields = spell.remove in 
  remove_fields.location

let spell_remove_amount spell = 
  let remove_fields = spell.remove in 
  remove_fields.amount

let spell_long_effect spell = 
  spell.long_effect
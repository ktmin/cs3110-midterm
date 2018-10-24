open Yojson.Basic.Util

type spell_name = string
type damage = int 
type target = string
type description = string


(** [spell] stores each spell as a record with fields name as a string, damage 
    as an int, target as a string, and description as a string  *)
type spell_info = {
  name: spell_name;
  damage: damage;
  target: target;
  description: description;
}

exception UnknownSpell of spell_name

type t = {spells: spell_info list}

(** [create_spell j] extracts the spell information from [j]  *)
let create_spell j = 
  {
    name = j |> member "name" |> to_string |> String.lowercase_ascii;
    damage = j |> member "damage" |> to_int;
    target = j |> member "target" |> to_string;
    description = j |> member "description" |> to_string
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

let shuffle hogwarts = 
  let spells = get_spells hogwarts in 
  {
    spells = QCheck.Gen.(generate1 (shuffle_l spells))
  }

let add_spell hogwarts spell = 
  let spells = get_spells hogwarts in {spells = spell::spells}

let spell_description hogwarts spell = 
  let spell' = search hogwarts spell in spell'.description

let spell_damage hogwarts spell = 
  let spell' = search hogwarts spell in spell'.damage

let spell_name sp =
  sp.name

let spell_target spell = 
  spell.target
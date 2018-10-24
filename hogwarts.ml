open Yojson.Basic.Util

type spell_name = string
type spell_damage = int 
type target = string
type description = string


type spell = {
  name: spell_name;
  damage: spell_damage;
  target: target;
  description: description;
}

let create_spell j = 
  {
    name = j |> member "name" |> to_string;
    damage = j |> member "damage" |> to_int;
    target = j |> member "target" |> to_string;
    description = j |> member "description" |> to_string
  }






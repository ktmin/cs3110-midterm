open Yojson.Basic.Util

type spell = {
  name: string;
  damage: int;
  target: string;
  description: string
}

type t = {spells: spell list}


let create_spell j = 
  {
    name = j |> member "name" |> to_string;
    damage = j |> member "damage" |> to_int;
    target = j |> member "target" |> to_string;
    description = j |> member "description" |> to_string
  }

let from_json j = 
  {
    spells = (j |> member "spells" |> to_list |> List.map create_spell)
  }


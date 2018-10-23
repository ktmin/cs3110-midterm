open Hogwarts
open Yojson.Basic.Util

type player_name = string
type player_health = int
type deck = spells
exception UnknownPlayer of player_name  

type player = {
  name : player_name;
  health: player_health;
  deck: deck;
}

let make_deck j = 
  {spls =  j |> member "deck" |> to_list |> List.map create_spell;
  }


let get_player j = {
  name = j |> member "name" |> to_string;
  health = j |> member "health" |> to_int;
  deck = make_deck j   
}

let get_name st =
  st.name


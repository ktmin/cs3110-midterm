open Hogwarts
open Yojson.Basic.Util

type player_name = string
type player_health = int
exception UnknownPlayer of player_name  

type player = {
  name : player_name;
  health: player_health;
}

type deck = {
  deck : spell list;
}

let get_player j = {
  name = j |> member "name" |> to_string;
  health = j |> member "health" |> to_int;   
}

let get_deck j = { 
  deck = j |> member "spells" |> to_list |> List.map create_spell; 
}

let get_name st =
  st.name

let get_health st = 
  st.health 

let get_hand st =
  st.hand


(** update hand and deck
    return updated hand*)
let draw st1 st2=




  let cast st =





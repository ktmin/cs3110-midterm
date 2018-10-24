open Hogwarts
open Yojson.Basic.Util

type player_name = string
type player_health = int
exception UnknownPlayer of player_name  

type player = {
  name : player_name;
  health: player_health;
}

type hand = {
  hand : spell list;
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
    returns a tuple 
    of updated hand and 
    deck*)
let draw chosen st1 st2=
  (st1.hand @ chosen, 
  match st2.deck with
  | [] -> []
  | h :: t -> t  
  )  

(**from stack overflow. plan to write my own code*)
let rec shuffle lst =
let nd = List.map (fun c -> (Random.bits (), c)) lst in
    let sond = List.sort compare nd in
    List.map snd sond



let rec cast chosen st =
  List.filter (fun x -> x <> chosen) st.hand
  

let rec casted damage st =
  st.health - damage   







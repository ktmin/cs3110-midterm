open Yojson.Basic.Util
open Hogwarts

type player_name = string
type player_hp = int


type player = {
  name : player_name;
  hp: player_hp;
}

type hand = {
  hand : spell_info list;
}

type deck = {
  deck : spell_info list;
}

let get_name st =
  st.name

let get_hp st = 
  st.hp 

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

let cast hogwarts chosen st =    
  let spell = Hogwarts.search hogwarts chosen in 
  (Hogwarts.spell_damage hogwarts chosen , 
   List.filter (fun x -> x <> spell) st.hand)

(** returns hp after the spell is casted*)
let casted hogwarts spell st = 
  let new_hp = st.hp - Hogwarts.spell_damage hogwarts spell in 
  {st with hp= new_hp }



(* module type Command = sig
   type player_name

   type player_hp

   type player

   type hand

   type deck

   val get_name: player -> player_name

   val get_hp: player -> player_hp 

   val get_hand: hand -> Hogwarts.spell_info list

   val draw: Hogwarts.spell_info list ->
    hand -> deck -> Hogwarts.spell_info list * Hogwarts.spell_info list

   val cast : 'a -> Hogwarts.spell_info -> hand -> 'a * Hogwarts.spell_info list

   end 

   module Command: Command = struct  
   type player_name = string
   type player_hp = int


   type player = {
    name : player_name;
    hp: player_hp;
   }

   type hand = {
    hand : spell_info list;
   }

   type deck = {
    deck : spell_info list;
   }

   let get_name st =
    st.name

   let get_hp st = 
    st.hp 

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

   let cast damage chosen st =    
    (damage,List.filter (fun x -> x <> chosen) st.hand)

   (** returns hp after the spell is casted*)
   let casted damage st = 
    st.hp - damage  

   end  *)






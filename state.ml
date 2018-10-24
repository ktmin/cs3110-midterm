open Yojson.Basic.Util
open Hogwarts

module type Command = sig
  type player_name

  type player_hp

  type player

  type hand

  type deck

  val get_name: player -> player_name

  val get_hp: player -> player_hp 

  val get_hand: hand -> Hogwarts.spell list

  val draw: Hogwarts.spell list ->
    hand -> deck -> Hogwarts.spell list * Hogwarts.spell list

  val cast : 'a -> Hogwarts.spell -> hand -> 'a * Hogwarts.spell list

end 

module Command: Command = struct  
  type player_name = string
  type player_hp = int


  type player = {
    name : player_name;
    hp: player_hp;
  }

  type hand = {
    hand : spell list;
  }

  type deck = {
    deck : spell list;
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

end 






(* open Hogwarts *)
type t = {
  name : string;
  hp: int;
  level: int;
  dazed: int;
  hand: Hogwarts.spell_info list;
  deck: Hogwarts.spell_info list;
}

let get_name st =
  st.name

let get_hp st =
  st.hp

let get_level st =
  st.level

let get_dazed st = 
  st.dazed

let get_hand (st:t) =
  st.hand

let get_deck (st:t) =
  st.deck

(*filter out deck so that the cards' level is 
  less than or equal to the player's level
  outputs state.t *)
let get_level_deck hogwarts st =
  let new_deck = List.filter (fun x -> st.level >= 
                                       Hogwarts.spell_level hogwarts) st.deck in {
    st with deck = new_deck
  }


let init_player hogwarts name =
  {name=name; hp=100; level = 0; dazed = 0; hand=[]; 
   deck=(QCheck.Gen.(generate1 (shuffle_l
                                  (Hogwarts.get_spells hogwarts))))
  }

let init_enemy hogwarts name =
  {name=name; hp=100; level = 0; dazed = 0;
   hand=[]; 
   deck=(QCheck.Gen.(generate1 (shuffle_l 
                                  (Hogwarts.get_spells hogwarts))))}

let draw (st:t) =
  match st.deck with
  | [] -> st
  | h::t -> {st with hand=(h::st.hand); deck=t}


let update_damage st spell = 
  let damage = st.hp - (Hogwarts.spell_damage spell) in
  {st with hp = damage} 

let update_caster st spell =
  let updated_hand = List.filter (fun x -> x <> spell) st.hand in 
  {st with hand = updated_hand}

let cast spell st1 st2 : (t*t) =
  let updated_hand = update_caster st1 spell in (
    if Hogwarts.spell_target spell = "self" then (
      let updated_health = update_damage updated_hand spell in
      (updated_hand,updated_health)
    )
    else (
      let updated_health = update_damage st2 spell in
      (updated_hand,updated_health)
    ))


let rec drop n lst = 
  if n = 0 then lst else
    drop (n-1) (
      match lst with 
      | [] -> []
      | h :: t -> t 
    )


(*removal from hand and deck is removing 
  first n elements*)
(*not sure what long-effect is suppose to do*)

(** shouldn't block be boolean*)


(*seperate function to handle block*)

let update spell st1 st2 = 
  if st1.dazed > 0 then
    let new_dazed = (st1.dazed - 1) in {
      st1 with
      dazed = new_dazed
    } else
  if Hogwarts.spell_block spell >= 0 then
    let new_dazed = (st1.dazed - 1) in {
      st1 with
      dazed = new_dazed  
    } else  
    let new_dazed = st1.dazed + (Hogwarts.spell_daze spell) in 
    let updated_health = st1.hp - (Hogwarts.spell_damage spell) in
    if Hogwarts.spell_target spell = "self" then (
      if Hogwarts.spell_remove_location spell= "hand" then

        let new_hand = drop (Hogwarts.spell_remove_amount spell)
            (st1.hand) in 
        {st1 with 
         hand= new_hand;
         hp = updated_health;
         dazed = new_dazed} 
      else if Hogwarts.spell_remove_location spell = "deck" then 
        let new_deck = drop (Hogwarts.spell_remove_amount spell)
            (st1.deck) in 
        {st1 with 
         deck= new_deck;
         hp = updated_health;
         dazed = new_dazed}    
      else
        {st1 with 
         hp = updated_health;
         dazed = new_dazed} 
    ) else (  
      let new_dazed = st2.dazed + (Hogwarts.spell_daze spell) in 
      let updated_health = st2.hp - (Hogwarts.spell_damage spell) in
      if Hogwarts.spell_remove_location spell = "hand" then

        let new_hand = drop (Hogwarts.spell_remove_amount spell)
            (st2.hand) in 
        {st2 with 
         hand= new_hand;
         hp = updated_health;
         dazed = new_dazed} 
      else if Hogwarts.spell_remove_location spell = "deck" then 
        let new_deck = drop (Hogwarts.spell_remove_amount spell)
            (st2.deck) in 
        {st2 with 
         deck= new_deck;
         hp = updated_health;
         dazed = new_dazed}    
      else
        {st2 with 
         hp = updated_health;
         dazed = new_dazed} 
    )







(** returns hp after the spell is casted*)
(* let casted hogwarts spell st = 
   let new_hp = st.hp - Hogwarts.spell_damage hogwarts spell in 
   {st with hp= new_hp } *)



(*Keeping this for later when we return to module method*)
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











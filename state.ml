(* open Hogwarts *)
type t = {
  name : string;
  hp: int;
  level: int;
  dazed: int;
  prolong_effect: (int * int) list;  
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

(** returns a list of prolong 
    damage that will be applied*)
let rec get_prolong_damage (st:t) =
  let rec get_damage effects =  
    match effects with
    | [] -> [] 
    | (d,_) :: t -> d :: 
                    get_damage t in 
  let prolong = st.prolong_effect in
  get_damage prolong   

(** returns a list of prolong 
    turns that will be applied*)
let rec get_prolong_turn (st:t) =
  let rec get_turn turns =  
    match turns with
    | [] -> [] 
    | (_,turn) :: t -> turn :: 
                       get_turn t in 
  let prolong = st.prolong_effect in
  get_turn prolong 


let get_level_deck  st =
  let new_deck = List.filter (fun x -> st.level >= 
                                       Hogwarts.spell_level x) st.deck in {
    st with deck = new_deck
  }


let init_player hogwarts name =
  {name=name; hp=100; level = 0; dazed = 0; 
   prolong_effect = []; hand=[]; 
   deck=(QCheck.Gen.(generate1 (shuffle_l
                                  (Hogwarts.get_spells hogwarts))))
  }

let init_enemy hogwarts name =
  {name=name; hp=100; level = 0; dazed = 0;
   prolong_effect = [];
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


let rec update_prolong_list (st:t) =
  let rec update_list effect =  
    match effect with 
    | [] -> [] 
    | (damage, turn) :: t ->
      if (turn = 1) then t else 
        (damage, turn -1) :: update_list t in 
  let prolong = st.prolong_effect in 
  update_list prolong    

let rec append_prolong spell prolong = 
  if Hogwarts.is_long_effect spell = true then 
    prolong @ [Hogwarts.spell_long_effect spell] else 
    prolong 

let update_prolong spell st =
  let updated = update_prolong_list st in 
  append_prolong spell updated    

let damage_from_prolong st = 
  let rec update_damage hp = 
    match hp with 
    | [] -> 0 
    | (damage, _) :: t ->
      damage + update_damage t in 
  update_damage st.prolong_effect 


let rec drop n lst = 
  if n = 0 then lst else
    drop (n-1) (
      match lst with 
      | [] -> []
      | h :: t -> t 
    )

(** [update_helper_dazed st prolonged_effect] is a helper for update for if dazed 
    is true. *)
let update_helper_dazed st = 
  let new_dazed = (st.dazed - 1) in {
    st with
    dazed = new_dazed
  }

(** [update_helper_block st prolonged_effect] is a helper for update for if block
    is true. *)
let update_helper_block st  = 
  let new_dazed = (st.dazed - 1) in {
    st with
    dazed = new_dazed
  }

(** [update_helper_hand st spell u_health u_dazed prolonged effect] is a helper 
    for update if hand is being updated. *)
let update_helper_hand st spell u_health u_dazed = 
  let new_hand = drop (snd(Hogwarts.spell_remove spell))
      (st.hand) in 
  {st with 
   hand= new_hand;
   hp = u_health;
   dazed = u_dazed;
  } 

(** [update_helper_deck st spell u_health u_dazed prolonged effect] is a helper 
    for update if deck is being updated. *)
let update_helper_deck st spell u_health u_dazed = 
  let new_deck = drop (snd (Hogwarts.spell_remove spell))
      (st.deck) in 
  {st with 
   deck= new_deck;
   hp = u_health;
   dazed = u_dazed;
  } 

(** [update_helper_health st prolonged_effect prolonged_damage spell] is a helper 
    for updating the health of [st]. *)
let update_helper_health st  spell = 
  let new_dazed = st.dazed + (Hogwarts.spell_daze spell) in 
  let updated_health = st.hp - Hogwarts.spell_damage spell in 
  if fst (Hogwarts.spell_remove spell) = "hand" 
  then update_helper_hand st spell updated_health new_dazed 
  else (if fst (Hogwarts.spell_remove spell) = "deck" 
        then update_helper_deck st spell updated_health new_dazed
        else
          {st with 
           hp = updated_health;
           dazed = new_dazed} )



let update spell st1 st2 =    
  if st1.dazed > 0 then (update_helper_dazed st1 ) 
  else (if Hogwarts.spell_block spell = true 
        then update_helper_block st1 
        else (if Hogwarts.spell_target spell = "self" 
              then (update_helper_health st1 spell) 
              else (update_helper_health st2 spell)))




(*
let update spell st1 st2 = 
  let new_prolong_effect = update_prolong spell st1 st2 in 
  let prolong_damage = p_damage spell st1 st2 in   
  let updated_hp = st1.hp - (Hogwarts.spell_damage spell) - 
                         prolong_damage in 
  if st1.dazed > 0 then
    let new_dazed = (st1.dazed - 1) in {
      st1 with
      dazed = new_dazed;
      prolong_effect = new_prolong_effect;
      hp = updated_hp 
    } else
  if Hogwarts.spell_block spell = true then
    let new_dazed = (st1.dazed - 1) in {
      st1 with
      dazed = new_dazed; 
      prolong_effect = new_prolong_effect 
    } else  
    let new_dazed = st1.dazed + (Hogwarts.spell_daze spell) in 
    let updated_health = st1.hp - (Hogwarts.spell_damage spell) - 
                         prolong_damage in
    if Hogwarts.spell_target spell = "self" then (
      if fst (Hogwarts.spell_remove spell) = "hand" then
        let new_hand = drop (snd(Hogwarts.spell_remove spell))
            (st1.hand) in 
        {st1 with 
         hand= new_hand;
         hp = updated_health;
         dazed = new_dazed;
         prolong_effect= new_prolong_effect} 
      else if fst (Hogwarts.spell_remove spell) = "deck" then 
        let new_deck = drop (snd (Hogwarts.spell_remove spell))
            (st1.deck) in 
        {st1 with 
         deck= new_deck;
         hp = updated_health;
         dazed = new_dazed;
         prolong_effect= new_prolong_effect
        }    
      else
        {st1 with 
         hp = updated_health;
         dazed = new_dazed;
         prolong_effect = new_prolong_effect} 
    ) else (  
      let new_dazed = st2.dazed + (Hogwarts.spell_daze spell) in 
      let updated_health = st2.hp - (Hogwarts.spell_damage spell) -
                           prolong_damage in                      
      if fst (Hogwarts.spell_remove spell) = "hand" then

        let new_hand = drop (snd (Hogwarts.spell_remove spell))
            (st2.hand) in 
        {st2 with 
         hand= new_hand;
         hp = updated_health;
         dazed = new_dazed;
         prolong_effect= new_prolong_effect} 
      else if (fst (Hogwarts.spell_remove spell)) = "deck" then 
        let new_deck = drop (snd (Hogwarts.spell_remove spell))
            (st2.deck) in 
        {st2 with 
         deck= new_deck;
         hp = updated_health;
         dazed = new_dazed;
         prolong_effect=new_prolong_effect}    
      else
        {st2 with 
         hp = updated_health;
         dazed = new_dazed;
         prolong_effect= new_prolong_effect} 
    )
 *)   



(**updates state after drawing*)
let hand_after_cast spell st =
  let new_hand = List.filter (fun x -> spell <>
                                       x) st.hand in {
    st with hand = new_hand
  }


(**everytime one casts a spell prolong_damage is 
   updated *)
let update_prolong_damage spell st =
  let new_prolong_effect = update_prolong spell st in 
  let new_prolong_damage = damage_from_prolong st in 
  { st with 
    prolong_effect = new_prolong_effect;
    hp = st.hp - new_prolong_damage;
  }


let cast spell st1 st2 : (t*t) =
  if st1.dazed > 0 then
    let updated_self =update spell st1 st2 in 
    (hand_after_cast spell updated_self,update_prolong_damage spell st2) else   
  if Hogwarts.spell_block spell = true then
    let updated = update spell st1 st2 in 
    (hand_after_cast  spell updated, update_prolong_damage spell st2) 
  else 

  if Hogwarts.spell_target spell = "self" then (
    let updated_self = update spell st1 st2 in
    (hand_after_cast spell updated_self, update_prolong_damage spell st2)
  )
  else (
    let updated_enemy = update spell st1 st2 in
    (hand_after_cast spell st1, update_prolong_damage spell updated_enemy)
  )





(*removal from hand and deck is removing 
  first n elements*)
(*not sure what long-effect is suppose to do*)

(** shouldn't block be boolean*)


(*seperate function to handle block*)

(*takes care of damage done by 
  prolong spell*)












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











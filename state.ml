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
    prolong @ [(Hogwarts.spell_long_effect spell)] else 
    prolong 

let update_prolong spell st1 st2 =
  if (Hogwarts.spell_target spell) = "self" then
    let updated = update_prolong_list st1 in 
    append_prolong spell updated else 
    let updated = update_prolong_list st2 in 
    append_prolong spell updated  

let damage_from_prolong st = 
  let rec update_damage hp = 
    match hp with 
    | [] -> 0 
    | (damage, _) :: t ->
      damage + update_damage t in 
  update_damage st.prolong_effect 


let p_damage spell st1 st2 =
  if Hogwarts.spell_target spell = "self" then 
    damage_from_prolong st1 else 
    damage_from_prolong st2  

let rec drop n lst = 
  if n = 0 then lst else
    drop (n-1) (
      match lst with 
      | [] -> []
      | h :: t -> t 
    )

let update spell st1 st2 = 
  let new_prolong_effect = update_prolong spell st1 st2 in 
  let prolong_damage = p_damage spell st1 st2 in   
  if st1.dazed > 0 then
    let new_dazed = (st1.dazed - 1) in {
      st1 with
      dazed = new_dazed;
      prolong_effect = new_prolong_effect
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

let cast spell st1 st2 : (t*t) =
  if Hogwarts.spell_target spell = "self" then (
    let updated_self = update spell st1 st2 in
    (updated_self, st2)
  )
  else (
    let updated_enemy = update spell st1 st2 in
    (st1,updated_enemy)
  )

(*TODO: remove this*)
let to_list_hand pl : Hogwarts.spell_info list =
  pl.hand





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











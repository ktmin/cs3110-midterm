open Yojson.Basic 
(* open Hogwarts *)
type t = {
  name : string;
  house : string;
  full_hp: int;
  hp: int;
  level: int;
  dazed: int;
  blocked: int;
  prolong_effect: (int * int) list;  
  hand: Hogwarts.spell_info list;
  deck: Hogwarts.spell_info list;
  defeated_enemies: Hogwarts.character_name list
}

let get_house st =
  st.house

let get_name st =
  st.name

let get_full_hp st = 
  st.full_hp

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

let get_blocked (st:t) = 
  st.blocked


let get_defeated_enemies st = 
  st.defeated_enemies

let add_defeated_enemy st enemy hogwarts = 
  let defeated_enemy = (Hogwarts.search_characters hogwarts enemy) in 
  let prev_defeated = get_defeated_enemies st in 
  let defeated = (Hogwarts.character_name defeated_enemy)::prev_defeated in
  {
    st with defeated_enemies = defeated
  }

let rec get_prolong_damage (st:t) =
  let rec get_damage effects =  
    match effects with
    | [] -> [] 
    | (d,_) :: t -> d :: 
                    get_damage t in 
  let prolong = st.prolong_effect in
  get_damage prolong   

let rec get_prolong_turn (st:t) =
  let rec get_turn turns =  
    match turns with
    | [] -> [] 
    | (_,turn) :: t -> turn :: 
                       get_turn t in 
  let prolong = st.prolong_effect in
  get_turn prolong 

let get_prolong_tupes (st:t) =
  st.prolong_effect

let get_level_deck  st =
  let new_deck = List.filter (fun x -> st.level >= 
                                       Hogwarts.spell_level x) st.deck in {
    st with deck = new_deck
  }


let init_player hogwarts name house =
  {name=name; full_hp = 100; hp=100; level = 1; dazed = 0; blocked = 0;
   prolong_effect = []; hand=[]; 
   deck=(QCheck.Gen.(generate1 (shuffle_l
                                  (Hogwarts.get_spells hogwarts))));
   house=house;
   defeated_enemies = []
  }

let init_player_with_level_deck hogwarts name house = 
  let player = init_player hogwarts name house in
  get_level_deck player

let init_enemy hogwarts name house =
  let enemy_info = Hogwarts.search_characters hogwarts name in 
  let enemy_level = Hogwarts.character_level enemy_info in 
  let enemy_hp = Hogwarts.character_hp enemy_info in
  {name=name; full_hp =enemy_hp; hp= enemy_hp; level = enemy_level; dazed = 0;
   prolong_effect = [];
   hand=[]; blocked = 0;
   deck=(QCheck.Gen.(generate1 (shuffle_l 
                                  (Hogwarts.get_spells hogwarts))));
   house=house;
   defeated_enemies = []
  }

let init_enemy_with_level_deck hogwarts name house =
  get_level_deck (init_enemy hogwarts name house)

let refresh_deck hogwarts (st:t) = 
  let new_deck = (QCheck.Gen.(generate1 (shuffle_l 
                                           (Hogwarts.get_spells hogwarts)))) in 

  let new_level_deck = List.filter (fun x -> st.level >= 
                                             Hogwarts.spell_level x) new_deck in 
  {st  with deck = new_level_deck}

let draw (st:t) =
  match st.deck with
  | [] -> 
    let new_deck =  
      refresh_deck (Hogwarts.from_json (from_file "spells.json") 
                      (from_file "characters.json")) st in
    let new_hand = List.hd new_deck.deck in    
    {st with deck = new_deck.deck;
             hand = new_hand :: st.hand }
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

let update_dazed st = 
  let new_dazed = (st.dazed - 1) in {
    st with
    dazed = new_dazed
  }

(** [update_helper_block st prolonged_effect]
     is a helper for update for if 
    block is true. *)
let update_helper_block st  = 
  if st.dazed > 0 then 
    let new_dazed = (st.dazed - 1) in {
      st with
      dazed = new_dazed
    } else 
    st

(** [update_helper_hand st spell u_health u_dazed prolonged effect] 
    is a helper for update if hand is being updated. *)
let update_helper_hand st spell u_health u_dazed = 
  let new_hand = drop (snd(Hogwarts.spell_remove spell))
      (st.hand) in 
  {st with 
   hand= new_hand;
   hp = u_health;
   dazed = u_dazed;
  } 

(** [update_helper_deck st spell u_health u_dazed prolonged effect]
     is a helper 
    for update if deck is being updated. *)
let update_helper_deck st spell u_health u_dazed = 
  let new_deck = drop (snd (Hogwarts.spell_remove spell))
      (st.deck) in 
  {st with 
   deck= new_deck;
   hp = u_health;
   dazed = u_dazed;
  } 

(** [update_helper_health st prolonged_effect 
    prolonged_damage spell] is a 
    helper for updating the health of [st]. *)
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
  if st1.dazed > 0 then (update_dazed st1 ) 
  else (if Hogwarts.spell_block spell = true 
        then update_helper_block st2 
        else (if Hogwarts.spell_target spell = "self" 
              then (update_helper_health st1 spell) 
              else (update_helper_health st2 spell)))

let hand_after_cast spell st =
  let rec after_cast spell hand = 
    match hand with 
    | [] -> []
    | h :: t -> if h = spell then
        t else h :: (after_cast spell t)  
  in  
  let new_hand = after_cast spell st.hand in
  {st with hand = new_hand}


(** [update_prolong_damage Hogwarts.spell_info t]
    is state t after prolong damage takes effect 
    in one turn*)
let update_prolong_damage spell st =
  let new_prolong_effect = update_prolong spell st in 
  let new_prolong_damage = damage_from_prolong st in 
  { st with 
    prolong_effect = new_prolong_effect;
    hp = st.hp - new_prolong_damage;
  }

(** [update_blocked Hogwarts.spell_info t]
    is state t after blocking spell takes effect 
    in one turn*)
let update_blocked spell st = 
  if Hogwarts.spell_block spell = true then 
    let new_block = 1 in   
    {st with   
     blocked = new_block} else
    st 

(** [reset_blocked Hogwarts.spell_info t]
    resets the blocked field of t to 
    0*)
let reset_blocked spell st = 
  {st with 
   blocked = 0} 


let cast spell st1 st2 : (t*t) =
  if st1.dazed > 0 then
    let updated_self =update spell st1 st2 in 
    (updated_self,update_prolong_damage spell st2) else 
  if st1.blocked = 1 then 
    let updated_pl = reset_blocked spell st1 in
    let self_updated_pl = if Hogwarts.spell_target spell = "self"
      then update spell updated_pl st2 else 
        updated_pl in  
    (hand_after_cast spell self_updated_pl, st2) 
  else   
  if Hogwarts.spell_block spell = true then
    (hand_after_cast  spell (st1), 
     update_blocked spell
       (update_prolong_damage spell   (update spell st1  st2)   )) 
  else 

  if Hogwarts.spell_target spell = "self" then (
    let updated_self = update spell st1 st2 in
    (hand_after_cast spell updated_self, update_prolong_damage spell st2)
  )
  else (
    let updated_enemy = update spell st1 st2 in
    (hand_after_cast spell st1, update_prolong_damage spell updated_enemy)
  )


let required_wins (player:t) =
  (player.level) * 2

let level_up (player:t) : t =
  let req = required_wins player in
  if List.length player.defeated_enemies >= req then
    {player with level = player.level + 1}
  else player


let to_list_hand pl : Hogwarts.spell_info list =
  pl.hand



(** [execute_action spell pl_st ai_st] is the cast of [spell] from [ai_st] on 
    [pl_st]. *)
let execute_action spell ai_st pl_st =
  State.cast spell ai_st pl_st

(** [find_best_spell lst cmp] returns the best spell in [lst] by 
    comparing the result of [f] using [op]. *)
let rec find_best_spell lst cmp f op = 
  match lst with 
  | [] -> cmp
  | h::t -> if (op (f h) (f cmp))
    then find_best_spell t h f op
    else find_best_spell t cmp f op

(** [find_best_long_effect_spell lst cmp] is a version of [find_best_spell] but 
    specifically for finding the best long_effect spell in [lst] by using spell 
    [cmp] for comparison. *)
let rec find_best_long_effect_spell lst cmp = 
  match lst with 
  | [] -> cmp
  | h::t -> 
    let prev_damage = 
      fst(Hogwarts.spell_long_effect cmp) * snd(Hogwarts.spell_long_effect cmp) 
    in 
    let new_damage = 
      fst(Hogwarts.spell_long_effect h) * snd(Hogwarts.spell_long_effect h) 
    in 
    if new_damage > prev_damage 
    then find_best_long_effect_spell t h 
    else find_best_long_effect_spell t cmp


(** [hand_search hand spell_type] searches [hand] for best spell that matches 
    the [spell_type] being taken into consideration. *)
let rec hand_search hand spell_type f op = 
  let filtered_hand = 
    List.filter (fun x -> Hogwarts.spell_type x = spell_type) hand in
  match filtered_hand with 
  | [] -> None
  | h::t -> if spell_type = "persistent" 
    then Some (find_best_long_effect_spell filtered_hand (List.hd filtered_hand))
    else Some (find_best_spell filtered_hand (List.hd filtered_hand) f op)

(** [has_attack hand player ai] is the cast on [player] dependant on whether if 
    an attack spell is in the [hand] of the [ai]. *)
let has_attack ai_hand pl_state ai_state =
  let spell = hand_search ai_hand "attack" Hogwarts.spell_damage (>) in 
  match spell with 
  | None -> execute_action (List.hd ai_hand) ai_state pl_state
  | Some attack_spell -> execute_action attack_spell ai_state pl_state

(** [has_block hand player ai] is the cast on [player] dependant on whether if 
    a blocking spell is in the [hand] of the [ai]. *)
let has_block ai_hand pl_state ai_state =
  let filtered_hand = 
    List.filter (fun x -> Hogwarts.spell_type x = "blocking") ai_hand in
  match filtered_hand with 
  | [] -> execute_action (List.hd ai_hand) ai_state pl_state
  | h::t -> execute_action (List.hd filtered_hand) ai_state pl_state

(** [has_long_effect hand player ai] is the cast on [player] dependant on 
    whether if a long_effect spell is in the [hand] of the [ai]. *)
let has_long_effect ai_hand pl_state ai_state =
  let spell = hand_search ai_hand "persistent" Hogwarts.spell_long_effect (>) in 
  match spell with 
  | None -> has_block ai_hand pl_state ai_state
  | Some persistent_spell -> execute_action persistent_spell ai_state pl_state

(** [has_daze hand player ai] is the cast on [player] dependant on whether if 
    a daze spell is in the [hand] of the [ai]. *)
let has_daze ai_hand pl_state ai_state =
  let spell = hand_search ai_hand "stunning" Hogwarts.spell_daze (>) in 
  match spell with 
  | None -> has_long_effect ai_hand pl_state ai_state
  | Some daze_spell -> execute_action daze_spell ai_state pl_state

(** [has_healing hand player ai] is the cast on [player] dependant on whether 
    a healing spell is in the [hand] of the [ai]. *)
let has_healing ai_hand pl_state ai_state condition =
  let spell = hand_search ai_hand "healing" Hogwarts.spell_damage (<) in 
  match spell with 
  | None -> if condition = "attack" then has_attack ai_hand pl_state ai_state
    else has_daze ai_hand pl_state ai_state
  | Some healing_spell -> execute_action healing_spell ai_state pl_state

(** [is_full_health ai_state pl_state] is the cast of the [ai_state] on 
    [pl_state] dependant on its current health. *)
let is_full_health ai_state pl_state = 
  let ai_hand = State.get_hand ai_state in 
  if State.get_hp ai_state = State.get_full_hp ai_state 
  then has_attack ai_hand pl_state ai_state
  else has_healing ai_hand pl_state ai_state "attack"

(** [is_game_ending ai_state pl_state] determines if [ai_state] can win right 
    away against [pl_state]. *)
let is_game_ending ai_state pl_state = 
  let ai_hand = State.get_hand ai_state in 
  let best_spell = hand_search ai_hand "attack" Hogwarts.spell_damage (>) in 
  match best_spell with 
  | None -> has_healing ai_hand ai_state pl_state ""
  | Some spell -> 
    if (Hogwarts.spell_damage spell >= State.get_hp pl_state)
    then execute_action spell ai_state pl_state 
    else has_healing ai_hand pl_state ai_state ""

let enemy_decision ai_state pl_state = 
  if State.get_blocked ai_state > 0 
  then is_full_health ai_state pl_state
  else is_game_ending ai_state pl_state
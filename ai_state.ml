type  decision_tree = 
  | Leaf of (Hogwarts.spell_info -> State.t -> State.t -> State.t * State.t )
  | Node of decision_tree * (unit -> decision_tree) * decision_tree


let execute_action spell pl_st ai_st =
  State.cast spell ai_st pl_st

(** [find_killing_spell lst cmp] returns the best spell in [lst] by 
    comparing [f]. *)
let rec find_best_spell lst cmp f op = 
  match lst with 
  | [] -> cmp
  | h::t -> if (op (f h) (f cmp))
    then find_best_spell t h f op
    else find_best_spell t cmp f op

(** [hand_search hand spell_type] searches [hand] for best spell that matches 
    the [spell_type] being taken into consideration. *)
let rec hand_search hand spell_type f op = 
  let filtered_hand = 
    List.filter (fun x -> Hogwarts.spell_type x = spell_type) hand in
  match filtered_hand with 
  | [] -> None
  | h::t -> Some (find_best_spell filtered_hand (List.hd filtered_hand) f op)

let has_attack ai_hand spell_type pl_state ai_state =
  let spell = hand_search ai_hand "attack" Hogwarts.spell_damage (>) in 
  match spell with 
  | None -> execute_action (List.hd ai_hand) pl_state ai_state
  | Some s -> execute_action s pl_state ai_state

let has_healing ai_hand spell_type pl_state ai_state =
  let spell = hand_search ai_hand "healing" Hogwarts.spell_damage (<) in 
  match spell with 
  | None -> has_attack ai_hand spell_type pl_state ai_state
  | Some s -> execute_action s pl_state ai_state

(** [is_full_health ai_state pl_state] is the cast of the [ai_state] on 
    [pl_state] dependant on its current health. *)
let is_full_health ai_state pl_state = 
  let ai_hand = State.get_hand ai_state in 
  if State.get_hp ai_state = State.get_full_hp ai_state 
  then has_attack ai_hand "attack" pl_state ai_state
  else has_healing ai_hand "healing" pl_state ai_state 

(** [is_game_ending ai_state pl_state] determines if [ai_state] can win right 
    away against [pl_state]. *)
let is_game_ending ai_hand ai_state pl_state = 
  let selected_spell = hand_search ai_hand "attack" in 
  match selected_spell with 
  | None -> failwith("call stunning function")
  | Some spell -> 
    let killing_spell = find_best_spell ai_hand spell Hogwarts.spell_damage 
    in  
    if Hogwarts.spell_damage killing_spell >= State.get_hp pl_state 
    then execute_action spell ai_state pl_state 
    else failwith("call stunning function")

(** [traverse_right ai_hand spell_type] is a helper to check if the ai should 
    traverse to the right child. It's based on the [ai_hand] and the 
    [spell type].*)
let traverse_right ai_hand spell_type = 
  let selected_spell = hand_search ai_hand spell_type in 
  match selected_spell with 
  | None -> false
  | Some spell -> true

(** [can_daze ai_state] determines if the ai can daze *)
let can_daze ai_hand  = 
  traverse_right ai_hand "stunning"

(** [dazed pl_state] returns true if [pl_state] is already dazed. *)
let dazed pl_state = State.get_dazed pl_state > 0

(** [can_prolong_effect ai_state] returns true if [ai_state] has a spell with 
    long-effect; Otherwise, false. *)
let can_prolong_effect ai_hand = 
  traverse_right ai_hand "persistent"


(** [enemy_decision ai_state] returns true if the ai [ai_state] is currently being 
    blocked. *)
let enemy_decision ai_state = 
  if State.get_blocked ai_state > 0 
  then is_full_health ai_state
  else failwith("call function that returns subtree?")

(* let construct_descision_tree pl_spell = 
   check pl_spell *)







(** [spell_finder spells min_threshold max_threshold] takes a non-empty [spells]
    list and returns a tuple of the (minim*maximum) damaging spell in the given 
    thresholds (inclusive). *)
(* let spell_finder (spells:Hogwarts.spell_info list) 
    (min_threshold:int) (max_threshold:int) : 
   ((Hogwarts.spell_info) * ((Hogwarts.spell_info))) = 
   let max_damage = List.fold_left (fun max a -> 
      let damage = Hogwarts.spell_damage a in
      if max < damage && damage <= max_threshold then damage else max) 
      (min_threshold) spells in
   let min_damage = List.fold_left (fun max a -> 
      let damage = Hogwarts.spell_damage a in
      if max > damage && damage >= min_threshold then damage else max) 
      (max_threshold) spells in
   let min_spell =
    try (
      List.find (fun a -> (Hogwarts.spell_damage a) = min_damage) 
        spells 
    ) with Not_found -> (
        List.hd spells
      ) in
   let max_spell = 
    try (
      List.find (fun a -> (Hogwarts.spell_damage a) = max_damage) 
        spells 
    ) with Not_found -> (
        List.hd spells
      ) in (min_spell, max_spell)

   (** [enemy_turn hogwarts enemy player house] takes in the arguments with enemy
    as the caster and performs a basic naive action (attacking with as much 
    damage each time as possible). It returns a tuple where the first argument 
    is the new enemy state and the second, the new player state.*)
   let rec enemy_turn ?skip_draw:(skip_draw=false)(hogwarts:Hogwarts.t)
    (enemy:State.t) (player:State.t) (house:ANSITerminal.style) : 
   (State.t*State.t)=
   (*Quick health check*)
   if (State.get_hp enemy) <= 0 then (enemy,player)
   else (
    (*Either get 7 cards or maximum that is in deck*)
    let enemy_hand = State.to_list_hand enemy in
    let hand_size = (List.length enemy_hand) in
    if hand_size < 7 && not skip_draw then
      let new_hand = State.draw enemy in
      if(List.length (State.to_list_hand new_hand)) = hand_size then
        enemy_turn ~skip_draw:(true) hogwarts new_hand player house
      else 
        enemy_turn hogwarts new_hand player house
    else (
      if (State.get_dazed enemy) > 0 then (
        ANSITerminal.(print_string [house] 
                        "\n\nEnemy is dazed and cannot cast!\n");
        (State.update_dazed enemy, player))
      else (
        if hand_size = 0 then (
          ANSITerminal.(print_string [house] "\n Opponent skips their go");
          ((State.draw enemy),player)
        ) else (
          let hp = State.get_hp enemy in
          let min_max = spell_finder enemy_hand (-100) 100 in
          (*max heal may be the same as min damage if player has no healing
            spells*)
          let max_heal = fst min_max in
          let is_healing = Hogwarts.spell_damage max_heal < 0 in
          let max_damage = snd min_max in
          let min_damage = fst (spell_finder enemy_hand 0 100) in
          (* If a player blocks do not waste resources on them *)
          if State.get_blocked player = 1 then (
            if hp < 100 then (
              if is_healing && 
                 Hogwarts.spell_target max_heal = "self" then (
                let tup = cast_spell max_heal enemy player house in
                ((fst tup),player)
              ) else (
                cast_spell min_damage enemy player house
              )
            ) else (
              cast_spell min_damage enemy player house
            )
          ) else (
            let player_hp = State.get_hp player in (
              (*If can kill - kill. otherwise if player hp > enemy then heal
                otherwise do as much damage as possible*)
              if Hogwarts.spell_damage max_damage > player_hp then (
                cast_spell max_damage enemy player house) else (
                if player_hp > hp then (
                  if is_healing then
                    let tup = cast_spell max_heal enemy player house in
                    ((fst tup),player)
                  else
                    cast_spell max_damage enemy player house
                ) else (
                  cast_spell max_damage enemy player house
                )
              ))
          )
        )
      ))) *)
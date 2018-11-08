open View

module type AI = sig 
  module Printer : Mainview
  val enemy_decision: State.t -> State.t -> State.t * State.t
end

module MakeAI (V:Mainview) : AI = struct

  module Printer = V 

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

  (** [find_best_long_effect_spell lst cmp] is a version of [find_best_spell] 
      but specifically for finding the best long_effect spell in [lst] by using 
      spell [cmp] for comparison. *)
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
      then Some 
          (find_best_long_effect_spell filtered_hand (List.hd filtered_hand))
      else Some (find_best_spell filtered_hand (List.hd filtered_hand) f op)

  (** [has_attack hand ai player] is the cast on [player] dependant on whether 
      if an attack spell is in the [hand] of the [ai]. *)
  let has_attack enemy_hand enemy player =
    let spell = hand_search enemy_hand "attack" Hogwarts.spell_damage (>) in 
    match spell with 
    | None -> Printer.print_cast enemy (List.hd enemy_hand); 
      execute_action (List.hd enemy_hand) enemy player
    | Some attack_spell -> Printer.print_cast enemy attack_spell; 
      execute_action attack_spell enemy player

  (** [has_block hand ai player] is the cast on [player] dependant on whether if 
      a blocking spell is in the [hand] of the [ai]. *)
  let has_block enemy_hand enemy player =
    let filtered_hand = 
      List.filter (fun x -> Hogwarts.spell_type x = "blocking") enemy_hand in
    match filtered_hand with 
    | [] -> Printer.print_cast enemy (List.hd enemy_hand); 
      execute_action (List.hd enemy_hand) enemy player
    | h::t -> Printer.print_cast enemy (List.hd filtered_hand); 
      execute_action (List.hd filtered_hand) enemy player

  (** [has_long_effect hand ai player] is the cast on [player] dependant on 
      whether if a long_effect spell is in the [hand] of the [ai]. *)
  let has_long_effect enemy_hand enemy player =
    let spell = 
      hand_search enemy_hand "persistent" Hogwarts.spell_long_effect (>) in 
    match spell with 
    | None -> has_block enemy_hand enemy player
    | Some persistent_spell -> Printer.print_cast enemy persistent_spell; 
      execute_action persistent_spell enemy player

  (** [has_daze hand ai player] is the cast on [player] dependant on whether if 
      a daze spell is in the [hand] of the [ai]. *)
  let has_daze enemy_hand enemy player =
    let spell = hand_search enemy_hand "stunning" Hogwarts.spell_daze (>) in 
    match spell with 
    | None -> has_long_effect enemy_hand enemy player
    | Some daze_spell -> Printer.print_cast enemy daze_spell; 
      execute_action daze_spell enemy player

  (** [has_healing hand ai player] is the cast on [player] dependant on whether 
      a healing spell is in the [hand] of the [ai]. *)
  let has_healing enemy_hand enemy player condition =
    let spell = hand_search enemy_hand "healing" Hogwarts.spell_damage (<) in 
    match spell with 
    | None -> if condition = "attack" then has_attack enemy_hand enemy player
      else has_daze enemy_hand enemy player
    | Some healing_spell -> Printer.print_cast enemy healing_spell; 
      execute_action healing_spell enemy player

  (** [is_full_health enemy player] is the cast of the [enemy] on 
      [player] dependant on its current health. *)
  let is_full_health enemy player = 
    let enemy_hand = State.get_hand enemy in 
    if (2*(State.get_hp enemy)) > (State.get_full_hp enemy)
    then has_attack enemy_hand enemy player
    else has_healing enemy_hand enemy player "attack"

  (** [is_game_ending enemy player] determines if [enemy] can win right 
      away against [player]. *)
  let is_game_ending enemy player = 
    let enemy_hand = State.get_hand enemy in 
    let best_spell = hand_search enemy_hand "attack" Hogwarts.spell_damage (>) 
    in 
    match best_spell with 
    | None -> has_healing enemy_hand enemy player ""
    | Some spell -> 
      if (Hogwarts.spell_damage spell >= State.get_hp player)
      then (Printer.print_cast enemy spell; 
            execute_action spell enemy player) 
      else is_full_health enemy player

  (** [is_dazed ai pl] determines if [ai] is dazed. If not, makes decision based 
      on possibility of ending game. *) 
  let is_dazed enemy player = 
    if (State.get_dazed enemy) > 0 
    then let updated_enemy = 
           State.update_dazed enemy in (updated_enemy, player) 
    else is_game_ending enemy player

  let rec enemy_decision enemy player = 
    if (State.get_hp enemy) <= 0 then (enemy,player)
    else (
      if List.length (State.to_list_hand enemy) < 7 
      then enemy_decision (State.draw enemy) player
      else
        (
          if State.get_blocked enemy > 0 
          then is_full_health enemy player
          else is_dazed enemy player
        )
    )
end
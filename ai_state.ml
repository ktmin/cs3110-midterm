(** [spell_finder spells min_threshold max_threshold] takes a non-empty [spells]
    list and returns a tuple of the (minim*maximum) damaging spell in the given 
    thresholds (inclusive). *)
let spell_finder (spells:Hogwarts.spell_info list) 
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

let rec enemy_turn ?skip_draw:(skip_draw=false)(hogwarts:Hogwarts.t)
    (enemy:State.t) (player:State.t) (house:ANSITerminal.style)
    (cast_spell:Hogwarts.spell_info -> State.t -> State.t -> (State.t*State.t)) 
  : (State.t*State.t)=
  (*Quick health check*)
  if (State.get_hp enemy) <= 0 then (enemy,player)
  else (
    (*Either get 7 cards or maximum that is in deck*)
    let enemy_hand = State.to_list_hand enemy in
    let hand_size = (List.length enemy_hand) in
    if hand_size < 7 && not skip_draw then
      let new_hand = State.draw enemy in
      if(List.length (State.to_list_hand new_hand)) = hand_size then
        enemy_turn ~skip_draw:(true) hogwarts new_hand player house cast_spell
      else 
        enemy_turn hogwarts new_hand player house cast_spell
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
                let tup = cast_spell max_heal enemy player in
                ((fst tup),player)
              ) else (
                cast_spell min_damage enemy player
              )
            ) else (
              cast_spell min_damage enemy player
            )
          ) else (
            let player_hp = State.get_hp player in (
              (*If can kill - kill. otherwise if player hp > enemy then heal
                otherwise do as much damage as possible*)
              if Hogwarts.spell_damage max_damage > player_hp then (
                cast_spell max_damage enemy player) else (
                if player_hp > hp then (
                  if is_healing then
                    let tup = cast_spell max_heal enemy player in
                    ((fst tup),player)
                  else
                    cast_spell max_damage enemy player
                ) else (
                  cast_spell max_damage enemy player
                )
              ))
          )
        )
      )))
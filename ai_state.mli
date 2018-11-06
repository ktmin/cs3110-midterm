(** [enemy_turn hogwarts enemy player house cast_spell] takes in the arguments 
    with enemy as the caster and performs a basic naive action (attacking with 
    as much damage each time as possible). It returns a tuple where the first 
    argument is the new enemy state and the second, the new player state.
    The casting method used is [cast_spell] to cast the spell from 
    enemy to player.*)
val enemy_turn : ?skip_draw:bool -> Hogwarts.t -> State.t -> State.t -> 
  ANSITerminal.style ->
  (Hogwarts.spell_info -> State.t -> State.t -> (State.t*State.t)) -> 
  (State.t*State.t)
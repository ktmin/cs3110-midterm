type end_state = Win | Loss | Continue

(* module type Model = sig
   val run_cmd : State.t -> State.t -> unit
   val cast_spell : Hogwarts.spell_info -> State.t -> State.t -> unit
   val check_conditions : State.t -> State.t -> end_state
   val affirmation : Hogwarts.t -> Hogwarts.character_info -> bool
   end *)
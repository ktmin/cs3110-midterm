(** Makes ai decision.*)

(** [enemy_decision ai_state pl_state] is the new state of [pl_state] after the 
    ai makes a decision based on [ai_state]. *)
val enemy_decision: State.t -> State.t -> State.t * State.t
(** Makes ai decision.*)

open View

(** [AI] is the signature for the AI *)
module type AI = sig 
  module Printer : Mainview

  (** [enemy_decision ai_state pl_state] is the new state of [pl_state] after the 
      ai makes a decision based on [ai_state]. *)
  val enemy_decision: State.t -> State.t -> State.t * State.t
end

(** [MakeAI V] uses [View.View] as a printer in a game scenario and contains
    all ai logic to be used. *)
module MakeAI (V:Mainview) : AI
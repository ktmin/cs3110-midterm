(** Makes ai decision.*)

open View

module type AI = sig 
  module Printer : Mainview
  val enemy_decision: State.t -> State.t -> State.t * State.t
end

(** [MakeGame V] uses [View.View] as a printer in a game scenario and contains
    all game logic to be used. *)
module MakeAI (V:Mainview) : AI
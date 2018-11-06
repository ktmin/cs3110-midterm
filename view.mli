(**The [View] component keeps all methods that are to be output to the screen*)
module type View = sig
  (** [print_state caster target house] prints all of the status info on the 
      [caster] in [house] style. *)
  val print_state : State.t -> unit

  (** [list_cards spells house] prints the names of all [spells] in the colour
      of [house]*)
  val list_cards : State.t -> unit
  val print_enemy : Hogwarts.character_info -> State.t -> unit
  val print_post_condition : string -> Model.end_state -> unit
  val print_cast : State.t  -> Hogwarts.spell_info -> unit
  val print_spell_details : string -> Hogwarts.t -> Hogwarts.spell_info -> unit
  val print_title : unit -> unit
  val print_cmd_input : string -> string -> unit
  val print_validity : string -> bool -> string -> string -> unit
  val print_formatted_lst : string -> string list -> unit
  val print_enemy_lst : Hogwarts.t -> State.t -> unit
end
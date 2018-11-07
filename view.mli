(** This contains the module and implementation for the View component
    of the MVC*)

(**The [Mainview] component keeps all methods that are to be output to the 
   screen*)
module type Mainview = sig
  (** [print_clear len] prints out [len] number of newlines on screen. *)
  val print_clear : int -> unit

  (** [print_state caster blocking] prints all of the status info on the 
      [caster] in their house style. [blocking] is if the caster has an active
      blocking spell. *)
  val print_state : State.t -> bool -> unit

  (** [list_cards caster] prints the names of all [spells] in [caster]'s 
      hand in the colour of their respective house. *)
  val list_cards : State.t -> unit

  (** [print_enemy enemy enemy_state] prints a character info card for given 
      enemy. *)
  val print_enemy : Hogwarts.character_info -> State.t -> unit

  (** [print_post_condition house_name condition] prints an applicable message 
      to the respective condition (Win/Loss) and nothing for continue.
      Positive condition is a win, negative is a loss and 0 means 
      to continue. *)
  val print_post_condition : string -> int -> unit

  (** [print_cast caster spell] prints what [spell] the [caster] has just 
      done. *)
  val print_cast : State.t  -> Hogwarts.spell_info -> unit

  (** [print_spell_details house_name hogwarts spell] prints an ASCII card for a
      given [spell] in the style of a player's house. *)
  val print_spell_details : string -> Hogwarts.t -> string -> unit

  (** [print_title ()] prints the ASCII art title of the game*)
  val print_title : unit -> unit

  (** [print_cmd_input house_name input] prints out [input] 
      on the the line that requires player input in [house] colour. *)
  val print_cmd_input : string -> string -> unit

  (** [print_formatted_lst house_name lst] prints a list of text in the styling
      of the provided [house_name]. *)
  val print_formatted_lst : string -> string list -> unit

  (** [print_enemy_lst hogwarts player] prints the list of enemies with 
      influence on styling provided by [player] state. *)
  val print_enemy_lst : Hogwarts.t -> State.t -> unit

  (** [print_house house] prints a house in ASCII art (or nothing if house does
      not match presets.*)
  val print_house : string -> unit

  (** [print_card house width header body] prints an ASCII card of size [width] 
      with a centered [header] and body filling up the main part. This is in 
      colour of [house]. *)
  val print_card : string -> int -> string -> string list -> unit
end

(** [Make] creates a view structure for usage. *)
module Make : Mainview
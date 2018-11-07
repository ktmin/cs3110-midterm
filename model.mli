(** Model contains two logic modules, one for the game and one for the menu.
    This is all for inner game logic to be processed and returned to Controller.
*)

open View

(** [end_state] is the possible state that occurs after each possible turn 
    (player and enemy is 1 turn together). *)
type end_state = Win | Loss | Continue

(** [GameModel] is the model run when in battle *)
module type GameModel = sig
  (** The main instance of [Mainview] used to print out all game actions *)
  module Printer: Mainview

  (** [run_cmd hogwarts cmd player enemy] runs a given command and returns
      a tuple of updated states in the order of player, then enemy. *)
  val run_cmd : Hogwarts.t -> Command.command -> State.t -> State.t -> 
    (State.t*State.t)

  (** [check_conditions player enemy] returns the end_state based on player and
      enemy health. If both are at 0 or below then player wins by default. *)
  val check_conditions : State.t -> State.t -> end_state

  (** [cast_spell spell caster target] casts a spell from caster to target and 
      returns the renewed state of both in a tuple in respective order.
      Note: for self spells the target should still be the enemy, but for 
      updated values of caster take the first value of the tuple. *)
  val cast_spell : Hogwarts.spell_info -> State.t -> State.t -> 
    (State.t*State.t)
end

(** [MenuModel] is the model run when in character menu *)
module type MenuModel = sig
  (** [affirmation progress hogwarts enemy] generates a starting state for the 
      [enemy] provided if progress is set to true. Otherwise returns None. *)
  val affirmation : bool -> Hogwarts.t -> Hogwarts.character_info 
    -> State.t option

  (** [play_init spells_file chars_file] takes in a spells and characters file
      and attempts to create a Gamemode with it. Throws exception if files are 
      not found. *)
  val play_init : string -> string -> Hogwarts.t

  (** [choose_opponent hogwarts enemy_name] tries to get a an enemy of input
      name. Returns none if such named character is not found.
      Precondition: all names are stored as camelcase ASCII so any names are
      parsed in and formatted this way (case insensitive input). *)
  val choose_opponent : Hogwarts.t -> string -> Hogwarts.character_info option

  (** [create_player hogwarts name house] creates a starting player state based
      on the input [name] and [house]. *)
  val create_player : Hogwarts.t -> string -> string -> State.t
end

(** [MakeMenu] creates a menumodel for usage in menu. *)
module MakeMenu : MenuModel

(** [MakeGame V] uses [View.View] as a printer in a game scenario and contains
    all game logic to be used. *)
module MakeGame (V:Mainview) : GameModel
(** 
   Representation of player state.
   This module represents the player state of the card game 
   as it is being played,
   including the player's hp, 
   deck, hand, and functions that 
   cause the state to change.
*)

(**represents player state including
   the player itself, hand, and deck*)
type t

(**represents player name*)
type player_name = string

(**represents player*)
type player

type hand

type deck

(** update hand and deck
    after draw*)
val draw: t -> t

(** Given type t return 
    hand*)
val get_hand: t -> hand

(** Given type t return deck*)
val get_deck: t -> deck

(**list representation of spells*)
val to_list_hand: t -> Hogwarts.spell_info list

(** Given hogwarts state and player name
    outputs type t*)
val init_player: Hogwarts.t -> string -> t

(** Given hogwarts state outputs
    outputs type t*)
val init_enemy: Hogwarts.t -> t

(** Given type t, returns
    hp*)
val get_hp: t -> int

(** Given Hogwarts state, spell info, and
    type t, returns an updated verison of type t*)
val update : Hogwarts.t -> Hogwarts.spell_name -> t -> t

(** Given Hogwarts state, spell info, and
    type t, returns an updated verison of type t*)
val cast : Hogwarts.t -> Hogwarts.spell_info -> t -> t -> t



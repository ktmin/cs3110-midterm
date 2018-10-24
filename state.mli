(** 
   Representation of dynamic monopoly state.
   This module represents the state of a monopoly game as it is being played,
   including the player's current positon, current balance, and functions that 
   cause the state to change.
*)

type t

type player_name = string

type player

type hand

type deck

val draw: t -> t

val get_hand: t -> hand

val get_deck: t -> deck

val to_list_hand: t -> Hogwarts.spell_info list

val init_player: Hogwarts.t -> string -> t

val init_enemy: Hogwarts.t -> t

val get_hp: t -> int

val cast : Hogwarts.t -> Hogwarts.spell_name -> t -> 
  Hogwarts.damage * Hogwarts.spell_info list

(*val casted : Hogwarts.t -> Hogwarts.spell_name -> player -> player*)

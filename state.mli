(** 
   Representation of dynamic monopoly state.
   This module represents the state of a monopoly game as it is being played,
   including the player's current positon, current balance, and functions that 
   cause the state to change.
*)

type player_name = string

type player_hp = int

type player

type hand

type deck

val get_name: player -> player_name

val get_hp: player -> player_hp 

val get_hand: hand -> Hogwarts.spell_info list

val draw: Hogwarts.spell_info list ->
  hand -> deck -> Hogwarts.spell_info list * Hogwarts.spell_info list

val cast : Hogwarts.t -> Hogwarts.spell_name -> hand -> 
  Hogwarts.damage * Hogwarts.spell_info list

val casted : Hogwarts.t -> Hogwarts.spell_name -> player -> player
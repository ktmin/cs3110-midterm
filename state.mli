(** 
   Representation of player state.
   This module represents the player state of the card game 
   as it is being played,
   including the player's hp, 
   deck, hand, and functions that 
   cause the state to change.
*)

(** The abstract type of values representing a state. *)
type t

(**represents player name*)
type player_name = string

(**represents player*)
type player

(** The type of hand *)
type hand

(** The type of deck *)
type deck

(** [draw t] updates hand and deck of statue [t] *)
val draw: t -> t

(** [get_hand t] is the hand of a state [t] *)
val get_hand: t -> hand

(** [get_deck t] is the deck of a state [t] *)
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
    type t, returns an updated verison of type t that reduces/increases the 
    health of the target user*)
val update_damage : t -> Hogwarts.spell_info -> t

(** Given Hogwarts state, spell info, and
    type t, returns an updated verison of type t that removes the 
    spell card from the target user*)
val update_caster : t -> Hogwarts.spell_info -> t

(** Given Hogwarts state, spell info, and
    type t, returns an updated verison of type t*t. The first being the caster,
    second the target. If the spell is self targeted then the tuple will just
    be the same record twice*)
val cast : Hogwarts.spell_info -> t -> t -> t*t

val get_name : t -> player_name

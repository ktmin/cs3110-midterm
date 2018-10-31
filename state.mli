(** 
   Representation of player state.
   This module represents the player state of the card game 
   as it is being played,
   including the player's hp, 
   deck, hand, and functions that 
   cause the state to change.
*)

(** The abstract type of values representing a player
    state. *)
type t

(** [get_name t] is the name of 
    player in [t] *)
val get_name: t -> string

(** [get_hp t] is the hp of 
    player in [t] *)
val get_hp: t -> int

(** [get_level t] is the level of 
    player in [t] *)
val get_level: t -> int


(** [get_dazed t] is the number of
    turns player is dazed in [t] *)
val get_dazed: t -> int


(** [get_hand t] is the hand of a state [t] *)
val get_hand: t -> Hogwarts.spell_info list 

(** [get_deck t] is the deck of a state [t] *)
val get_deck: t -> Hogwarts.spell_info list 

(** [get_prolong_damage t] is the list of
    prolong damage that will be applied*)
val get_prolong_damage: t -> int list

(** [get_prolong_turn t] is the list of turns
    of prolonged damage *)
val get_prolong_turn: t -> int list 

(** [get_blocked t] is the representation of 
    how many times a player is blocked *)
val get_blocked: t -> int 

(** [get_level_deck t] is the 
    updated deck of a state [t], 
    filtered out by removing 
    cards that have greater 
    level than the level of player in [t]*)
val get_level_deck: t -> t

(** [init_player Hogwarts.t name] in the initial 
    state of [t] of player *)
val init_player: Hogwarts.t -> string -> t

(** [init_player_with_level_deck Hogwarts.t name] is a player initialized with a 
    leveled deck. *)
val init_player_with_level_deck : Hogwarts.t -> string -> t

(** [init_enemy Hogwarts.t name] in the initial 
    state of [t] of enemy *)
val init_enemy: Hogwarts.t -> string -> t

(** [init_enemy_with_level_deck Hogwarts.t name] is an enemy initialized with a 
    leveled deck. *)
val init_enemy_with_level_deck : Hogwarts.t -> string -> t

(**[draw t] updates the deck of a state [t]
   after draw*)
val draw: t -> t

(** [update_damage t] updates the 
    hp of a state [t] after cast *)
val update_damage : t -> Hogwarts.spell_info -> t

(** [update_caster t] updates the 
    deck and hand of a state [t] after cast*)
val update_caster : t -> Hogwarts.spell_info -> t


(** [update_prolong_list t] returns 
    the list of prolonged damage after
    one turn*)
val update_prolong_list : t -> (int * int) list

(**[update_prolong Hogwarts.spell_info] returns 
   the list of prolonged damaged after one turn*)
val update_prolong: Hogwarts.spell_info -> t ->
  (Hogwarts.damage * Hogwarts.turns) list



(** [drop n] is a helper function
    to remove n number of elements from a given 
    list*)
val drop : int -> 'a list -> 'a list 

(** [update spell st1 st2] updates a state [t]
    of player and a state [t] of enemy according to
    the casted spell *)
val update : Hogwarts.spell_info -> t -> t -> t  


(** [hand_after_cast t] updates state [t]
    after state[t] casts a spell *)
val hand_after_cast: Hogwarts.spell_info -> t -> t


(** [cast Hogwarts.spell_info] 
    returns an updated verison of type t*t.
    The first being the caster,
    second the target.*)
val cast: Hogwarts.spell_info -> t -> t -> t*t 

(**list representation of spells*)
val to_list_hand: t -> Hogwarts.spell_info list

(**[refresh_deck Hogwarts.t t] refreshes the deck
   of a state [t]*)
val refresh_deck: Hogwarts.t -> t -> t

(** [get_defeated_enemies st] is the list of defeated enemies. *)
val get_defeated_enemies: t -> Hogwarts.character_name list

(** [add_defeated_enemy st enemy hogwarts] is an updated state with defeated 
    enemies. *)
val add_defeated_enemy: t -> Hogwarts.character_name -> Hogwarts.t -> t

(** [required_wins t] returns amount of wins required to 
    progress to next level. *)
val required_wins: t -> int

(** [level_up t] attempts to level up player. Returns t if not enough wins
    have been secured. *)
val level_up: t -> t

(** [get_rpolong_tupes t] gets all prolonged effects of input caster*)
val get_prolong_tupes: t -> (int*int) list

(** [update_dazed st] is a helper for update for if dazed 
    is true. *)
val update_dazed: t -> t

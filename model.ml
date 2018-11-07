open Ai_controller
open View

type end_state = Win | Loss | Continue

module type GameModel = sig
  module Printer : Mainview
  module Enemy_Logic : AI
  val run_cmd : Hogwarts.t -> Command.command -> State.t -> State.t -> 
    (State.t*State.t)
  val check_conditions : State.t -> State.t -> end_state
  val cast_spell : Hogwarts.spell_info -> State.t -> State.t -> 
    (State.t*State.t)
end

module type MenuModel = sig
  val affirmation : bool -> Hogwarts.t -> Hogwarts.character_info 
    -> State.t option
  val play_init : string -> string -> Hogwarts.t
  val choose_opponent : Hogwarts.t -> string -> Hogwarts.character_info option
  val create_player : Hogwarts.t -> string -> string -> State.t
end

module MakeMenu : MenuModel = struct
  (** Finds opponent specified by name. For more details go to 
                    {{: Model.MenuModel.html#VALchoose_opponent} 
                    Model.MenuModel.choose_opponent}. *)
  let choose_opponent (hogwarts:Hogwarts.t) (enemy_name:string) : 
    Hogwarts.character_info option =
    let target_name_lst = String.split_on_char ' ' enemy_name in
    let target_name = String.concat " " (List.map (fun str -> 
        String.capitalize_ascii (String.lowercase_ascii str)) target_name_lst) 
    in try (
      let enemy = Hogwarts.search_characters hogwarts target_name in
      Some enemy
    ) with Hogwarts.UnknownCharacter target_name -> (
        None
      )
  (** Asserts that player wants to face set enemy. For more details go to 
                      {{: Model.MenuModel.html#VALaffirmation} 
                      Model.MenuModel.affirmation}. *)
  let affirmation (progress:bool) (hogwarts:Hogwarts.t) 
      (enemy:Hogwarts.character_info) : State.t option =
    if progress then
      let enemy_state = (State.init_enemy_with_level_deck hogwarts 
                           (Hogwarts.character_name enemy) 
                           (Hogwarts.character_house enemy)) in
      Some enemy_state
    else
      None

  (** Initializes json play files. For more details go to 
                        {{: Model.MenuModel.html#VALplay_init} 
                        Model.MenuModel.play_init}. *)
  let play_init (spells_file:string) (char_file:string) : Hogwarts.t =
    let spells = Yojson.Basic.from_file spells_file in
    let chars = Yojson.Basic.from_file char_file in
    Hogwarts.from_json spells chars

  (** Creates player state from parameters. For more details go to 
                          {{: Model.MenuModel.html#VALcreate_player} 
                          Model.MenuModel.create_player}. *)
  let create_player (hogwarts:Hogwarts.t) (name:string) (house:string) : 
    State.t = 
    State.init_player_with_level_deck hogwarts name house
end

module MakeGame (V:Mainview) (A:AI) : GameModel = struct
  (** [Mainview] module used for printing events. *)
  module Printer = V

  (** The enemy logic that is used for the enemy turn. *)
  module Enemy_Logic = A

  (** Checks win/loss conditions. For more details go to 
                {{: Model.GameModel.html#VALcheck_conditions} 
                Model.GameModel.check_conditions}. *)
  let check_conditions (player:State.t) (enemy:State.t) : end_state =
    if State.get_hp enemy <= 0 then
      Win
    else if State.get_hp player <= 0 then
      Loss
    else
      Continue

  (** Casts spell. For more details go to 
                  {{: Model.GameModel.html#VALcast_spell} 
                  Model.GameModel.cast_spell}. *)
  let cast_spell (spell:Hogwarts.spell_info) (caster:State.t) (target:State.t) :
    (State.t*State.t) =
    Printer.print_cast caster spell;
    State.cast spell caster target

  (** Casts spell. For more details go to 
                  {{: Model.GameModel.html#VALrun_cmd} 
                  Model.GameModel.run_cmd}. *)
  let run_cmd (hogwarts:Hogwarts.t) (cmd:Command.command) (player:State.t) 
      (enemy:State.t) : (State.t*State.t) =
    let p_house = State.get_house player in
    match cmd with
    | Forfeit -> (
        Printer.print_formatted_lst p_house 
          ["Turns out you weren't so tough and brave..."];
        exit 0
      )
    | Status -> (
        Printer.print_state player;
        Printer.print_state enemy;
        (player,enemy)
      )
    | View -> (
        Printer.print_formatted_lst p_house 
          ["The following spells can be cast: \n"];
        Printer.list_cards player;
        (player, enemy)
      )
    | Instruction  -> (
        Printer.print_formatted_lst p_house 
          ["Rules are simple:";
           "- You have a deck and spell cards that attack the opponent or heal you";
           "- Each turn you can just cast or draw and cast";
           "- You play against an AI that takes its turn after you";
           "- Winner is the person who makes the other reach 0 or less health"];
        (player, enemy)
      )
    | Describe lst -> (
        let sp_name = String.concat " " lst in (
          try (
            (*Note to self: the str->spell->str is to make the exception happen
              here rather than printer. Experiment later*)
            Printer.print_spell_details p_house hogwarts sp_name
          ) with Hogwarts.UnknownSpell sp_name -> (
              Printer.print_formatted_lst p_house 
                [(sp_name ^ " incorrectly spelled. Try again\n")]
            ));
        (player,enemy)
      )
    | Draw -> (
        let can_get = List.length (State.to_list_hand player) < 5 in
        if can_get then (
          let new_hand = State.draw player in
          let new_card =  List.hd(State.to_list_hand new_hand) in (
            Printer.print_formatted_lst p_house 
              [("You drew "^(Hogwarts.spell_name new_card))];
            (new_hand,enemy))
        ) else (
          Printer.print_formatted_lst p_house ["You already have 5 spells...";
                                               "Don't get greedy"];
          (player,enemy)
        )
      )
    | Help -> (
        Printer.print_formatted_lst p_house [
          "Possible Commands:";
          "- Cast [card name] -> casts the card";
          "- Describe [card name] -> gives a description of the card";
          "- Draw -> draws a spell from your deck";
          "- Forfeit -> give up and quit the game";
          "- Help -> displays a list of possible commands";
          "- Instruction -> explains how spell duels work";
          "- View -> views all spells you can currently cast";
          "- Status -> displays the status of both you and the enemy"
        ];
        (player,enemy)
      )
    | Cast lst -> (
        let sp_name = String.concat " " lst in (
          try (
            let spell = Hogwarts.search_spells hogwarts sp_name in
            let updated = cast_spell spell player enemy in 
            let enemy_updated = (
              if(Hogwarts.spell_target spell) = "self" 
              then 
                enemy
              else 
                (snd updated)
            ) in let en,pl = Enemy_Logic.enemy_decision enemy_updated 
                     (fst updated) in
            (pl,en)
          ) with Hogwarts.UnknownSpell sp_name -> (
              Printer.print_formatted_lst p_house 
                [(sp_name ^ " incorrectly spelled. Try again\n")];
              (player,enemy)
            ))
      )
end
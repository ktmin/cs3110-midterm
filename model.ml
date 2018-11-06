type end_state = Win | Loss | Continue

module type GameModel = sig
  val run_cmd : Hogwarts.t -> Command.command -> State.t -> State.t -> 
    (State.t*State.t)
  val check_conditions : State.t -> State.t -> end_state
  val cast_spell : Hogwarts.spell_info -> State.t -> State.t -> 
    (State.t*State.t)
    (*val affirmation : Hogwarts.t -> Hogwarts.character_info -> bool*)
end

module type MenuModel = sig
end

module Make (V: View.View) : GameModel = struct
  module Printer = V

  let check_conditions (player:State.t) (enemy:State.t) : end_state =
    if State.get_hp enemy <= 0 then
      Win
    else if State.get_hp player <= 0 then
      Loss
    else
      Continue

  let cast_spell (spell:Hogwarts.spell_info) (caster:State.t) (target:State.t) :
    (State.t*State.t) =
    Printer.print_cast caster spell;
    State.cast spell caster target

  (*TODO: this one is weird that it needs to have msgs inside it*)
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
            let spell = Hogwarts.search_spells hogwarts sp_name in
            Printer.print_spell_details p_house hogwarts spell
          ) with Hogwarts.UnknownSpell sp_name -> (
              Printer.print_formatted_lst p_house 
                [(sp_name ^ " incorrectly spelled. Try again\n")]
            ));
        (player,enemy)
      )
    | Draw -> (
        let can_get = List.length (State.to_list_hand player) >= 5 in
        if can_get then (
          let new_hand = State.draw player in
          let new_card =  List.hd(State.to_list_hand new_hand) in (
            Printer.print_formatted_lst p_house 
              [("You drew "^(Hogwarts.spell_name new_card))];
            (new_hand,enemy))
        ) else (
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
            (*Note to self: the str->spell->str is to make the exception happen
              here rather than printer. Experiment later*)
            let spell = Hogwarts.search_spells hogwarts sp_name in
            let updated = cast_spell spell player enemy in 
            let enemy_updated = (
              if(Hogwarts.spell_target spell) = "self" 
              then 
                enemy
              else 
                (snd updated)
                (*TODO: this will change with new AI*)
            ) in Ai_state.enemy_turn hogwarts enemy_updated (fst updated) 
              ANSITerminal.red cast_spell
          ) with Hogwarts.UnknownSpell sp_name -> (
              Printer.print_formatted_lst p_house 
                [(sp_name ^ " incorrectly spelled. Try again\n")];
              (player,enemy)
            ))
      )
end
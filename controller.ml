(** Raised if [string] in parse_input fails predicate test. *)
exception InvalidInput of string

(** Menu model for game menu logic. *)
module Menu = Model.MakeMenu
(** View is for displaying text to the user. *)
module View = View.Make
(** The AI Logic in use of the games. *)
module Ai = Ai_controller.MakeAI (View)
(** Game model for game logic. *)
module Game = Model.MakeGame (View) (Ai)

(** Simple var to hold command to quit game at any point. *)
let quit_cmd = "quit"

(**[default_condition str] is the default predicate for input parsing and 
   returns true no matter what is provided for [str]. *)
let default_condition (str:string) : bool =
  true

(**[parse_input] takes in an optional predicate [pred] and returns the 
   input string from the user if it passes the predicate (by default all strings 
   are valid). If predicate returns false then [InvalidInput] exception is 
   raised.*)
let parse_input ?pred:(pred=default_condition) (_:unit) : string =
  let input = read_line () in
  if input = quit_cmd then (
    View.print_formatted_lst "" ["Thank you for playing and goodbye"];
    exit 0;
  ) else if pred input then
    input
  else
    raise (InvalidInput input)

(** [battle skip_states hogwarts player enemy] runs through the game loop for 
    [player] and [enemy] until a win or loss condition is achieved. In the case 
    of win, the player is returned to choose another opponent, but loss will end 
    the game entirely. *)
let rec battle (skip_states:bool) (hogwarts:Hogwarts.t) 
    (player:State.t) (enemy:State.t) : State.t =
  let p_house = State.get_house player in
  match Game.check_conditions player enemy with
  | Win -> (
      View.print_post_condition p_house 1;
      let force = (State.get_level enemy)-2 >= (State.get_level player) in
      (State.level_up ~force:(force)
         (State.add_defeated_enemy player (State.get_name enemy) 
            hogwarts))
    )
  | Loss -> (
      View.print_post_condition p_house (-1);
      exit 0;
    )
  | Continue -> (
      if not skip_states then (
        View.print_state player ((State.get_blocked enemy) = 1);
        View.print_state enemy ((State.get_blocked player) = 1));

      View.print_cmd_input p_house "Enter command";
      try (
        let cmd = Command.parse (parse_input ()) in
        let new_states = Game.run_cmd hogwarts cmd player enemy in
        battle (cmd = Command.Status) hogwarts (fst new_states) (snd new_states)
      ) with Command.Invalidcommand -> (
          View.print_formatted_lst p_house 
            ["not a command buddy.";"Type in help to get a list of commands"];
          battle false hogwarts player enemy
        )
    )

(** [play_game hogwarts player] is the main game loop that initiates
    battle, and keeps track of player inputs for choosing opponent. Will keep
    recurring until a player either dies in battle or manually quits.*)
let rec play_game (hogwarts:Hogwarts.t) (player:State.t) : unit =
  let defeated_len = List.length (State.get_defeated_enemies player) in 
  let enemy_len = List.length (Hogwarts.get_characters hogwarts) in
  if defeated_len = enemy_len then
    View.print_formatted_lst (State.get_house player) 
      ["\nA winner is you!";
       "You have wasted enough time to defeat all enemies";
       "Now go. Shoo. You have homework to do."]
  else
    let p_house = State.get_house player in (
      View.print_formatted_lst p_house 
        [("Hey, "^(State.get_name player)); 
         ("Current Level: "^(string_of_int (State.get_level player)))];
      View.print_house p_house;
      View.print_enemy_lst hogwarts player;
      View.print_cmd_input p_house "Enter enemy choice";

      match Menu.choose_opponent hogwarts (parse_input ()) with
      | None -> (
          View.print_formatted_lst p_house 
            ["That wasn't on the list...";
             "10/10 for effort, -5/7 for execution. Try again"];
          play_game hogwarts player
        )
      | Some enemy -> (
          View.print_clear 50;
          View.print_enemy enemy (State.init_enemy_with_level_deck hogwarts 
                                    (Hogwarts.character_name enemy) 
                                    (Hogwarts.character_house enemy));
          View.print_cmd_input p_house "Enter Yes to confirm enemy";
          let yes_no = String.uppercase_ascii (parse_input ()) in
          let confirm = Menu.affirmation (yes_no = "Y" || yes_no = "YES") 
              hogwarts enemy in
          View.print_clear 50;
          match confirm with
          | None -> play_game hogwarts player
          | Some enemy_state -> (
              View.print_formatted_lst p_house 
                [("You are battling with "^(Hogwarts.character_name enemy))];
              let new_state = battle false hogwarts player enemy_state in
              play_game hogwarts new_state
            )
        )
    )

(** [get_name] returns the name of a person from terminal input.
    The predicate is that the name must be alphabetical (with spaces allowed) 
    only.*)
let get_name _ : string =
  View.print_formatted_lst "" 
    ["\n\nWelcome! Welcome!";
     "I lost my list of names... So remind me who are you? "];
  let reg = Str.regexp "^[A-Za-z]+[A-Za-z ]*$" in 
  let rec parse_name _ = 
    View.print_cmd_input "" "Enter your name";
    try (
      parse_input ~pred:(fun s -> Str.string_match reg s 0) ()
    ) with InvalidInput input -> (
        View.print_formatted_lst "" 
          ["Simple stuff,";
           "I wonder how you will fare in Hogwarts if you struggle at even 
           this...";
           "Try again"];
        parse_name ()
      ) in parse_name ()

(** [get_house] returns the house of a person from terminal input.
    The predicate is that the house must be one of four canon homes:
    Gryffindor, Slytherin, Ravenclaw, Hufflepuff. It is case insensitive. *)
let get_house _ : string =
  View.print_formatted_lst "" ["\n\nAlso the sorting hat is out for lunch";
                               "So you'll need to choose your own house.";
                               "As a reminder the houses are: "];
  View.print_formatted_lst "gryffindor" ["Gryffindor"];
  View.print_formatted_lst "slytherin" ["Slytherin"];
  View.print_formatted_lst "ravenclaw" ["Ravenclaw"];
  View.print_formatted_lst "hufflepuff" ["Hufflepuff"];
  let rec choose_house _ =
    View.print_cmd_input "" "Enter your house choice";
    try (
      parse_input ~pred:(fun s -> match (String.lowercase_ascii s) with
          | "gryffindor" | "slytherin" | "ravenclaw" | "hufflepuff" -> true
          | _ -> false) ()
    ) with InvalidInput input -> (
        View.print_formatted_lst "" 
          ["Not a house..."];
        choose_house ()
      ) in choose_house ()

(** [get_files] returns the player input of spell and character JSON files in a 
    tuple of (spells,charaacters) *)
let get_files (_:unit) : (string*string)=
  View.print_cmd_input "" "Enter name of spells file";
  let spells = parse_input () in
  View.print_cmd_input "" "Enter name of characters file";
  let chars = parse_input () in
  (spells,chars)

(** [init_game] is the entry point into the game. *)
let rec init_game ?skip_clear:(skip_clear=false) (_:unit) =
  if not skip_clear then View.print_clear 50;
  View.print_title ();
  View.print_formatted_lst "" ["\nInput Quit at any point to exit game"];
  View.print_formatted_lst "" 
    ["\nRules of the game are simple:";
     "- You have a deck and spell cards that attack the opponent or heal you";
     "- Each turn you can just cast or draw and cast";
     "- You play against an AI that takes its turn after you";
     "- Winner is the person who makes the other reach 0 or less health"];
  let files = get_files () in
  try (
    let hogwarts = Menu.play_init (fst files) (snd files) in
    View.print_clear 50;
    let name = get_name ()  in
    View.print_clear 50;
    let house = get_house () in
    let player = Menu.create_player hogwarts name house in (
      View.print_clear 50;
      play_game hogwarts player
    )
  ) with _ -> (
      View.print_clear 50;
      View.print_formatted_lst "gryffindor" ["Files were invalid. Try again."];
      init_game ~skip_clear:(true) ()
    )

(**/**)
let () = init_game ()
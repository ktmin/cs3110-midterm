type end_state = Win | Loss | Continue

(** [enemy_turn hogwarts enemy player house] takes in the arguments with enemy
    as the caster and performs a basic naive action (attacking with as much 
    damage each time as possible). It returns a tuple where the first argument 
    is the new enemy state and the second, the new player state.*)
let rec enemy_turn (hogwarts:Hogwarts.t)(enemy:State.t) 
    (player:State.t) (house:ANSITerminal.style) : (State.t*State.t)=
  (*Quick health check*)
  if (State.get_hp enemy) <= 0 then (enemy,player)
  else (
    let enemy_hand = State.to_list_hand enemy in
    if (List.length enemy_hand) < 7 then
      enemy_turn hogwarts (State.draw enemy) player house
    else (
      (*What this does is either tries to kill the player if one spell exists
        or does as much damage as possible. For milestone one I am not letting it
        use self spells because it will glitch the game out because of how state
        currently updates*)
      let killers = List.filter (fun a -> 
          (Hogwarts.spell_damage a) > State.get_hp player) enemy_hand in
      match killers with
      | h::_ -> (
          ANSITerminal.(print_string [house] 
                          ("\nEnemy casts "^(Hogwarts.spell_name h)));
          State.cast h enemy player)
      | [] -> (
          let max_damage = List.fold_left (fun max a -> 
              let damage = Hogwarts.spell_damage a in
              if max < damage then damage else max) 0 enemy_hand in 
          let target_spell = 
            List.find (fun a -> (Hogwarts.spell_damage a) = max_damage) 
              enemy_hand 
          in (
            if Hogwarts.spell_target target_spell = "self" then (
              ANSITerminal.(print_string [house] "\n Opponent skips their go");
              ((State.draw enemy),player))
            else (
              ANSITerminal.(print_string [house] 
                              ("\nEnemy casts "^
                               (Hogwarts.spell_name target_spell)));
              State.cast target_spell enemy player))
        )
    ))

(** [check_conditions player enemy] checks to see whether [player] or [enemy]
    is below 0 health and if so, produces a Loss or Win state respictively if 
    one has less than required health. Otherwise Continue is produced.
    If both are under 0 health then by default the method will currently award 
    the player the victory.*)
let check_conditions (player:State.t) (enemy:State.t) : end_state =
  if State.get_hp enemy <= 0 then
    Win
  else if State.get_hp player <= 0 then
    Loss
  else
    Continue

(** [list_cards spells house] prints the names of all [spells] in the colour
    of [house]*)
let rec list_cards (spells:Hogwarts.spell_info list) (house:ANSITerminal.style)=
  match spells with
  | [] -> (
      ANSITerminal.(print_string [house] "\n\nYeah... You have no spells")
    )
  | h::[] -> (
      ANSITerminal.(print_string 
                      [magenta] 
                      ((Hogwarts.spell_name h ^ 
                        "\n\nEnter: Describe spell_name to see spell 
                        description."))))
  | h::t -> (
      ANSITerminal.(print_string [magenta] ((Hogwarts.spell_name h) ^ ", "));
      list_cards t house
    )

(** [run_command player enemy house hogwarts cmd callback] runs a valid [cmd] 
    and mitigates possible errors that are thrown through Hogwarts.UnknownSpell.
    Post command, the callback function is called and takes in [player] [enemy]
    [house] [hogwarts] in that respective order.*)
let rec run_command (player:State.t) (enemy:State.t)
    (house:ANSITerminal.style) (hogwarts:Hogwarts.t) (cmd:Command.command) 
    (callback:State.t -> State.t -> ANSITerminal.style -> Hogwarts.t -> unit) 
  : unit =
  match cmd with
  | Draw -> (
      if List.length (State.to_list_hand player) >= 5 then (
        ANSITerminal.(print_string 
                        [house] 
                        ("Don't get greedy. 
                        You already have at least 5 cards"));
        callback player enemy house hogwarts
      ) else (
        let drawn = State.draw player in
        let chosen_card = List.hd(State.to_list_hand drawn) in
        ANSITerminal.(print_string [house] 
                        ("You drew: "^(Hogwarts.spell_name chosen_card)));
        callback drawn enemy house hogwarts))
  | View -> (ANSITerminal.(print_string [house] 
                             "The following spells can be casted: \n");
             list_cards (State.to_list_hand player) house;
             callback player enemy house hogwarts)
  | Help -> (ANSITerminal.(print_string [house] 
                             "Invalid command. Possible commands: \n
    Draw, cast [card name], describe [card name], view, instruction, help, 
    status, forfeit"); 
             callback player enemy house hogwarts)
  | Forfeit -> (ANSITerminal.(
      print_string [house] 
        "Turns out you weren't so tough and brave...\n"); exit 0)
  | Describe lst -> (
      let sp_name = String.concat " " lst in
      try (
        ANSITerminal.(print_string [house] 
                        ("Description for "^sp_name^":\n"
                         ^(Hogwarts.spell_description hogwarts sp_name))
                     );
        callback player enemy house hogwarts
      ) with Hogwarts.UnknownSpell sp_name -> (
          ANSITerminal.(print_string [house] 
                          (sp_name ^ " incorrectly spelled. Try again"));
        );
        callback player enemy house hogwarts
    )
  | Instruction -> (ANSITerminal.(print_string [house] 
                                    "Rules are simple:\n
    - You have a deck and spell cards that attack the opponent or heal you\n
    - Each turn you can just cast or draw and cast\n
    - You play against an AI that takes its turn after you\n
    - Winner is the person who makes the other reach 0 or less health");
                    callback player enemy house hogwarts)
  | Status -> ANSITerminal.(print_string [house] "Your health:\n");
    ANSITerminal.(print_string [magenta] (string_of_int 
                                            (State.get_hp player)));
    ANSITerminal.(print_string [house] "\nEnemy's health:\n");
    ANSITerminal.(print_string [magenta] (string_of_int 
                                            (State.get_hp enemy)));
    callback player enemy house hogwarts
  | Cast lst -> (
      (*TODO: reduce this part into its own method*)
      let sp_name = String.concat " " lst in
      try (
        let info = Hogwarts.search hogwarts sp_name in
        if(List.mem (info) (State.to_list_hand player)) then (
          ANSITerminal.(print_string [house] ("You cast "^sp_name));
          let cast_update = State.cast info player enemy in (
            if(Hogwarts.spell_target info) = "self" then (
              let tup = enemy_turn hogwarts enemy (fst cast_update) house in
              callback (snd tup) (fst tup) house hogwarts
            ) else (
              let tup = enemy_turn hogwarts (snd cast_update) 
                  (fst cast_update) house in
              callback (snd tup) (fst tup) house hogwarts
            ))
        )
        else (
          ANSITerminal.(
            print_string [house] "Nice try but you don't have that spell");
          callback player enemy house hogwarts
        )) with Hogwarts.UnknownSpell sp_name -> (
          ANSITerminal.(print_string [house] 
                          (sp_name ^ " incorrectly spelled. Try again"))
        )
    )

(** [play player enemy house name] is the main game loop that takes in [player]
    and [enemy] states, [house] text color and [name] hogwarts game state and 
    progresses based on player inputs until a win/loss condition is fulfilled.*)
let rec play (player:State.t) (enemy:State.t)
    (house: ANSITerminal.style) (name: Hogwarts.t)  =
  match check_conditions player enemy with
  | Win -> (ANSITerminal.(print_string [house] "\nCongrats you win!\n"); exit 0)
  | Loss -> (ANSITerminal.(print_string [house] "\nYou lose :(\n"); exit 0)
  | Continue -> (
      ANSITerminal.print_string [house] "\n\nEnter an action to perform > ";
      let cmd = read_line () in
      try (
        run_command player enemy house name (Command.parse cmd) play
      ) with Command.Invalidcommand ->
        (ANSITerminal.(print_string [house] 
                         "Invalid command. Possible commands: \n
    Draw, cast [card name], describe [card name], view, instruction, help, 
    status, forfeit"); play player enemy house name ))

(** [choose_house h] returns the colour representing the respective Harry Potter
    house as specified by [h]. If the house name is invalid 
    (name does not exist), None will be returned.*)
let choose_house (h: string): ANSITerminal.style option =
  match String.lowercase_ascii h with
  | "gryffindor" -> Some ANSITerminal.red
  | "slytherin" -> Some ANSITerminal.green
  | "ravenclaw" -> Some ANSITerminal.blue
  | "hufflepuff" -> Some ANSITerminal.yellow
  | _ -> None


let play_init f player house = 
  let json = Yojson.Basic.from_file f in
  let hogwarts = Hogwarts.from_json json in 
  let player_state = State.init_player hogwarts player in
  let enemy_state = State.init_enemy hogwarts "Malfoy" in (
    ANSITerminal.(print_string [house] 
                    ("\nYour opponent is "^
                     (State.get_name enemy_state)^"!\n"));
    play player_state enemy_state house hogwarts)


(** [name house] takes in the ANSITerminal colour [house] and records the 
    player's name to be used in gameplay. 
    The name specified should be regular ASCII alphabet with no numbers 
    or special characters. Method will repeat until a valid input is received.*)
let rec name house =
  ANSITerminal.(
    print_string [magenta] "Now, your name. Let's keep it simple, 
  no numbers or symbols. Just letters. Please.\n\n";
    print_string [magenta] "Enter your name > "
  );

  let reg = Str.regexp "^[A-Za-z]+$" in
  let player_name = read_line () in
  if Str.string_match reg player_name 0 then
    (ANSITerminal.(
        print_string [house] ("Welcome " ^ player_name ^ "!");
      );
     play_init "spells.json" player_name house 
    ) else (ANSITerminal.(
      print_string [magenta] "Simple stuff, 
      I wonder how you will fare in Hogwarts if you struggle at even this... 
      Try again\n"
    ); name house)

(** [house] begins the game by asking the player for their chose Harry Potter
    house. It will progress to name and gameplay if a valid name is provided
    (case-insensitive) or repeat until a valid one is inputted.*)
let rec house () =
  ANSITerminal.(
    print_string [magenta] "\nWelcome to ";
    print_string [red] "Ho"; print_string [green] "gw";
    print_string [yellow] "ar"; print_string [blue] "ts";
    print_string [magenta] 
      "!\nThe sorting hat is at lunch so you'll have to choose your own house.\n
    In case you forgot, the choices are:\n";
    print_string [red] "Gryffindor "; print_string [green] "Slytherin ";
    print_string [yellow] "Hufflepuff "; print_string [blue] "Ravenclaw";
    print_string [magenta] "\n\nEnter house name > ";
  );
  match choose_house (read_line ()) with
  | Some x -> name x
  | None -> ANSITerminal.(print_string [magenta] "... That isn't a house. 
    They are literally in front of you. Let's try this again.\n"); house ()

(**/**)
let () = house ()
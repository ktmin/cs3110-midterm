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
    if (List.length enemy_hand) < 3 then
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
            List.find (fun a -> (Hogwarts.spell_damage a) = max_damage) enemy_hand 
          in (
            if Hogwarts.spell_target target_spell = "self" then 
              ((State.draw enemy),player)
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
      ANSITerminal.(print_string [house] "\n\nEnter: Describe spell_name 
      to see spell description.")
    )
  | h::t -> (
      ANSITerminal.(print_string [magenta] ((Hogwarts.spell_name h) ^ " "));
      list_cards t house
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
        match Command.parse cmd with
        | Draw -> (
            let drawn = State.draw player in
            let chosen_card = List.hd(State.to_list_hand drawn) in
            ANSITerminal.(print_string [house] 
                            ("You drew: "^(Hogwarts.spell_name chosen_card)));
            play drawn enemy house name
          )
        | Cast lst -> (
            let sp_name = String.concat " " lst in
            let info = Hogwarts.search name sp_name in
            if(List.mem (info) (State.to_list_hand player)) then (
              ANSITerminal.(print_string [house] ("You cast "^sp_name));
              let cast_update = State.cast info player enemy in (
                if(Hogwarts.spell_damage info) < 0 then (
                  let tup = enemy_turn name enemy (fst cast_update) house in
                  play (snd tup) (fst tup) house name
                ) else (
                  let tup = enemy_turn name (snd cast_update) 
                      (fst cast_update) house in
                  play (snd tup) (fst tup) house name
                ))
            )
            else (
              ANSITerminal.(
                print_string [house] "Nice try but you don't have that spell");
              play player enemy house name
            )
          )
        | Describe lst -> (
            let sp_name = List.fold_left (fun acc a -> acc^a) "" lst in
            ANSITerminal.(print_string [house] 
                            ("Description for "^sp_name^":\n"
                             ^(Hogwarts.spell_description name sp_name))
                         );
            play player enemy house name
          )
        | View -> (ANSITerminal.(print_string [house] 
                                   "The following spells can be casted: \n");
                   list_cards (State.to_list_hand player) house;
                   play player enemy house name)
        | Instruction -> (ANSITerminal.(print_string [house] 
                                          "Rules are simple:\n
    - You have a deck and spell cards that attack the opponent or heal you\n
    - Each turn you can just cast or draw and cast\n
    - You play against an AI that takes its turn after you\n
    - Winner is the person who makes the other reach 0 or less health");
                          play player enemy house name)
        | Forfeit -> (ANSITerminal.(
            print_string [house] 
              "Turns out you weren't so tough and brave...\n"); exit 0)
        | Help -> raise Command.Invalidcommand
        | Status -> ANSITerminal.(print_string [house] "Your health:\n");
          ANSITerminal.(print_string [magenta] (string_of_int 
                                                  (State.get_hp player)));
          ANSITerminal.(print_string [house] "\nEnemy's health:\n");
          ANSITerminal.(print_string [magenta] (string_of_int 
                                                  (State.get_hp enemy)));
          play player enemy house name)


      with Command.Invalidcommand ->
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
  let enemy_state = State.init_enemy hogwarts in (
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

     (* play (State.init_player Hogwarts player_name) (State.init_enemy Hogwarts)
        house Hogwarts *)
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
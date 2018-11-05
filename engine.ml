type end_state = Win | Loss | Continue

(** [print_state caster target house] prints all of the status info on the 
    [caster] in [house] style. *)
let print_state caster target house = 
  ANSITerminal.(print_string [house]("\n"^
                                     (State.get_name caster)^"'s health: ");
                print_string [magenta] (string_of_int 
                                          (State.get_hp caster)));
  let dazed_time = State.get_dazed caster in
  if dazed_time > 0 then (
    ANSITerminal.(print_string [house]("\nDazed: ");
                  print_string [magenta] (string_of_int dazed_time);
                  print_string [house](" turns"))
  );
  let effects = State.get_prolong_tupes caster in
  if List.length effects > 0 then (
    ANSITerminal.(print_string [magenta] "\nEffects: Turns Left");
    List.iter (fun (a,b) -> 
        ANSITerminal.(print_string [house]
                        ((string_of_int a)^": "^(string_of_int b)^"\n"))) 
      effects
  );
  if (State.get_blocked target) = 1 then
    ANSITerminal.(print_string [house;ANSITerminal.Bold] "\nIs blocking\n")                             

(** [cast_spell spell caster target house] prints the casting [spell] that
    affects the target and printed info is in [house] style *)
let cast_spell (spell:Hogwarts.spell_info) (caster:State.t) (target:State.t)
    (house:ANSITerminal.style)=
  let name = State.get_name caster in (
    ANSITerminal.(print_string [house] 
                    ("\n"^name^" casts "^
                     (Hogwarts.spell_name spell)));
    State.cast spell caster target)

(** [spell_finder spells min_threshold max_threshold] takes a non-empty [spells]
    list and returns a tuple of the (minim*maximum) damaging spell in the given 
    thresholds (inclusive). *)
let spell_finder (spells:Hogwarts.spell_info list) 
    (min_threshold:int) (max_threshold:int) : 
  ((Hogwarts.spell_info) * ((Hogwarts.spell_info))) = 
  let max_damage = List.fold_left (fun max a -> 
      let damage = Hogwarts.spell_damage a in
      if max < damage && damage <= max_threshold then damage else max) 
      (min_threshold) spells in
  let min_damage = List.fold_left (fun max a -> 
      let damage = Hogwarts.spell_damage a in
      if max > damage && damage >= min_threshold then damage else max) 
      (max_threshold) spells in
  let min_spell =
    try (
      List.find (fun a -> (Hogwarts.spell_damage a) = min_damage) 
        spells 
    ) with Not_found -> (
        List.hd spells
      ) in
  let max_spell = 
    try (
      List.find (fun a -> (Hogwarts.spell_damage a) = max_damage) 
        spells 
    ) with Not_found -> (
        List.hd spells
      ) in (min_spell, max_spell)

(** [enemy_turn hogwarts enemy player house] takes in the arguments with enemy
    as the caster and performs a basic naive action (attacking with as much 
    damage each time as possible). It returns a tuple where the first argument 
    is the new enemy state and the second, the new player state.*)
let rec enemy_turn ?skip_draw:(skip_draw=false)(hogwarts:Hogwarts.t)
    (enemy:State.t) (player:State.t) (house:ANSITerminal.style) : 
  (State.t*State.t)=
  (*Quick health check*)
  if (State.get_hp enemy) <= 0 then (enemy,player)
  else (
    (*Either get 7 cards or maximum that is in deck*)
    let enemy_hand = State.to_list_hand enemy in
    let hand_size = (List.length enemy_hand) in
    if hand_size < 7 && not skip_draw then
      let new_hand = State.draw enemy in
      if(List.length (State.to_list_hand new_hand)) = hand_size then
        enemy_turn ~skip_draw:(true) hogwarts new_hand player house
      else 
        enemy_turn hogwarts new_hand player house
    else (
      if (State.get_dazed enemy) > 0 then (
        ANSITerminal.(print_string [house] 
                        "\n\nEnemy is dazed and cannot cast!\n");
        (State.update_dazed enemy, player))
      else (
        if hand_size = 0 then (
          ANSITerminal.(print_string [house] "\n Opponent skips their go");
          ((State.draw enemy),player)
        ) else (
          let hp = State.get_hp enemy in
          let min_max = spell_finder enemy_hand (-100) 100 in
          (*max heal may be the same as min damage if player has no healing
            spells*)
          let max_heal = fst min_max in
          let is_healing = Hogwarts.spell_damage max_heal < 0 in
          let max_damage = snd min_max in
          let min_damage = fst (spell_finder enemy_hand 0 100) in
          (* If a player blocks do not waste resources on them *)
          if State.get_blocked player = 1 then (
            if hp < 100 then (
              if is_healing && 
                 Hogwarts.spell_target max_heal = "self" then (
                let tup = cast_spell max_heal enemy player house in
                ((fst tup),player)
              ) else (
                cast_spell min_damage enemy player house
              )
            ) else (
              cast_spell min_damage enemy player house
            )
          ) else (
            let player_hp = State.get_hp player in (
              (*If can kill - kill. otherwise if player hp > enemy then heal
                otherwise do as much damage as possible*)
              if Hogwarts.spell_damage max_damage > player_hp then (
                cast_spell max_damage enemy player house) else (
                if player_hp > hp then (
                  if is_healing then
                    let tup = cast_spell max_heal enemy player house in
                    ((fst tup),player)
                  else
                    cast_spell max_damage enemy player house
                ) else (
                  cast_spell max_damage enemy player house
                )
              ))
          )
        )
      )))

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
    (callback: ?asked_state:bool -> State.t -> State.t -> 
     ANSITerminal.style -> Hogwarts.t -> unit) 
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
        if List.length (State.to_list_hand drawn) > 0 then (
          let chosen_card = List.hd(State.to_list_hand drawn) in
          ANSITerminal.(print_string [house] 
                          ("You drew: "^(Hogwarts.spell_name chosen_card)));
          callback drawn enemy house hogwarts
        )
        else (
          ANSITerminal.(print_string [house] "You have no cards to draw from");
          callback drawn enemy house hogwarts)))

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
                          (sp_name ^ " incorrectly spelled. Try again\n"));
          callback player enemy house hogwarts
        )
    )
  | Instruction -> (ANSITerminal.(print_string [house] 
                                    "Rules are simple:\n
    - You have a deck and spell cards that attack the opponent or heal you\n
    - Each turn you can just cast or draw and cast\n
    - You play against an AI that takes its turn after you\n
    - Winner is the person who makes the other reach 0 or less health");
                    callback player enemy house hogwarts)
  | Status -> print_state player enemy house; print_state enemy player house;
    callback player enemy house hogwarts
  | Cast lst -> (
      (*TODO: reduce this part into its own method*)
      let sp_name = String.concat " " lst in
      try (
        let info = Hogwarts.search_spells hogwarts sp_name in
        if(List.mem (info) (State.to_list_hand player)) then (
          ANSITerminal.(print_string [house] ("You cast "^sp_name));
          let cast_update = State.cast info player enemy in (
            if(Hogwarts.spell_target info) = "self" then (
              let tup = enemy_turn hogwarts enemy (fst cast_update) house in
              callback ~asked_state:(false) (snd tup) (fst tup) house hogwarts
            ) else (
              let tup = enemy_turn hogwarts (snd cast_update) 
                  (fst cast_update) house in
              callback ~asked_state:(false) (snd tup) (fst tup) house hogwarts
            ))
        )
        else (
          ANSITerminal.(
            print_string [house] "Nice try but you don't have that spell");
          callback player enemy house hogwarts
        )) with Hogwarts.UnknownSpell sp_name -> (
          ANSITerminal.(print_string [house] 
                          (sp_name ^ " incorrectly spelled. Try again"));
          callback player enemy house hogwarts
        )
    )

(** [affirmation hogwarts enemy_info enemy] takes in the enemy for the info 
    output and returns a boolean if the player chooses to fight that enemy.
    Y or YES will fight, any other input (N or invalid) will return to character
    menu. *)
let affirmation (hogwarts:Hogwarts.t)(enemy_info:Hogwarts.character_info) 
    (enemy:State.t) : bool =
  ANSITerminal.(print_string [ANSITerminal.Bold; cyan] 
                  (Hogwarts.character_name enemy_info);
                print_string [cyan] ("\n-=-=-=-=-\n"^
                                     (Hogwarts.character_description enemy_info)
                                     ^"\n-=-=-=-=-\n");
                print_string [cyan] ("Level: "^
                                     (string_of_int (State.get_level enemy)));
                print_string [cyan] ("\nStarting Health: "^
                                     (string_of_int (State.get_hp enemy)));
                print_string [cyan] ("\nHouse: "^
                                     (Hogwarts.character_house enemy_info)));
  ANSITerminal.(print_string [magenta] 
                  "\n\nDo you want to duel this opponent (Y/N)? ");
  let ans = String.uppercase_ascii (read_line ()) in
  ans = "Y" || ans = "YES"

(** [inverse_colour colour] takes in a colour and returns the inverse (if it was
    in the background. If not one of four house clours 
    (red, green, blue, yellow) then default ANSITerminal style is returned. *)
let inverse_colour (colour:ANSITerminal.style) : ANSITerminal.style list =
  match colour with
  | ANSITerminal.Foreground(Red) -> [ANSITerminal.white;ANSITerminal.on_red]
  | ANSITerminal.Foreground(Green) -> [ANSITerminal.white;ANSITerminal.on_green]
  | ANSITerminal.Foreground(Blue) -> [ANSITerminal.white;ANSITerminal.on_blue]
  | ANSITerminal.Foreground(Yellow) -> 
    [ANSITerminal.black;ANSITerminal.on_yellow]
  | _ -> [ANSITerminal.default]

(** [choose_opponenet player hogwarts house callback] gives the option for the 
    player to choose from the range of opponents to face. [callback] is 
    called when player chooses a character and affirms that they want 
    to battle them. *)
let rec choose_opponent (player:State.t) (hogwarts:Hogwarts.t) 
    (house:ANSITerminal.style) 
    (callback: ?asked_state:bool -> State.t -> State.t -> 
     ANSITerminal.style -> Hogwarts.t -> unit) : unit =
  ANSITerminal.(print_string [magenta] "\nYour level: ";
                print_string [house] (string_of_int (State.get_level player));
                print_string [magenta] "\nWins to next level: ";
                print_string [house] 
                  (string_of_int (State.required_wins player - 
                                  (List.length 
                                     (State.get_defeated_enemies player)))));

  ANSITerminal.(print_string [magenta] 
                  "\n\nHere are the possible opponents you may face:\n\n");
  ANSITerminal.(print_string [cyan] 
                  "Name: Level ";
                print_string [black; on_cyan] 
                  "(inverted colors are completed)";
                print_string [black] "\n");
  let mapped = List.map (fun c -> (Hogwarts.character_name c),
                                  (string_of_int (Hogwarts.character_level c))) 
      (Hogwarts.get_characters hogwarts) in
  (*print all enemies accordingly*)
  List.iter (fun tup -> let colour = if(List.mem (fst tup)
                                          (State.get_defeated_enemies player)) 
                          then 
                            (inverse_colour house) 
                          else [house] in 
              ANSITerminal.( print_string
                               colour ((fst tup)^": "^(snd tup));
                             print_string [default] "\n"
                           )) mapped;

  ANSITerminal.(print_string [magenta] 
                  "\nEnter the name of who you want to face > ");
  (*This complete mess just makes the string camel case*)
  let target_name_lst = String.split_on_char ' ' (read_line ()) in
  let target_name = String.concat " " (List.map (fun str -> 
      String.capitalize_ascii (String.lowercase_ascii str)) target_name_lst) in
  try (
    let enemy_char = Hogwarts.search_characters hogwarts target_name in
    let enemy = (State.init_enemy_with_level_deck hogwarts 
                   (Hogwarts.character_name enemy_char) 
                   (Hogwarts.character_house enemy_char)) in

    if(affirmation hogwarts enemy_char enemy) then (
      print_state player enemy house; print_state enemy player house;
      callback player enemy house hogwarts
    ) else
      choose_opponent player hogwarts house callback
  ) with Hogwarts.UnknownCharacter target_name -> (
      ANSITerminal.(print_string [house] 
                      "\n\nI have no idea where you saw that name...\n
                Try entering an actual person's name this time\n");
      choose_opponent player hogwarts house callback
    )

(** [play player enemy house name] is the main game loop that takes in [player]
    and [enemy] states, [house] text color and [name] hogwarts game state and 
    progresses based on player inputs until a win/loss condition is fulfilled.
    The [asked_state] is an optional argument that if set to false will by 
    default print out the statuses of [player] and [enemy]*)
let rec play ?asked_state:(asked_state=true) (player:State.t) (enemy:State.t)
    (house: ANSITerminal.style) (hogwarts: Hogwarts.t)  =
  match check_conditions player enemy with
  | Win -> (ANSITerminal.(print_string [Bold; cyan] "\n\n-=-=-=-=-=-=-=-=-";
                          print_string [house] "\nCongrats you win!\n";
                          print_string [Bold; cyan] "-=-=-=-=-=-=-=-=-\n"); 
            choose_opponent 
              (State.level_up
                 (State.add_defeated_enemy player 
                    (State.get_name enemy) hogwarts)) 
              hogwarts house play)
  | Loss -> (ANSITerminal.(print_string [house] "\nYou lose :( and die\n"); 
             exit 0)
  | Continue -> (
      if (State.get_dazed player) > 0 then (
        ANSITerminal.(print_string [house] 
                        "\nYou are dazed and unable to cast\n");
        let step = State.update_dazed player in
        let tup = enemy_turn hogwarts enemy step house in
        play (snd tup) (fst tup) house hogwarts)

      else (
        (*This is to avoid double status description*)
        if not asked_state then (print_state player enemy house; 
                                 print_state enemy player house);
        ANSITerminal.print_string [house] "\n\nEnter an action to perform > ";
        let cmd = read_line () in
        try (
          run_command player enemy house hogwarts (Command.parse cmd) play
        ) with Command.Invalidcommand ->
          (ANSITerminal.(print_string [house] 
                           "Invalid command. Possible commands: \n
    Draw, cast [card name], describe [card name], view, instruction, help, 
    status, forfeit"); play player enemy house hogwarts )))

(*Below is all once-off used for starting the game*)

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

(** [play_init f1 f2 house] takes in a spells json [f1] and characters [f2], 
    and a ANSITerminal.style [house] color, then starts the game.*)
let play_init f1 f2 player h_house house = 
  let j1 = Yojson.Basic.from_file f1 in
  let j2 = Yojson.Basic.from_file f2 in
  let hogwarts = Hogwarts.from_json j1 j2 in 
  let player_state = State.init_player_with_level_deck hogwarts player h_house in
  choose_opponent player_state hogwarts house play

(** [name house] takes in the ANSITerminal colour [house] and records the 
    player's name to be used in gameplay. 
    The name specified should be regular ASCII alphabet with no numbers 
    or special characters. Method will repeat until a valid input is received.*)
let rec name h_house house =
  ANSITerminal.(
    print_string [magenta] "Now, your name. Let's keep it simple, 
  no numbers or symbols. Just letters. Please.\n\n";
    print_string [magenta] "Enter your name > "
  );
  let reg = Str.regexp "^[A-Za-z]+[A-Za-z ]*$" in
  let player_name = read_line () in
  if Str.string_match reg player_name 0 then
    (ANSITerminal.(
        print_string [house] ("Welcome " ^ player_name ^ "!");
      );
     play_init "spells.json" "characters.json" (String.trim player_name) h_house house 
    ) else (ANSITerminal.(
      print_string [magenta] "Simple stuff, 
      I wonder how you will fare in Hogwarts if you struggle at even this... 
      Try again\n"
    ); name h_house house)

(*ASCII Art FTW: http://patorjk.com/software/taag/*)
let intro_text = [
  ["    __  __";"   / / / /";"  / /_/ /";" / __  /";"/_/ /_/";"       "];
  ["    ";"___ ";" __ \\";" /_/ /";"\\____/";"     "];
  ["       ";" ____ _";"/ __ `/";" /_/ /";"\\__, / ";"/____/ "];
  ["         ";"_      __";" | /| / /";"| |/ |/ /";"|__/|__/";"         "];
  ["      ";"____ _";" __ `/";" /_/ /";"\\__,_/";"     "];
  ["      ";"_____";" ___/";" /  ";"_/   ";"      "];
  ["__ ";"/ /_";" __/";"/ /_";"\\__/";"    "];
  ["     ";"_____";" ___/";"(__  ) ";"____/  ";"       "]
]

(** [get_color_code num] gets the respective color of house in accordance with
    [num]. 0 or 1 is red, 2,3 yellow, 4,5 green, 6,7 blue. Any other number
    will return white.*)
let get_color_code (num:int) : ANSITerminal.style =
  match num with
  | 0 | 1 -> ANSITerminal.red
  | 2 | 3 -> ANSITerminal.yellow
  | 4 | 5 -> ANSITerminal.green
  | 6 | 7 -> ANSITerminal.blue 
  | _ -> ANSITerminal.white

let ref_num = ref 0

(** [print_arr arr] prints he first element of a non-empty list and returns
    all non-printed elements (can be an empty list).*)
let rec print_arr (arr:string list) : (string list)=
  let itr = (!ref_num mod 8) in
  let color = get_color_code itr in
  match arr with
  | [] -> []
  | h::t -> (ANSITerminal.(print_string [color] (h)); 
             ref_num := !ref_num + 1;
             if itr = 7 then print_string "\n";
             t)

(** [print_arr_2d arr] prints all elements from a list of string lists.
    The printing pattern is it prints the head element of each respective list
    and iterates forward until all lists are empty.*)
let rec print_arr_2d (arr:(string list) list) =
  match arr with
  | [] -> ()
  | h::t -> ( let printed = print_arr h in
              if List.length printed > 0 then
                print_arr_2d (t@[printed])
              else
                print_arr_2d t
            )

(** [house] begins the game by asking the player for their chose Harry Potter
    house. It will progress to name and gameplay if a valid name is provided
    (case-insensitive) or repeat until a valid one is inputted.*)
let rec house () =
  ANSITerminal.(
    print_string [magenta] "\nWelcome to \n");
  print_arr_2d intro_text;
  ANSITerminal.(
    print_string [magenta] 
      "\n\nThe sorting hat is at lunch so you'll have to choose your own house.
      \nIn case you forgot, the choices are:\n";
    print_string [red] "Gryffindor "; print_string [green] "Slytherin ";
    print_string [yellow] "Hufflepuff "; print_string [blue] "Ravenclaw";
    print_string [magenta] "\n\nEnter house name > ";
  );
  let chosen = read_line () in
  match choose_house (chosen) with
  | Some x -> name chosen x
  | None -> ANSITerminal.(print_string [magenta] "... That isn't a house. 
    They are literally in front of you. Let's try this again.\n"); house ()

(**/**)
let () = house ()
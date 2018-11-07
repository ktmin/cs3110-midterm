module type Mainview = sig
  val print_state : State.t -> bool -> unit
  val list_cards : State.t -> unit
  val print_enemy : Hogwarts.character_info -> State.t -> unit
  val print_post_condition : string -> int -> unit
  val print_cast : State.t  -> Hogwarts.spell_info -> unit
  val print_spell_details : string -> Hogwarts.t -> string -> unit
  val print_title : unit -> unit
  val print_cmd_input : string -> string -> unit
  val print_formatted_lst : string -> string list -> unit
  val print_enemy_lst : Hogwarts.t -> State.t -> unit
  val print_house : string -> unit
  val print_card : string -> int -> string -> string list -> unit
end

module Make : Mainview = struct
  (*===Start Util Methods===*)
  (** [print_clear len] prints out [len] number of newlines on screen. *)
  let rec print_clear (len:int) : unit =
    if len <= 0 then
      ()
    else (
      print_string "\n"; print_clear (len-1)
    ) 

  (** [get_house house_name] gets the respective color code for the house of
      [house_name]. Defaults to [white] if house is not found*)
  let get_house (house_name:string) : ANSITerminal.style =
    match (String.lowercase_ascii house_name) with
    | "gryffindor" -> ANSITerminal.red
    | "slytherin" -> ANSITerminal.green
    | "ravenclaw" -> ANSITerminal.blue
    | "hufflepuff" -> ANSITerminal.yellow
    | _ -> ANSITerminal.magenta

  (** [inverse_colour house] takes in a house and returns the inverse of the 
      usual. If not one of four house colours (red, green, blue, yellow) 
      then default ANSITerminal style is returned. *)
  let inverse_colour (house:string) : ANSITerminal.style list =
    match (String.lowercase_ascii house) with
    |"gryffindor" -> [ANSITerminal.yellow;
                      ANSITerminal.on_red]
    |"slytherin" -> [ANSITerminal.white;
                     ANSITerminal.on_green]
    |"ravenclaw" -> [ANSITerminal.white;
                     ANSITerminal.on_blue]
    |"hufflepuff" -> [ANSITerminal.black;
                      ANSITerminal.on_yellow]
    | _ -> [ANSITerminal.default]
  (*===End Util Methods===*)
  (*===Start ASCII Art Utils===*)
  (*ASCII Art FTW: http://patorjk.com/software/taag/ (intro -> puff) *)
  (** The ASCII art for intro text*)
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

  (** The ASCII art for slytherin. *)
  let slyth = [
    [" _____"; "/  ___|"; "\\ `--."; " `--. \\"; "/\\__/ /"; "\\____/"; "      "; 
     "      "];
    [" _ "; " |"; "| |"; " |"; " |"; "|_|"; "   "; "   "];
    ["      "; "     "; "_   _"; " | | |"; " |_| |"; "\\__, |"; " __/ |"; 
     "|___/ "];
    ["_   "; "| | "; "| |_"; " __|"; " |_"; "\\__|"; "    "; "     "];
    ["_    "; "| |    "; "| |__ "; " '_  \\"; "| | | |"; "_| |_|"; "      "; 
     "      "];
    ["      "; "     "; "  ___ ";"/ _ \\";"  __/";"\\___|";"     "; "     "];
    ["    "; 
     "     "; 
     "_ __"; 
     " '__|"; 
     " |  "; 
     "_|  "; "    "; "    "];
    ["  _ "; "(_)"; " _ "; " |"; "| |"; "|_|"; "   "; "   "];
    ["      "; "      "; "_ __  "; " '_  \\ "; " | | |"; "_| |_|"; "      "; 
     "      "]
  ]

  (** The ASCII art for gryffindor. *)
  let gryff = [
    [" .---. "; "/   __}"; "\\  {_ }"; " `---' "];
    [".----."; "| {}  }"; "| .-. \\"; "`-' `-' "];
    [".-.  .-."; "\\ \\/ / "; " }  {  "; "`--'  "];
    [".----."; "| {_  "; "| |   "; "`-'   "];
    [".----."; "| {_  "; "| |   "; "`-'   "];
    [".-."; "| |"; "| |"; "`-'"];
    [".-. .-."; "|  `| |"; "| |\\  |"; "`-' `-'"];
    [".----. "; "| {}  \\"; "|     /"; "`----' "];
    [" .----. "; "/  {}  \\"; "\\      /"; " `----' "];
    [".----. "; "| {}  }"; "| .-. \\"; "`-' `-'"]
  ]

  (** The ASCII art for ravenclaw. *)
  let claw = [
    ["█▄▄▄▄ "; "█  ▄▀ "; "█▀▀▌  "; "█  █  "; "  █   "; " ▀    "; "      "];
    ["██   "; "█ █  "; "█▄▄█ "; "█  █  "; "   █ "; "  █  "; " ▀   "];
    ["     ▄   "; "     █  "; "█     █ "; "█    █ "; "  █  █  "; "   █▐   "; 
     "   ▐    "];
    ["▄███▄   "; "█▀   ▀  "; "██▄▄    "; "█▄   ▄▀ "; "▀███▀   "; "        "; 
     "        "];
    ["   ▄   "; "    █  "; "██   █ "; "█ █  █ "; "█  █ █ "; "█   ██ "; 
     "       "];
    ["▄█▄    "; "█▀ ▀▄  "; "█   ▀  "; "█▄  ▄▀ "; "▀███▀  "; "       "; 
     "       "];
    ["█    "; "█    "; "█    "; "███▄ "; "    ▀"; "     "; "     "];
    ["██   "; "█ █  "; "█▄▄█ "; "█  █  "; "   █ "; "  █  "; " ▀   "];
    ["  ▄ ▄   "; " █   █  "; "█  ▄  █ "; "█  █  █ "; " █ █ █ █"; "  ▀   ▀ "; 
     "        "]
  ]

  (** The ASCII art for hufflepuff. *)
  let puff = [
    ["       "; " ,---. "; "| .-. |"; "| '-' '"; "|  |-' "; "`--'   "];
    ["       "; " ,---. "; "| .-. |"; "' '-' '"; " `---' "; "       "];
    ["  ,--.  "; ",-'  '-."; "'-.  .-'"; "  |  |  "; "  `--'  "; "        "];
    ["        "; " ,--,--."; "' ,-.  |"; "\\ '-'  |"; " `--`--'"; "        "];
    ["  ,--.  "; ",-'  '-."; "'-.  .-'"; "  |  |  "; "  `--'  "; "        "];
    ["       "; " ,---. "; "| .-. |"; "' '-' '"; " `---' "; "       "]
  ]

  (** [even_odd even odd num] returns a list of styles based on if the [num] is 
      even or odd*)
  let even_odd (even:ANSITerminal.style list) (odd:ANSITerminal.style list) (num:int) :
    ANSITerminal.style list=
    if(num mod 2) = 0 then
      even
    else
      odd

  (** [get_color_code num] gets the respective color of house in accordance with
      [num]. 0 or 1 is red, 2,3 yellow, 4,5 green, 6,7 blue. Any other number
      will return white.*)
  let get_color_code (num:int) : ANSITerminal.style list =
    match num with
    | 0 | 1 -> [ANSITerminal.red]
    | 2 | 3 -> [ANSITerminal.yellow]
    | 4 | 5 -> [ANSITerminal.green]
    | 6 | 7 -> [ANSITerminal.blue]
    | _ -> [ANSITerminal.white]

  (** [ref_num] counts the current iteration for color coding the title. *)
  let ref_num = ref 0

  (** [print_arr arr] prints he first element of a non-empty list and returns
      all non-printed elements (can be an empty list).*)
  let rec print_arr (modd:int) (fn: int -> ANSITerminal.style list) 
      (arr:string list) : (string list)=
    let itr = (!ref_num mod modd) in
    let color = fn itr in
    match arr with
    | [] -> []
    | h::t -> (ANSITerminal.(print_string color (h)); 
               ref_num := !ref_num + 1;
               if itr = (modd-1) then print_string "\n";
               t)

  (** [print_arr_2d modd fn arr] prints all elements from a list of string 
      lists. The printing pattern is it prints the head element of each 
      respective list and iterates forward until all lists are empty.*)
  let rec print_arr_2d (modd:int) (fn: int -> ANSITerminal.style list) 
      (arr:(string list) list) =
    match arr with
    | [] -> ()
    | h::t -> ( let printed = print_arr modd fn h in
                if List.length printed > 0 then
                  print_arr_2d modd fn (t@[printed])
                else
                  print_arr_2d modd fn t
              )

  (*===End ASCII Art Utils===*)    

  (** Prints formatted list. For more details go to 
            {{: View.View.html#VALprint_formatted_lst} 
            View.View.print_formatted_lst}. *)
  let print_formatted_lst (house_name:string) (lst:string list) : unit =
    let house = get_house house_name in
    let rec run_through house lst =
      match lst with 
      | [] -> print_string "\n";
      | h::t -> (
          ANSITerminal.(print_string [house] (h^"\n"));
          run_through house t
        ) in run_through house lst

  (*===Start ASCII Card Methods===*)
  (** [chop width lst] chops a list's strings into one string until no more
      can be done, then returns the same list with the new string at the head.*)
  let chop (width:int) (lst:string list) : string list =
    let start = "" in
    let rec append (lst:string list) (str:string) : string list =
      match lst with
      | [] -> [str]
      | h::t -> (
          let appended = str^" "^h in
          if String.length appended > width then
            (str::lst)
          else
            append t appended
        ) in append lst start

  (** [partition_list width lst] partitions a list in such a way that all strings
      in returned list are less than or equal to [width] characters.*)
  let partition_list (width:int) (lst:string list) : string list =
    let acc = [] in
    let rec partition acc lst = 
      match lst with
      | [] -> acc
      | _::_ -> (
          match chop width lst with
          | [] -> acc
          | h::t -> partition (acc@[h]) t
        ) in partition acc lst

  (** [get_lines width] generates [width] amount of dashes for ascii card.*)
  let get_lines ?pattern:(pattern="-") (width:int) : string =
    let start_str = "" in
    let rec run width str =
      if width <= 0 then str
      else run (width-1) (str^pattern)
    in run width start_str

  (** [get_centered_text width input] returns a string of set [width] with
      [input] at its center. *)
  let get_centered_text (width:int) (input:string) : string =
    let len = String.length input in
    let buffer = (width-len)/2 in
    let buffers = get_lines ~pattern:(" ") buffer in
    buffers^input^buffers

  (** [pad width s] returns a string that is padded to be exactly [width] size.
  *)
  let rec pad (width:int) (s:string) : string = (
    if String.length s < width then
      pad width (s^" ")
    else
      s
  )

  (*This is at the bottom to utilize print_formatted_lst*)
  (** Prints ascii card. For more details go to 
      {{: View.View.html#VALprint_card} 
      View.View.print_card}. *)
  let print_card (house:string) (width:int) (header:string) (body:string list) : 
    unit =
    let rim, header = (get_lines width), (get_centered_text width header) in
    let top, bottom, mid = ("."^rim^"."), ("'"^rim^"'"), 
                           (get_lines ~pattern:("=") width) in
    let body_lst = partition_list width body in
    let width_adjust = List.map (pad width) body_lst in
    let final_lst = [header;mid]@width_adjust in
    let mapped = List.map (fun s -> "|"^s^"|") final_lst in

    print_formatted_lst house (top::(mapped@[bottom]))
  (*===End ASCII Card Methods===*)

  (*===Start State Dependant Methods===*)
  (*TODO: ASCII character card*)
  let print_enemy (enemy:Hogwarts.character_info) (enemy_state:State.t) 
    : unit =
    let name = Hogwarts.character_name enemy in
    let len = (String.length name)+14 in
    let lvl = "Level: "^(string_of_int (State.get_level enemy_state)) in
    let health = "Starting Health: "^
                 (string_of_int (State.get_hp enemy_state)) in
    let house = "House: "^(Hogwarts.character_house enemy) in
    let break = get_lines ~pattern:(" ") (len-1) in
    let desc = Str.split (Str.regexp " ")
        (Hogwarts.character_description enemy) in
    print_card (Hogwarts.character_house enemy) len name 
      ([house; health; lvl; break]@desc)
  (* ANSITerminal.(print_string [ANSITerminal.Bold; house] 
                  (Hogwarts.character_name enemy);
                print_string [house] 
                  ("\n-=-=-=-=-\n"^
                   (Hogwarts.character_description enemy)^"\n-=-=-=-=-\n");
                print_string [house] 
                  ("Level: "^(string_of_int (State.get_level enemy_state)));
                print_string [house] 
                  ("\nStarting Health: "^
                   (string_of_int (State.get_hp enemy_state)));
                print_string [house] 
                  ("\nHouse: "^(Hogwarts.character_house enemy))) *)

  (** Prints state of caster. For more details go to 
      {{: View.View.html#VALprint_state} View.View.print_state}. *)
  let print_state (caster:State.t) (blocking:bool) : unit =
    let house = get_house (State.get_house caster) in
    ANSITerminal.(print_string [house]
                    ("\n"^(State.get_name caster)^"'s health: ");
                  print_string [magenta] 
                    (string_of_int (State.get_hp caster)));
    let dazed_time = State.get_dazed caster in
    if                                                     dazed_time > 0 then (
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
    if blocking then
      ANSITerminal.(print_string [house;ANSITerminal.Bold] "\nIs blocking\n") 

  (** Prints cards of caster. For more details go to 
      {{: View.View.html#VALlist_cards} View.View.list_cards}. *)
  let list_cards (caster:State.t) : unit = 
    let house, spells = (get_house (State.get_house caster)), 
                        (State.to_list_hand caster) in
    let rec list_all_cards house spells = 
      match spells with
      | [] -> (
          ANSITerminal.(print_string [house] "\n\nYeah... You have no spells")
        )
      | h::[] -> (
          ANSITerminal.(print_string [magenta] 
                          ((Hogwarts.spell_name h ^ 
                            "\n\nEnter: Describe spell_name to see spell 
    description."))))
      | h::t -> (
          ANSITerminal.(print_string [magenta] 
                          ((Hogwarts.spell_name h) ^ ", "));
          list_all_cards house t
        ) in list_all_cards house spells

  (** Prints cast of caster. For more details go to 
      {{: View.View.html#VALprint_cast} View.View.print_cast}. *)
  let print_cast (caster:State.t) (spell:Hogwarts.spell_info) : unit =
    let name, house = (State.get_name caster), 
                      (get_house (State.get_house caster)) in
    ANSITerminal.(print_string [house]
                    ("\n"^name^" casts "^(Hogwarts.spell_name spell)))

  (** Prints list of enemies. For more details go to 
        {{: View.View.html#VALprint_enemy_lst} View.View.print_enemy_lst}. *)
  let print_enemy_lst (hogwarts:Hogwarts.t) (player:State.t) : unit =
    let p_house = State.get_house player in
    let house = get_house p_house in
    (*map enemies to name,level pair*)
    let mapped = List.map (fun c -> 
        (Hogwarts.character_name c),
        (string_of_int (Hogwarts.character_level c))) 
        (Hogwarts.get_characters hogwarts) in (
      ANSITerminal.(print_string [magenta] 
                      "\n\nHere are the possible opponents you may face:\n\n");
      ANSITerminal.(print_string [cyan] 
                      "Name: Level ";
                    print_string [black; on_cyan] 
                      "(inverted colors are completed)";
                    print_string [default] "\n");
      (*print all enemies accordingly*)
      List.iter (fun tup -> 
          let colour = if(List.mem(fst tup)(State.get_defeated_enemies player)) 
            then 
              (inverse_colour p_house) 
            else 
              [house] in 
          ANSITerminal.(print_string colour ((fst tup)^": "^(snd tup));
                        print_string [default] "\n"
                       )) mapped
    )

  (*===End State Dependant Methods===*)

  (*TODO: ASCII spell card*)
  (** Prints details of spell. For more details go to 
        {{: View.View.html#VALprint_spell_details} 
        View.View.print_spell_details}. *)
  let print_spell_details (house_name:string) (hogwarts:Hogwarts.t) 
      (spell: string) : unit =
    let len = (String.length spell)+12 in
    let info = Hogwarts.search_spells hogwarts spell in
    let dmg = "Damage: "^(string_of_int (Hogwarts.spell_damage info)) in
    let lvl = "Level: "^(string_of_int (Hogwarts.spell_level info)) in
    let t = "Type: "^(Hogwarts.spell_type info) in
    let break = get_lines ~pattern:(" ") (len-1) in
    let desc = Str.split (Str.regexp " ")
        (Hogwarts.spell_description hogwarts spell) in
    print_card house_name len spell 
      ([dmg; lvl; t; break]@desc)

  (** Prints win/loss condition output. For more details go to 
          {{: View.View.html#VALprint_post_condition} 
          View.View.print_post_condition}. *)
  let print_post_condition (house_name:string) (condition:int) 
    : unit =
    let house = get_house house_name in (
      if condition > 0 then
        ANSITerminal.(print_string [Bold; cyan] "\n\n-=-=-=-=-=-=-=-=-";
                      print_string [house] "\nCongrats you win!\n";
                      print_string [Bold; cyan] "-=-=-=-=-=-=-=-=-\n")
      else if condition < 0 then
        ANSITerminal.(print_string [house] 
                        "\nYou lose :( everything goes dark...\nand silence")
      else
        ()
    )
  (** Prints ASCII house. For more details go to 
            {{: View.View.html#VALprint_house} View.View.print_house}. *)
  let print_house (house:string) =
    ref_num := 0;
    match (String.lowercase_ascii house) with
    | "gryffindor" -> print_arr_2d 10
                        (even_odd [ANSITerminal.red]
                           [ANSITerminal.yellow]) gryff
    | "slytherin" -> print_arr_2d 9
                       (even_odd [ANSITerminal.green]
                          [ANSITerminal.white]) slyth
    | "hufflepuff" -> print_arr_2d 6
                        (even_odd [ANSITerminal.black; ANSITerminal.on_yellow]
                           [ANSITerminal.yellow; ANSITerminal.on_black]) puff
    | "ravenclaw" -> print_arr_2d 9
                       (even_odd [ANSITerminal.blue]
                          [ANSITerminal.white]) claw
    | _ -> ()

  (** Prints game title. For more details go to 
                  {{: View.View.html#VALprint_title} 
                  View.View.print_title}. *)
  let print_title (_:unit) : unit =
    ANSITerminal.(
      print_string [magenta] "\nWelcome to \n");
    print_arr_2d 8 get_color_code intro_text

  (** Prints string before cmd input. For more details go to 
                {{: View.View.html#VALprint_cmd_input} 
                View.View.print_cmd_input}. *)
  let print_cmd_input (house_name:string) (input:string) : unit =
    print_clear 2;
    let house = get_house house_name in
    ANSITerminal.(print_string [Bold;house] (input^" > "))
end
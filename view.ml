module type View = sig
  val print_state : State.t -> unit
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
end

module Make : View = struct
  (*===Start Util Methods===*)
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

  let claw = [
    ["█▄▄▄▄ "; "█  ▄▀ "; "█▀▀▌  "; "█  █  "; "  █   "; " ▀    "; "      "];
    ["██   "; "█ █  "; "█▄▄█ "; "█  █  "; "   █ "; "  █  "; " ▀   "];
    ["     ▄   "; "     █  "; "█     █ "; "█    █ "; "  █  █  "; "   █▐   "; "   ▐    "];
    ["▄███▄   "; "█▀   ▀  "; "██▄▄    "; "█▄   ▄▀ "; "▀███▀   "; "        "; "        "];
    ["   ▄   "; "    █  "; "██   █ "; "█ █  █ "; "█  █ █ "; "█   ██ "; "       "];
    ["▄█▄    "; "█▀ ▀▄  "; "█   ▀  "; "█▄  ▄▀ "; "▀███▀  "; "       "; "       "];
    ["█    "; "█    "; "█    "; "███▄ "; "    ▀"; "     "; "     "];
    ["██   "; "█ █  "; "█▄▄█ "; "█  █  "; "   █ "; "  █  "; " ▀   "];
    ["  ▄ ▄   "; " █   █  "; "█  ▄  █ "; "█  █  █ "; " █ █ █ █"; "  ▀   ▀ "; "        "]
  ]

  let puff = [
    ["       "; " ,---. "; "| .-. |"; "| '-' '"; "|  |-' "; "`--'   "];
    ["       "; " ,---. "; "| .-. |"; "' '-' '"; " `---' "; "       "];
    ["  ,--.  "; ",-'  '-."; "'-.  .-'"; "  |  |  "; "  `--'  "; "        "];
    ["        "; " ,--,--."; "' ,-.  |"; "\\ '-'  |"; " `--`--'"; "        "];
    ["  ,--.  "; ",-'  '-."; "'-.  .-'"; "  |  |  "; "  `--'  "; "        "];
    ["       "; " ,---. "; "| .-. |"; "' '-' '"; " `---' "; "       "]
  ]

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

  (* [ref_num] counts the current iteration for color coding the title. *)
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
  (*===Start State Dependant Methods===*)
  (*TODO: ASCII character card*)
  let print_enemy (enemy:Hogwarts.character_info) (enemy_state:State.t) 
    : unit =
    let house = get_house (State.get_house enemy_state) in
    ANSITerminal.(print_string [ANSITerminal.Bold; house] 
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
                    ("\nHouse: "^(Hogwarts.character_house enemy)))

  let print_state (caster:State.t) : unit =
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
    (*TODO: this is broken*)
    if (State.get_blocked caster) = 1 then
      ANSITerminal.(print_string [house;ANSITerminal.Bold] "\nIs blocking\n") 

  (*TODO: change this to be better sorted*)
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

  let print_cast (caster:State.t) (spell:Hogwarts.spell_info) : unit =
    let name, house = (State.get_name caster), 
                      (get_house (State.get_house caster)) in
    ANSITerminal.(print_string [house]
                    ("\n"^name^" casts "^(Hogwarts.spell_name spell)))

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
  let print_spell_details (house_name:string) (hogwarts:Hogwarts.t) 
      (spell: string) : unit =
    let house = (get_house house_name) in
    ANSITerminal.(print_string [house] 
                    ("Description for "^spell^":\n"
                     ^(Hogwarts.spell_description hogwarts spell)))

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

  let print_title (_:unit) : unit =
    ANSITerminal.(
      print_string [magenta] "\nWelcome to \n");
    print_arr_2d 8 get_color_code intro_text

  let print_cmd_input (house_name:string) (input:string) : unit =
    print_clear 2;
    let house = get_house house_name in
    ANSITerminal.(print_string [Bold;house] (input^" > "))

  let print_formatted_lst (house_name:string) (lst:string list) : unit =
    let house = get_house house_name in
    let rec run_through house lst =
      match lst with 
      | [] -> print_string "\n";
      | h::t -> (
          ANSITerminal.(print_string [house] (h^"\n"));
          run_through house t
        ) in run_through house lst
end
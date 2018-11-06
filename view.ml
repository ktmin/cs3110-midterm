module type View = sig
  val print_state : State.t -> unit
  val list_cards : State.t -> unit
  val print_enemy : Hogwarts.character_info -> State.t -> unit
  (*Internal commands:
    choose_house, inverse_house, ref num, print_arr, print_arr2d, intro_text,
    get_colour_codes*)
  val print_post_condition : string -> Model.end_state -> unit
  val print_cast : State.t  -> Hogwarts.spell_info -> unit
  val print_spell_details : string -> Hogwarts.t -> Hogwarts.spell_info -> unit
  val print_title : unit -> unit
  (*val print_cmd_in : string -> unit
    val print_validity : string -> string -> unit*)
  (*TODO: print menu*)
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
  let get_house (house_name: string) : ANSITerminal.style =
    match house_name with
    | "gryffindor" -> ANSITerminal.red
    | "slytherin" -> ANSITerminal.green
    | "ravenclaw" -> ANSITerminal.blue
    | "hufflepuff" -> ANSITerminal.yellow
    | _ -> ANSITerminal.white

  (** [inverse_colour colour] takes in a colour and returns the inverse (if it 
      was in the background. If not one of four house colours 
      (red, green, blue, yellow) then default ANSITerminal style is returned. *)
  let inverse_colour (colour:ANSITerminal.style) : ANSITerminal.style list =
    match colour with
    |ANSITerminal.Foreground(Red) -> [ANSITerminal.white;
                                      ANSITerminal.on_red]
    |ANSITerminal.Foreground(Green) -> [ANSITerminal.white;
                                        ANSITerminal.on_green]
    |ANSITerminal.Foreground(Blue) -> [ANSITerminal.white;
                                       ANSITerminal.on_blue]
    |ANSITerminal.Foreground(Yellow) -> [ANSITerminal.black;
                                         ANSITerminal.on_yellow]
    | _ -> [ANSITerminal.default]
  (*===End Util meMetho===*)
  (*===Start Title Utils===*)
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
  (*===End Title Utils===*)      
  (*===Start State Dependant Methods===*)
  (*TODO: ASCII character card*)
  let print_enemy (enemy: Hogwarts.character_info) (enemy_state: State.t) 
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

  let print_state (caster: State.t) : unit =
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
  let list_cards (caster: State.t) : unit = 
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

  (*===End State Dependant Methods===*)
  (*TODO: ASCII spell card*)
  let print_spell_details (house_name:string) (hogwarts: Hogwarts.t) (
      spell: Hogwarts.spell_info) : unit =
    let house, sp_name = (get_house house_name), (Hogwarts.spell_name spell) in
    ANSITerminal.(print_string [house] 
                    ("Description for "^sp_name^":\n"
                     ^(Hogwarts.spell_description hogwarts sp_name)))

  let print_post_condition (house_name: string) (condition:Model.end_state) 
    : unit =
    let house = get_house house_name in
    match condition with
    | Win -> ANSITerminal.(print_string [Bold; cyan] "\n\n-=-=-=-=-=-=-=-=-";
                           print_string [house] "\nCongrats you win!\n";
                           print_string [Bold; cyan] "-=-=-=-=-=-=-=-=-\n")
    | Loss -> ANSITerminal.(print_string [house] "\nYou lose :( and die\n")
    | Continue -> ()

  let print_title (anything: unit) : unit =
    ()
end
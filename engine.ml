open Hogwarts
open Command
open State

(**[play game] TODO:*)
let rec play player enemy (house: ANSITerminal.style) (name: string)  =
  (*TODO: print desc once State.t is realized*)
  ANSITerminal.print_string [house] "Enter an action to perform > ";
  let cmd = read_line () in
  try (
    match parse cmd with
    | Draw -> ()
    | Cast lst -> ()
    | Describe lst -> ()
    | View -> ()
    | Instruction -> ()
    | Forfeit -> () )
  with Invalidcommand ->
    ANSITerminal.(print_string [house] "Invalid command. Possible commands: \n
    Draw, cast [card name], describe [card name], view, instruction,
    forfeit"); play player enemy house name 

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
    ANSITerminal.(
      print_string [house] ("Welcome " ^ player_name ^ "!")
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

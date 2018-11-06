exception InvalidInput of string

module Menu = Model.MakeMenu
module View = View.Make
module Game = Model.MakeGame (View)

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
  if pred input then
    input
  else
    raise (InvalidInput input)

let get_name _ : string =
  View.print_formatted_lst "" 
    ["Welcome! Welcome!";
     "I lost my list of names... So remind me who are you? "];
  let reg = Str.regexp "^[A-Za-z]+[A-Za-z ]*$" in 
  let rec parse_name _ = 
    View.print_cmd_input "" "Enter your name";
    try (
      parse_input ~pred:(fun s -> Str.string_match reg s 0) ()
    ) with InvalidInput input -> (
        View.print_formatted_lst "" 
          ["Simple stuff,";
           "I wonder how you will fare in Hogwarts if you struggle at even this...";
           "Try again"];
        parse_name ()
      ) in parse_name ()

let get_house _ : string =
  View.print_formatted_lst "" ["Also the sorting hat is out for lunch";
                               "So you'll need to choose your own house.";
                               "As a reminder the houses are: "];
  View.print_formatted_lst "gryffindor" ["Gryffindor "];
  View.print_formatted_lst "slytherin" ["Slytherin"];
  View.print_formatted_lst "ravenclaw" ["Ravenclaw "];
  View.print_formatted_lst "hufflepuff" ["Hufflepuff "];
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

let get_files (_:unit) : (string*string)=
  View.print_cmd_input "" "Enter name of spells file";
  let spells = parse_input () in
  View.print_cmd_input "" "Enter name of characters file";
  let chars = parse_input () in
  (spells,chars)

let rec init_game (_:unit) =
  View.print_title ();
  let files = get_files () in
  try (
    let hogwarts = Menu.play_init (fst files) (snd files) in
    let name = get_name () in
    let house = get_house () in
    let player = Menu.create_player hogwarts name house in ()

  ) with _ -> (
      View.print_formatted_lst "" ["Files were invalid. Try again."];
      init_game ()
    )

(**/**)
let () = init_game ()
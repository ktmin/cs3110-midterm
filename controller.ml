(*TODO: move all descriptions to mli file*)

exception InvalidInput of string

(**[default_condition str] is the default predicate for input parsing and 
   returns true no matter what is provided for [str]. *)
let default_condition (str:string) : bool =
  true

(**[parse_input ?pred] takes in an optional predicate [pred] and returns the 
   input string from the user if it passes the predicate (by default all strings 
   are valid). If predicate returns false then [InvalidInput] exception is 
   raised.*)
let parse_input ?pred:(pred=default_condition) : string =
  let input = read_line () in
  if pred input then
    input
  else
    raise (InvalidInput input)

(**/**)
let () = ()
open Hogwarts

type command = 
  | Draw 
  | Cast of string list 
  | Describe of string list
  | View 
  | Instruction  
  | Forfeit
  | Status 
  | Help


exception Invalidcommand

(**removes all blanks from a list*)
let rec no_blank_elem list = 
  match list with 
  | [] -> []
  | h :: t -> if ((h = "")) then 
      no_blank_elem t else 
      h :: no_blank_elem t

let parse_list list = 
  match list with
  | [] -> raise Invalidcommand
  | [""] -> raise Invalidcommand 
  | h::t ->
    if ((h="draw") && (t = [])) then Draw else

    if ((h ="cast") && (t != [])) then Cast (no_blank_elem t) else

    if (h="describe" && (t != [])) then Describe (no_blank_elem t) else

    if (h ="view" && (t = [])) then View else 

    if (h ="instruction" && (t = [])) then Instruction else     

    if ((h = "forfeit") && (t = [])) then Forfeit else

    if ((h = "help") && (t = [])) then Help else

    if ((h = "status") && (t = [])) then Status else
      raise Invalidcommand 

(**prase a string and outputs its corresponding command.
   Outputs an exception Invalidcommand if a command is malfored*)
let parse str = 
  let trim_str = String.trim str in 
  let string = String.lowercase_ascii(trim_str) in   
  let split_list = String.split_on_char ' ' string in
  parse_list split_list


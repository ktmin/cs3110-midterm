(**
   Parsing of player commands.
*)

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

val parse: string -> command

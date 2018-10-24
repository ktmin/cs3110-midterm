(**
   Parsing of player commands.
*)

type command = 
  | Draw of string list
  | Cast of string list 
  | Describe of string list
  | View 
  | Instruction  
  | Forfeit

exception Invalidcommand

val parse: string -> command

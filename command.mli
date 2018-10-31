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

(**prase a string and outputs its corresponding command.
   Outputs an exception Invalidcommand if a command is malfored*)
val parse: string -> command

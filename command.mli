(**
   Parsing of player commands.
*)

(** [command] is the representation of command. *)
type command = 
  | Draw
  | Cast of string list 
  | Describe of string list
  | View 
  | Instruction  
  | Forfeit
  | Status 
  | Help 

(** Thrown in [parse] when it cannot locate a valid command. *)
exception Invalidcommand


(**[parse string] prases a string and outputs its corresponding
   command. Outputs an exception Invalidcommand if a command is malfored. *)
val parse: string -> command

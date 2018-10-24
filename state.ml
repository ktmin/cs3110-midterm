(* open Hogwarts *)

type player_name = string


type player = {
  name : player_name;
  hp: int;
}

type hand = Hogwarts.spell_info list

type deck = Hogwarts.spell_info list

type t = {
  player: player;
  hand: hand;
  deck: deck;
}

let get_hp player = 
  player.player.hp

let init_player hogwarts name =
  {player={name=name; hp=100}; hand=[]; deck=(Hogwarts.get_spells hogwarts)}

let init_enemy hogwarts =
  {player={name="Malfoy"; hp=100}; hand=[]; deck=(Hogwarts.get_spells hogwarts)}

(** update hand and deck
    returns a tuple 
    of updated hand and 
    deck*)
let draw (pl:t) =
  match pl.deck with
  | [] -> pl
  | h::t -> {pl with hand=(h::pl.hand); deck=t}

let get_hand (pl:t) : hand =
  pl.hand

let get_deck (pl:t) : deck =
  pl.deck

let update hogwarts spell st = 
  let spell' = Hogwarts.search hogwarts spell in 
  let damage = st.player.hp - (Hogwarts.spell_damage hogwarts spell) in
  let updated_hand = List.filter (fun x -> x <> spell') st.hand in 
  let updated_player = {st.player with hp = damage} in 
  {st with player = updated_player; hand = updated_hand }

let cast hogwarts spell st1 st2 =
  let spell_name = Hogwarts.spell_name spell in 
  if Hogwarts.spell_target spell = "self" then update hogwarts spell_name st1 
  else update hogwarts spell_name st2


(** returns hp after the spell is casted*)
(* let casted hogwarts spell st = 
   let new_hp = st.hp - Hogwarts.spell_damage hogwarts spell in 
   {st with hp= new_hp } *)

(*TODO: remove this*)
let to_list_hand pl : Hogwarts.spell_info list =
  pl.hand


(*Keeping this for later when we return to module method*)
(* module type Command = sig
   type player_name

   type player_hp

   type player

   type hand

   type deck

   val get_name: player -> player_name

   val get_hp: player -> player_hp 

   val get_hand: hand -> Hogwarts.spell_info list

   val draw: Hogwarts.spell_info list ->
    hand -> deck -> Hogwarts.spell_info list * Hogwarts.spell_info list

   val cast : 'a -> Hogwarts.spell_info -> hand -> 'a * Hogwarts.spell_info list

   end 

   module Command: Command = struct  
   type player_name = string
   type player_hp = int


   type player = {
    name : player_name;
    hp: player_hp;
   }

   type hand = {
    hand : spell_info list;
   }

   type deck = {
    deck : spell_info list;
   }

   let get_name st =
    st.name

   let get_hp st = 
    st.hp 

   let get_hand st =
    st.hand

   (** update hand and deck
      returns a tuple 
      of updated hand and 
      deck*)
   let draw chosen st1 st2=
    (st1.hand @ chosen, 
     match st2.deck with
     | [] -> []
     | h :: t -> t  
    ) 

   let cast damage chosen st =    
    (damage,List.filter (fun x -> x <> chosen) st.hand)

   (** returns hp after the spell is casted*)
   let casted damage st = 
    st.hp - damage  

   end  *)






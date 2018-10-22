open Yojson.Basic.Util


type location_id = int

type location = {id: location_id; name: string; description: string; color: string; 
                 owned: bool; price: int}

type t = {start_location: location_id; locations: location list}


(** [create_locations j] is a record representation of the json information 
    for each location. *)
let create_locations j = 
  {
    id = j |> member "id" |> to_int;
    name = j |> member "name" |> to_string;
    description = j |> member "description" |> to_string;
    color = j |> member "color" |> to_string;
    owned = j |> member "owned" |> to_bool;
    price = j |> member "price" |> to_int
  }


let from_json j = 
  {
    start_location = j |> member "start" |> to_int;
    locations = (j |> member "locations" |> to_list |> List.map create_locations)
  }
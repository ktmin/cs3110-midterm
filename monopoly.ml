open Yojson.Basic.Util


type location = {id: int; name: string; description: string; color: string; 
                 owned: bool; price: int}

type t = {start_location: int; locations: location list}


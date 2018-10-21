open Yojson.Basic.Util


type location = {id: int; name: string; color: string; price: int}

type t = {start_location: int; locations: location list}
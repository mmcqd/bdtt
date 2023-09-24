[@@@warning "-39"] (* the preprocessor sometimes generates useless 'rec' *)

open Earley_core

let keywords = Hashtbl.of_seq @@ List.to_seq [
  ("type",());
  ("nat",());
  ("def",());
  ("zero",());
  ("suc",());
  ("rec",())
]

let parser raw_name = ''[a-zA-Z_][0-9a-zA-Z+_-]*''

let _name = Earley.(no_blank_layout @@ greedy @@ raw_name)

let name = _name |> Earley.apply @@ fun token ->
  match Hashtbl.find_opt keywords token with
  | Some _ -> Earley.give_up ()
  | None -> token


let type_ = Earley.greedy @@ parser { "type" } -> ()
let nat = Earley.greedy @@ parser { "nat" } -> ()
let zero = Earley.greedy @@ parser { "zero" } -> ()
let suc = Earley.greedy @@ parser { "suc" } -> ()
let rec_ = Earley.greedy @@ parser { "rec" } -> ()
let def = Earley.greedy @@ parser { "def" } -> ()
let assign = Earley.greedy @@ parser { ":=" } -> ()
let colon = Earley.greedy @@ parser { ":" } -> ()
let double_colon = Earley.greedy @@ parser { "::" } -> ()
let maps_to = Earley.greedy @@ parser { "=>" } -> ()
let arrow = Earley.greedy @@ parser { "->" } -> ()
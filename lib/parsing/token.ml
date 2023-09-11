[@@@warning "-39"] (* the preprocessor sometimes generates useless 'rec' *)

open Earley_core



let parser raw_name = ''[a-zA-Z_][0-9a-zA-Z+_-]*''
let name = Earley.(no_blank_layout @@ greedy @@ raw_name)


let def = Earley.greedy @@ parser { "def" } -> ()
let assign = Earley.greedy @@ parser { ":=" } -> ()
let colon = Earley.greedy @@ parser { ":" } -> ()
let double_colon = Earley.greedy @@ parser { "::" } -> ()
let maps_to = Earley.greedy @@ parser { "=>" } -> ()
let arrow = Earley.greedy @@ parser { "->" } -> ()
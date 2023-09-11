open Earley_core


let () = Earley.warn_merge := false
let parse_string s = Earley.parse_string Grammar.program Blanks.default s
let parse_file f = Earley.parse_file Grammar.program Blanks.default f

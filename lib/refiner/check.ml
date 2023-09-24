
module S = Nbe.Syntax
module D = Nbe.Domain

type goal = D.t
type result = S.t
type t = goal -> result

let rule ?id (t : D.tp -> S.t) goal =
 (match id with
    | Some _ -> ()
    | None -> ()); t goal
let run t goal = t goal
let run_tp t = t D.Univ

let peek (t : D.tp -> t) : t = rule @@ fun goal ->
  run (t goal) goal

let infer (inf : Infer.t) : t = rule @@ fun goal ->
  let tp,tm = Infer.run inf () in
  try Eff.equate_tp tp goal; tm with Nbe.Conv.ConversionError -> 
  let goal = Eff.quote_tp goal in
  let tp = Eff.quote_tp tp in
  print_endline (Logger.show_span_opt (Logger.Locate.read ()));
  Logger.fatal_string `Conversion (Format.sprintf "Needed a term of type %s but got a term of type %s" (S.show goal) (S.show tp))


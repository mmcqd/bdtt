
module S = Nbe.Syntax
module D = Nbe.Domain

type goal = D.t
type result = S.t
type t = goal -> result

let rule (t : D.tp -> S.t) (goal : D.t) : S.t = t goal
let run t goal = t goal
let run_tp t = t D.Univ


let infer (inf : Infer.t) : t =
  fun goal ->
  let tp,tm = Infer.run inf () in
  try Eff.equate_tp tp goal; tm with
  | Nbe.Conv.ConversionError ->
    let goal = Eff.quote_tp goal in
    let tp = Eff.quote_tp tp in
    failwith (Printf.sprintf "Needed a term of type %s but got a term of type %s" (S.show goal) (S.show tp))



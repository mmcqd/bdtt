
module S = Nbe.Syntax
module D = Nbe.Domain

type t = D.t -> S.t

let rule (t : D.tp -> S._t) (goal : D.t) : S.t = Nbe.Data.{data = t goal ; tp = goal.data}
let run t goal = t goal
let run_tp t = t Nbe.Data.{data = D.Univ ; tp = D.Univ}


let infer (inf : Infer.t) : t =
  fun goal ->
  let tm = Infer.run inf () in
  let tp' = Nbe.Data.{data = tm.tp ; tp = D.Univ} in
  try Eff.equate tp' goal; tm with
  | Nbe.Conv.ConversionError ->
    let tp = Eff.quote goal in
    let tp' = Eff.quote tp' in
    failwith (Printf.sprintf "Needed a term of type %s but got a term of type %s" (S.show tp) (S.show tp'))



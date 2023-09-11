
module S = Nbe.Syntax
module D = Nbe.Domain

type goal = unit
type result = S.t

type t = goal -> result

let rule t = t
let run t goal = t goal
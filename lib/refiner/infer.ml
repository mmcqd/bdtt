open Nbe

type goal = unit
type result = Domain.t * Syntax.t

type t = goal -> result

let rule t = t
let run t goal = t goal
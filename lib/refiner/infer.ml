open Nbe

type goal = unit
type result = Domain.t * Syntax.t

type t = goal -> result

let rule ?id t goal = 
  (match id with Some _ -> () | None -> ()); t goal
let run t goal = t goal
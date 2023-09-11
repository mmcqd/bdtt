

type t = {check: Check.t Lazy.t; infer: Infer.t Lazy.t}

let run_check {check; _} goal = Check.run (Lazy.force check) goal
let run_infer {infer; _} goal = Infer.run (Lazy.force infer) goal
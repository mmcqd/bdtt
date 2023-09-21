open Nbe

type t


type goal = Domain.t
type result = Syntax.t

val rule : (goal -> result) -> t
val run : t -> (goal -> result)
val run_tp : t -> Syntax.t

val infer : Infer.t -> t
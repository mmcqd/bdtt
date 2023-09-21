open Nbe

type t

type goal = unit
type result = Domain.t * Syntax.t

val rule : (goal -> result) -> t
val run : t -> goal -> result

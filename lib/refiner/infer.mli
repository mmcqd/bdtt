

type t

type goal = unit
type result = Nbe.Syntax.t

val rule : (goal -> result) -> t
val run : t -> goal -> result

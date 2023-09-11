

type t



val rule : (Nbe.Domain.t -> Nbe.Syntax._t) -> t
val run : t -> Nbe.Domain.t -> Nbe.Syntax.t
val run_tp : t -> Nbe.Syntax.t

val infer : Infer.t -> t
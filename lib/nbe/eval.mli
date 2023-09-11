
val eval_in_env : Domain.env -> Syntax.t -> Domain.t
val elim_clo : Domain.clo -> Domain.t list -> Domain.t
val eval_top : Domain.globals -> Syntax.t -> Domain.t
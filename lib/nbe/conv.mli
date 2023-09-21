
exception ConversionError

val equate_in_env : Domain.env -> Domain.tp -> Domain.t -> Domain.t -> unit
val equate_top : Domain.tp -> Domain.t -> Domain.t -> unit
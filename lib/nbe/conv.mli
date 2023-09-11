
exception ConversionError

val equate_in_env : Domain.env -> Domain.t -> Domain.t -> unit
val equate_top : Domain.t -> Domain.t -> unit
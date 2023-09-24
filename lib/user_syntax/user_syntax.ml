


type 'a located = 'a Asai.Span.located = {
  loc : Asai.Span.t option; [@opaque] 
  value : 'a
}
[@@deriving show]
  

type cmd = _cmd located
and _cmd = 
  | Def of {name: string; tm: term; tp: term option}

and term = _term located
and _term =
  | Univ
  | Var of string
  | Pi of term * string * term
  | Lam of string list * term
  | App of term * term
  | Ann of {tm : term; tp : term}
  | Nat
  | Zero
  | Suc of term
  | Rec of {name : string; zero : term; suc : string * term}
[@@deriving show]

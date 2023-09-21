

type t =
  | Univ
  | Idx of int
  | Def of string
  | Pi of tp * string * tp
  | Lam of string * t
  | App of t * t
  | Nat
  | Zero
  | Suc of t
  | Rec of {name : string ; mot : string * tp ; scrut : t;  zero : t; suc : string * t}
  | RecableType of string

and tp = t
  [@@deriving show {with_path = false}]
  
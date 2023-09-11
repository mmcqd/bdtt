

type _t = Data._syn =
  | Univ
  | Idx of int
  | Def of string
  | Pi of tp * string * tp
  | Lam of string * t
  | App of t * t
and t = Data.syn

and tp = t
  [@@deriving show {with_path = false}]
  
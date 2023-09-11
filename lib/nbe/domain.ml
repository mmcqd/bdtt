[@@@warning "-30"]
open Bwd

let pp_bwd pp_elem fmt bwd = Format.pp_print_list pp_elem fmt (Bwd.to_list bwd)

type _t = Data._dom =
  | Univ
  | Pi of tp * string * clo
  | Lam of string * clo
  | Neu of {hd: head; sp: spine}

and t = Data.dom

and _head = Data._head = 
  | Lvl of int

and head = Data.head

and spine = Data.spine

and elim = Data.elim =
  | App of t

and clo = Data.clo = {env : env; body : Syntax.t}

and locals = t bwd

and globals = (string * t) bwd

and env = Data.env = {locals: locals; globals: globals}

and tp = t

[@@deriving show {with_path = false}]


let lvl tp l = Data.{data = Neu {hd = {data = Lvl l ; tp = tp.data} ; sp = Bwd.Emp} ; tp = tp.data}

let empty_env = {locals = Bwd.Emp; globals = Bwd.Emp}
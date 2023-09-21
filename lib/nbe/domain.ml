[@@@warning "-30"]
open Bwd

let pp_bwd pp_elem fmt bwd = Format.pp_print_list pp_elem fmt (Bwd.to_list bwd)

(* type _ t =
  | Univ : [`Type] t
  | Pi : [`Type] t * string * [`Type] clo -> [`Type] t
  | Lam : string * 'a clo -> [`Term] t
  | Neu : {hd: head; sp: spine; tp: [`Type] t} -> [< | `Term | `Type] t
  | Nat : [`Type] t
  | Zero : [`Term] t
  | Suc : [`Term] t -> [`Term] t
  | RecableType : string -> [`Type] t

and head = 
  | Lvl of int
  

and _ clo = |

and head = |
and spine = | *)


type t =
  | Univ
  | Pi of tp * string * clo
  | Lam of string * clo
  | Neu of {hd: head; sp: spine; tp: tp}
  | Nat
  | Zero
  | Suc of t
  | RecableType of string


and head =
  | Lvl of int


and spine = elim bwd

and elim =
  | App of cell
  | Rec of {name : string ; mot : string * clo ; zero : t; suc : string * clo}

and clo = {env : env; body : Syntax.t}

and locals = t bwd

and globals = (string * cell) bwd

and env = {locals: locals; globals: globals}

and tp = t

and cell = {tm : t; tp : tp}

[@@deriving show {with_path = false}]


let lvl tp l = Neu {hd = Lvl l ; sp = Bwd.Emp; tp = tp}

let empty_env = {locals = Bwd.Emp; globals = Bwd.Emp}
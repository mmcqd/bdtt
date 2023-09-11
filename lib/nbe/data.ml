[@@@warning "-30"]
open Bwd


let pp_bwd pp_elem fmt bwd = Format.pp_print_list pp_elem fmt (Bwd.to_list bwd)

type _syn =
  | Univ
  | Idx of int
  | Def of string
  | Pi of syn_tp * string * syn_tp
  | Lam of string * syn
  | App of syn * syn
and syn = _syn annot

and syn_tp = syn

(* and syn_pi_mode = 
  | Check of syn_tp
  | Infer of (string * dom_tp) list * syn_pattern
  [@@deriving show {with_path = false}] *)
  

and _dom =
  | Univ
  | Pi of dom_tp * string * clo
  | Lam of string * clo
  | Neu of {hd: head; sp: spine}
and dom = _dom annot

(* and dom_pi_mode = 
  | Check of dom_tp
  | Infer of (string * dom_tp) list * clo *)

and _head = 
  | Lvl of int
and head = _head annot

and spine = (elim * _dom) bwd

and elim =
  | App of dom

and clo = {env : env; body : syn}

and locals = dom bwd

and globals = (string * dom) bwd

and env = {locals: locals; globals: globals}

and dom_tp = dom

and 'a annot = {data : 'a; tp : _dom}

[@@deriving show {with_path = false}]



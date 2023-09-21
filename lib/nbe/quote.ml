open Bwd

module S = Syntax
module D = Domain

module Eff = Algaeff.Reader.Make (struct type env = int end)

let bind tps f =
  let l = Eff.read () in
  let args = List.mapi (fun i tp -> D.lvl tp (l + i)) tps in
  Eff.scope (fun l -> l + List.length args) @@ fun () ->
  f args

let rec quote (tp : D.tp) (tm : D.t): S.t =
  match tp, tm with
  | D.Univ, D.Univ -> S.Univ
  | D.Univ, D.Pi (base, id, fam) ->
    let qbase = quote_tp base in
    let fam = bind [base] @@ fun arg -> quote_tp (Eval.elim_clo fam arg) in
    S.Pi (qbase, id, fam)
  | D.Pi (base, _, fam), D.Lam (id, body) -> 
    bind [base] @@ fun arg -> 
    let body_tp = Eval.elim_clo fam arg in
    let body = Eval.elim_clo body arg in
    S.Lam (id, quote body_tp body)
  | D.Univ, D.Nat -> S.Nat
  | D.Nat, D.Zero -> S.Zero
  | D.Nat, D.Suc n -> S.Suc (quote D.Nat n)
  | _, D.Neu {hd; sp; _} -> quote_neutral hd sp
  | _ -> failwith "ill typed quote"

and quote_neutral hd sp = BwdLabels.fold_left sp ~f:quote_elim ~init:(quote_hd hd)
  
and quote_hd = function
  | D.Lvl l -> S.Idx (Eff.read() - l - 1)

and quote_elim hd = function
  | D.App {tm ; tp} -> S.App (hd, quote tp tm)
  | D.Rec _ -> failwith ""

and quote_tp tm = quote D.Univ tm

let quote_in_env env tm = Eff.run ~env:(Bwd.length env.D.locals) @@ fun () -> quote tm
let quote_top = quote_in_env D.empty_env


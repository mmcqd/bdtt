open Bwd

module S = Syntax
module D = Domain

module Eff = Algaeff.Reader.Make (struct type env = int end)
let () = Eff.register_printer @@ fun `Read -> Some "Unhandled Quote Read Effect"

let bind tps f =
  let l = Eff.read () in
  let args = List.mapi (fun i tp -> D.lvl tp (l + i)) tps in
  Eff.scope (fun l -> l + List.length args) @@ fun () ->
  f args

let rec quote (tp : D.tp) (tm : D.t): S.t =
  Logger.tracef "While quoting %a : %a" D.pp tm D.pp tp @@ fun () ->
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
  | D.Univ, D.RecableType id -> S.RecableType id
  | D.Nat, D.Zero -> S.Zero
  | D.Nat, D.Suc n -> S.Suc (quote D.Nat n)
  | _, D.Neu {hd; sp; _} -> quote_neutral hd sp
  | _ ->
    Format.printf "BAD QUOTE %a : %a\n" D.pp tm D.pp tp;
    Logger.fatalf `InternalError  "BAD QUOTE %a : %a" D.pp tm D.pp tp

and quote_neutral hd sp = BwdLabels.fold_left sp ~f:quote_elim ~init:(quote_hd hd)
  
and quote_hd = function
  | D.Lvl l -> S.Idx (Eff.read() - l - 1)

and quote_elim hd = function
  | D.App {tm ; tp} -> S.App (hd, quote tp tm)
  | D.Rec {name; mot = (m,mot); zero; suc = (n,suc)} -> 
    let qmot = bind [D.Nat] @@ fun m -> quote D.Univ (Eval.elim_clo mot m) in
    let zero = quote (Eval.elim_clo mot [D.Zero]) zero in
    bind [D.Pi (D.Nat,"_",mot)] @@ fun self ->
    let suc = bind [D.Nat] @@ fun n -> quote (Eval.elim_clo mot n) (Eval.elim_clo suc (self @ n)) in
    S.Rec {name ; mot = (m,qmot); zero; suc = (n,suc) ; scrut = hd}

and quote_tp tm = quote D.Univ tm

let quote_in_env env tp tm = Eff.run ~env:(Bwd.length env.D.locals) @@ fun () -> quote tp tm
let quote_top = quote_in_env D.empty_env


open Bwd

module S = Syntax
module D = Domain

module Eff = Algaeff.Reader.Make (struct type env = int end)

let bind tps f =
  let l = Eff.read () in
  let args = List.map (fun tp -> D.lvl tp l) tps in
  Eff.scope (fun l -> l + List.length args) @@ fun () ->
  f args

let rec quote (tm : D.t) : S.t =
  match tm.tp, tm.data with
  | D.Univ, D.Univ  -> {data = S.Univ; tp = tm.tp}
  | D.Univ, D.Pi (base, id, fam) ->
    let qbase = quote base in
    let fam = bind [base] @@ fun arg -> quote (Eval.elim_clo fam arg) in
    {data = S.Pi (qbase, id, fam) ; tp = tm.tp}
  | D.Pi (base, _, _), D.Lam (id, body) -> 
    bind [base] @@ fun arg -> 
    let body = Eval.elim_clo body arg in
    Data.{data = S.Lam (id, quote body) ; tp = tm.tp}
  | _, D.Neu {hd; sp; _} -> quote_neutral hd sp
  | _ -> failwith "ill typed quote"

and quote_neutral hd sp = BwdLabels.fold_left sp ~f:quote_elim ~init:(quote_hd hd)
  
and quote_hd hd =
  match hd.data with
  | D.Lvl l -> {data = S.Idx (Eff.read() - l - 1) ; tp = hd.tp}

and quote_elim hd = fun (elim, tp) ->
  match elim with
  | D.App tm -> {data = S.App (hd, quote tm) ; tp}


let quote_in_env env tm = Eff.run ~env:(Bwd.length env.D.locals) @@ fun () -> quote tm
let quote_top = quote_in_env D.empty_env
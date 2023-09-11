open Bwd

module S = Syntax
module D = Domain

type env = int
module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)

exception ConversionError

let bind tps f =
  let l = Eff.read ()in
  let args = List.map (fun tp -> D.lvl tp l) tps in
  Eff.scope ((+) (List.length args)) @@ fun () ->
  f args

let rec equate (tm1 : D.t) (tm2 : D.t) : unit =
  match tm1.tp, tm1.data, tm2.data with
  | D.Univ, D.Univ, D.Univ  -> ()
  | D.Univ, D.Pi (base1, _, fam1), D.Pi (base2, _, fam2) ->
    equate base1 base2;
    bind [base1] @@ fun arg -> equate (Eval.elim_clo fam1 arg) (Eval.elim_clo fam2 arg)
  | D.Pi (base, _, _), D.Lam (_, body1), D.Lam (_, body2) -> 
    bind [base] @@ fun arg ->
    equate (Eval.elim_clo body1 arg) (Eval.elim_clo body2 arg)
  | _, D.Neu neu1, D.Neu neu2 -> equate_neutral neu1.hd neu1.sp neu2.hd neu2.sp
  | _ -> raise ConversionError



and equate_neutral hd1 sp1 hd2 sp2 = 
  equate_hd hd1 hd2;
  BwdLabels.iter2 ~f:equate_elim sp1 sp2
  
and equate_hd hd1 hd2 =
  match hd1.data, hd2.data with
    | D.Lvl l1, D.Lvl l2 -> if Int.equal l1 l2 then () else raise ConversionError

and equate_elim e1 e2 =
  match fst e1, fst e2 with
    | D.App a1, D.App a2 -> equate a1 a2





let equate_in_env env tm1 tm2 = Eff.run ~env:(Bwd.length env.D.locals) @@ fun () -> equate tm1 tm2
let equate_top = equate_in_env D.empty_env
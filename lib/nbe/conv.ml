open Bwd

module S = Syntax
module D = Domain

type env = int
module Eff = Algaeff.Reader.Make (struct type nonrec env = env end)
let () = Eff.register_printer @@ fun `Read -> Some "Unhandled Conversion Read Effect"

exception ConversionError

let bind tps f =
  let l = Eff.read ()in
  let args = List.mapi (fun i tp -> D.lvl tp (l + i)) tps in
  Eff.scope (fun l -> l + List.length args) @@ fun () ->
  f args

let rec equate (tp : D.tp) (tm1 : D.t) (tm2 : D.t) : unit =
  match tp, tm1, tm2 with
  | D.Univ, D.Univ, D.Univ  -> ()
  | D.Univ, D.Pi (base1, _, fam1), D.Pi (base2, _, fam2) ->
    equate_tp base1 base2;
    bind [base1] @@ fun arg -> equate_tp (Eval.elim_clo fam1 arg) (Eval.elim_clo fam2 arg)
  | D.Pi (base, _, fam), D.Lam (_, body1), D.Lam (_, body2) -> 
    bind [base] @@ fun arg ->
    equate (Eval.elim_clo fam arg) (Eval.elim_clo body1 arg) (Eval.elim_clo body2 arg)
  | D.Univ, D.Nat, D.Nat -> ()
  | D.Nat, D.Zero, D.Zero -> ()
  | D.Nat, D.Suc n1, D.Suc n2 -> equate D.Nat n1 n2
  | _, D.Neu neu1, D.Neu neu2 -> equate_neutral neu1.hd neu1.sp neu2.hd neu2.sp
  | _ -> 
    raise ConversionError


and equate_neutral hd1 sp1 hd2 sp2 = 
  equate_hd hd1 hd2;
  BwdLabels.iter2 ~f:equate_elim sp1 sp2
  
and equate_hd hd1 hd2 =
  match hd1, hd2 with
    | D.Lvl l1, D.Lvl l2 -> if Int.equal l1 l2 then () else raise ConversionError
and equate_elim e1 e2 =
  match e1, e2 with
    | D.App a1, D.App a2 -> equate a1.tp a1.tm a2.tm
    | _ -> failwith "TODO"

and equate_tp tp1 tp2 = equate D.Univ tp1 tp2



let equate_in_env env tp tm1 tm2 = Eff.run ~env:(Bwd.length env.D.locals) @@ fun () -> equate tp tm1 tm2
let equate_top = equate_in_env D.empty_env

module S = Nbe.Syntax
module D = Nbe.Domain
module U = User_syntax
open Refiner
module T = Tactic


let rec check (tm : U.term) : Check.t =
  Check.peek @@ fun goal ->
  T.trace_check ?loc:tm.loc "While checking against %a" D.pp goal @@
  match tm.value with 
    | U.Univ -> T.univ
    | U.Pi (base, id, fam) -> T.pi (check base) id (fun _ -> check fam)
    | U.Lam (ids, body) -> T.lam ids (fun _ -> check body)
    | U.Zero -> T.zero
    | U.Suc n -> T.suc (check n)
    | U.Rec {name ; zero; suc = (n,suc)} -> T.rec_ name (check zero) (n,fun _ _ -> check suc)
    | _ -> Check.infer (infer tm)
  
and infer (tm : U.term) : Infer.t =
  T.trace_infer ?loc:tm.loc "While inferring" @@
  match tm.value with
    | U.Var id -> T.var id
    | U.App (f, arg) -> T.app (infer f) (ambi arg)
    | U.Ann {tm ; tp} -> T.annot (check tp) (check tm)
    | U.Nat -> T.nat
    | _ -> T.infer_fail

and ambi tm : Ambi.t = {check = lazy (check tm); infer = lazy (infer tm)}



let check_top globals tm goal =
  Eff.run ~env:{Eff.empty_env with globals} @@ fun () -> 
  Check.run (check tm) goal


let infer_top globals tm goal = Eff.run ~env:{Eff.empty_env with globals} @@ fun () -> Infer.run (infer tm) goal
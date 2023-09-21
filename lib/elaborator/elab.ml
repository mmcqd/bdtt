
module S = Nbe.Syntax
module U = User_syntax
open Refiner
module T = Tactic


let rec check (tm : U.term) : Check.t =
  (* print_endline @@ "checking " ^ U.show_term tm; *)
  match tm.value with 
    | U.Univ -> T.univ
    | U.Pi (base, id, fam) -> T.pi (check base) id (fun _ -> check fam)
    | U.Lam (ids, body) -> T.lam ids (fun _ -> check body)
    | U.Zero -> T.zero
    | U.Suc n -> T.suc (check n)
    | _ -> Check.infer (infer tm)
  
and infer (tm : U.term) : Infer.t =
  (* print_endline @@ "infering " ^ U.show_term tm; *)
  match tm.value with
    | U.Var id -> T.var id
    | U.App (f, arg) -> T.app (infer f) (ambi arg)
    | U.Ann {tm ; tp} -> T.annot (check tp) (check tm)
    | U.Nat -> T.nat
    | _ -> failwith "infer"

and ambi tm : Ambi.t = {check = lazy (check tm); infer = lazy (infer tm)}



let check_top globals tm goal = Eff.run ~env:{Eff.empty_env with globals} @@ fun () -> Check.run (check tm) goal
let infer_top globals tm goal = Eff.run ~env:{Eff.empty_env with globals} @@ fun () -> Infer.run (infer tm) goal
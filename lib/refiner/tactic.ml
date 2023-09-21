(* open Bwd *)

module S = Nbe.Syntax
module D = Nbe.Domain

type 'a bind = D.t -> 'a
type 'a multi_bind = D.t list -> 'a



let univ : Check.t = Check.rule @@ function
  | D.Univ -> S.Univ
  | _ -> failwith "univ"


exception UnboundVar of string

let var id : Infer.t = Infer.rule @@ fun () ->
  (* print_endline "tac_var"; *)
  match Eff.lookup_local id with
    | Some tm -> tm.tp, Eff.quote tm.tp tm.tm
    | None -> 
      match Eff.lookup_global id with
        | Some tm -> tm.tp, Eff.quote tm.tp tm.tm
        | None -> raise @@ UnboundVar id

let pi (base : Check.t) id (fam : Check.t bind) : Check.t = Check.rule @@ function
  | D.Univ -> 
    let base = Check.run_tp base in
    let fam = Eff.bind ~id ~tp:(Eff.eval base) @@ fun base -> Check.run_tp (fam base) in
    S.Pi (base, id, fam)
  | _ -> failwith "pi_check"

let rec lam ids (body : Check.t bind) : Check.t = Check.rule @@ function
    | D.Pi (base, _, fam) ->
      begin 
      match ids with
        | id :: (_ :: _ as ids) -> 
          Eff.bind ~id ~tp:base @@ fun x ->
          S.Lam (id, Check.run (lam ids body) (Eff.elim_clo fam [x]))
        | [ id ] -> 
          Eff.bind ~id ~tp:base @@ fun x ->
          S.Lam (id, Check.run (body x) (Eff.elim_clo fam [x]))
        | [] -> failwith "impossible"
      end
    | _ -> failwith "lam" 


let app (fn : Infer.t) (arg : Ambi.t) : Infer.t = Infer.rule @@ fun () ->
  match Infer.run fn () with
    | D.Pi (base,_, fam), fn -> 
      let arg = Ambi.run_check arg base in
      let tp = Eff.elim_clo fam [Eff.eval arg] in
      tp, S.App (fn, arg)
    | _ -> failwith "app"

let annot (tp : Check.t) (tm : Check.t) : Infer.t = Infer.rule @@ fun () ->
  let tp = Eff.eval @@ Check.run_tp tp in
  tp, Check.run tm tp

let nat : Infer.t = Infer.rule @@ fun () ->
  D.Univ, S.Nat

let zero : Check.t = Check.rule @@ function
    | D.Nat -> S.Zero
    | _ -> failwith "zero"

let suc (n : Check.t) : Check.t = Check.rule @@ function
    | D.Nat -> S.Suc (Check.run n D.Nat)
    | _ -> failwith "suc"




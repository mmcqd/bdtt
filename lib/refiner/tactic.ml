(* open Bwd *)

module S = Nbe.Syntax
module D = Nbe.Domain

type 'a bind = D.t -> 'a
type 'a multi_bind = D.t list -> 'a



let univ : Check.t = Check.rule ~id:"UNIV" @@ function
  | D.Univ -> S.Univ
  | goal -> Logger.fatalf `Elaboration "Expected element of %a but got type" D.pp goal


let var id : Infer.t = Infer.rule ~id:"VAR" @@ fun () ->
  match Eff.lookup_local id with
    | Some tm ->   
      let x = tm.tp, Eff.quote tm.tp tm.tm in
      x
    | None -> 
      match Eff.lookup_global id with
        | Some tm -> tm.tp, Eff.quote tm.tp tm.tm
        | None -> 
          Logger.fatalf `NotInScope "Variable %s not in scope" id

let pi (base : Check.t) id (fam : Check.t bind) : Check.t = Check.rule ~id:"PI" @@ function
  | D.Univ -> 
    let base = Check.run_tp base in
    let fam = Eff.bind ~id ~tp:(Eff.eval base) @@ fun base -> Check.run_tp (fam base) in
    S.Pi (base, id, fam)
  | goal -> Logger.fatalf `Elaboration "Expected element of %a but got Pi" D.pp goal

let rec lam ids (body : Check.t bind) : Check.t = Check.rule ~id:"LAM" @@ function
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
    | goal -> Logger.fatalf `Elaboration "Expected element of %a but got Lam" D.pp goal


let app (fn : Infer.t) (arg : Ambi.t) : Infer.t = Infer.rule ~id:"APP" @@ fun () ->
  match Infer.run fn () with
    | D.Pi (base,_, fam), fn -> 
      let arg = Ambi.run_check arg base in
      let tp = Eff.elim_clo fam [Eff.eval arg] in
      tp, S.App (fn, arg)
    | inf,_ -> Logger.fatalf `Elaboration "Trying to eliminate element of %a but expected Pi" D.pp inf

let annot (tp : Check.t) (tm : Check.t) : Infer.t = Infer.rule ~id:"ANNOT" @@ fun () ->
  let tp = Eff.eval @@ Check.run_tp tp in
  tp, Check.run tm tp

let nat : Infer.t = Infer.rule @@ fun () ->
  D.Univ, S.Nat

let zero : Check.t = Check.rule ~id:"ZERO" @@ function
    | D.Nat -> S.Zero
    | goal -> Logger.fatalf `Elaboration "Expected element of %a but got Zero" D.pp goal

let suc (n : Check.t) : Check.t = Check.rule ~id:"SUC" @@ function
    | D.Nat -> 
      S.Suc (Check.run n D.Nat)
    | goal -> 
      Logger.fatalf `Elaboration "Expected element of %a but got Suc" D.pp goal



let rec_ (id : string) (zero : Check.t) ((x,suc): string * Check.t bind bind) : Check.t = Check.rule ~id:"REC" @@ function
  | D.Pi (D.Nat, mx, fam) as goal-> 
    Eff.bind ~id:"#n" ~tp:D.Nat @@ fun arg ->
    let qmot = Eff.bind ~id:mx ~tp:(RecableType id) @@ fun w -> Eff.quote D.Univ (Eff.elim_clo fam [w]) in
    let zero, suc =
      Eff.bind ~id ~tp:goal @@ fun self ->
      let zero = Check.run zero (Eff.elim_clo fam [D.Zero]) in 
      let suc = Eff.bind ~id:x ~tp:(D.RecableType id) @@ fun n ->
        let tp = Eff.elim_clo fam [n] in
        let tac = suc n self in
        Check.run tac tp
      in
      zero, suc
    in
    S.Lam ("#n", S.Rec {name = id; mot = (mx,qmot); scrut = Eff.quote D.Nat arg; zero; suc = (x,suc)})
    | goal -> Logger.fatalf `Elaboration "Expected element of %a but got Rec" D.pp goal


module type TacKind = sig
  type t
  type goal
  type result
  val run : t -> goal -> result
  val rule : ?id:string -> (goal -> result) -> t
end

let trace (type t) (module T : TacKind with type t = t) ?loc : ('a, Format.formatter, unit, T.t -> T.t) format4 -> 'a =
  Asai.Diagnostic.kmessagef ?loc @@ fun msg tac -> 
    T.rule ~id:"TRACE" @@ fun goal ->
      Logger.Locate.scope (fun _ -> loc) @@ fun () ->
      Logger.trace_message msg @@ fun () ->
        T.run tac goal

let trace_check ?loc = trace (module Check) ?loc
let trace_infer ?loc = trace (module Infer) ?loc

let infer_fail : Infer.t = Infer.rule @@ fun () -> 
  Logger.fatalf `NotInferable "Failed to infer a type for this term, try providing a type annotation"

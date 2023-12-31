open Bwd
open Bwd.Infix

module S = Syntax
module D = Domain

module Eff = Algaeff.Reader.Make (struct type env = D.env end)
let () = Eff.register_printer @@ fun `Read -> Some "Unhandled Evaluation Read Effect"


let clo body = D.{body; env = Eff.read()}
let clo_id (id, body) = (id, D.{body; env = Eff.read()})

let local idx = 
  try Bwd.nth (Eff.read()).locals idx with
    | Failure _ -> Logger.fatalf `InternalError "Local %d not found" idx

let global id = 
  match Bwd.find_opt (fun (id', _) -> String.equal id id') (Eff.read()).globals with
    | Some (_,tm) -> tm.tm
    | None -> Logger.fatalf `InternalError "Global %s not found" id

let rec eval (tm : S.t) : D.t =
  Logger.tracef "While evaluating %a" S.pp tm @@ fun () ->
  match tm with
  | S.Idx idx -> local idx
  | S.Def id -> global id
  | S.Univ -> D.Univ
  | S.Pi (base, id, fam) -> D.Pi (eval base, id, clo fam)
  | S.Lam (id, body) -> D.Lam (id, clo body)
  | S.App (f, arg) -> elim_app (eval f) (eval arg)
  | S.Nat -> D.Nat
  | S.Zero -> D.Zero
  | S.Suc n -> D.Suc (eval n)
  | S.RecableType name -> D.RecableType name
  | S.Rec ({name; mot; scrut; zero; suc} as self) -> elim_rec (eval @@ S.Lam ("#n", S.Rec {self with scrut = S.Idx 0})) name (clo_id mot) (eval scrut) (eval zero) (clo_id suc) 

and elim_app f arg = 
  match f with
    | D.Lam (_, clo) -> elim_clo clo [arg]
    | D.Neu {hd ; sp ; tp = D.Pi (base,_,fam)} -> 
      D.Neu {hd ; sp = sp <: App {tm = arg; tp = base} ; tp = elim_clo fam [arg]}
    | _ -> Logger.fatalf `InternalError "bad app"

and elim_rec self name (x,mot) scrut zero (y,suc) =
  match scrut with
    | D.Zero -> zero
    | D.Suc n -> 
      elim_clo suc [self; n]
    | D.Neu {hd; sp; tp = D.Nat} -> 
      D.Neu {hd; sp = sp <: Rec {name; mot = x, mot; zero; suc = y, suc}; tp = elim_clo mot [scrut]}
    | _ -> Logger.fatalf `InternalError "bad rec on %a" D.pp scrut

and elim_clo D.{body; env} args = 
  let env = {env with locals = env.locals <@ args} in
  Eff.run ~env @@ fun () -> eval body


let eval_in_env env tm = Eff.run ~env @@ fun () -> eval tm
let eval_top globals = eval_in_env {D.empty_env with globals}
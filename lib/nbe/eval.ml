open Bwd
open Bwd.Infix

module S = Syntax
module D = Domain

module Eff = Algaeff.Reader.Make (struct type env = D.env end)

let clo body = D.{body; env = Eff.read()}

let local idx = Bwd.nth (Eff.read()).locals idx
let global id = 
  match Bwd.find_opt (fun (id', _) -> String.equal id id') (Eff.read()).globals with
    | Some (_,tm) -> tm
    | None -> failwith "unbound global"

let rec eval : S.t -> D.t = fun syn ->
  match syn.data with
  | S.Idx idx -> local idx
  | S.Def id -> global id
  | S.Univ -> {data = D.Univ ; tp = syn.tp}
  | S.Pi (base, id, fam) -> {data = D.Pi (eval base, id, clo fam) ; tp = syn.tp}
  | S.Lam (id, body) -> {data = D.Lam (id, clo body) ; tp = syn.tp}
  | S.App (f, arg) -> elim_app (eval f) (eval arg) syn.tp

and elim_app f arg tp = 
  match f.data with
    | D.Lam (_, clo) -> elim_clo clo [arg]
    | D.Neu {hd ; sp} -> 
      D.{data = D.Neu {hd ; sp = sp <: (App arg, tp)} ; tp}
    | _ -> failwith "bad app"

and elim_clo D.{body; env} args = 
  let env = {env with locals = env.locals <@ args} in
  Eff.run ~env @@ fun () -> eval body


let eval_in_env env tm = Eff.run ~env @@ fun () -> eval tm
let eval_top globals = eval_in_env {D.empty_env with globals}
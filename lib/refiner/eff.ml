open Bwd
open Bwd.Infix

module D = Nbe.Domain

type elab_env = {
  locals : D.t bwd;
  local_tps : D.tp bwd;
  local_names : (string * D.cell) bwd;
  globals : (string * D.cell) bwd;
  local_size : int;
}

let empty_env = { locals = Bwd.Emp; local_tps = Bwd.Emp ; local_names = Bwd.Emp ; globals = Bwd.Emp ; local_size = 0 }
module RefinerReader = Algaeff.Reader.Make(struct type nonrec env = elab_env end)

include RefinerReader


let bind ~id ~tp f =
  let env = read () in
  let arg = D.lvl tp env.local_size in
  scope (fun env -> {env with 
    locals = env.locals <: arg; 
    local_tps = env.local_tps <: tp;
    local_names = env.local_names <: (id, {tm = arg ; tp});
    local_size = env.local_size + 1
  }) @@ fun () -> f arg


let lookup_local id = (read ()).local_names |> Bwd.find_opt (fun (id', _) -> String.equal id id') |> Option.map snd
let lookup_global id = (read ()).globals |> Bwd.find_opt (fun (id', _) -> String.equal id id') |> Option.map snd


let nbe_env () =
  let env = read () in
  D.{locals = env.locals; globals = env.globals}

let eval tm =
  Nbe.Eval.eval_in_env (nbe_env ()) tm

let elim_clo = Nbe.Eval.elim_clo 

let quote tp tm =
  Nbe.Quote.quote_in_env (nbe_env ()) tp tm

let quote_tp tp =
  Nbe.Quote.quote_in_env (nbe_env ()) D.Univ tp

let equate tp tm1 tm2 =
  Nbe.Conv.equate_in_env (nbe_env ()) tp tm1 tm2

let equate_tp tp1 tp2 =
  Nbe.Conv.equate_in_env (nbe_env ()) D.Univ tp1 tp2

open Bwd
open Bwd.Infix

module D = Nbe.Domain

type elab_env = {
  locals : D.t bwd;
  local_tps : D.tp bwd;
  local_names : (string * D.t) bwd;
  globals : (string * D.t) bwd;
  local_size : int;
  pattern_vars : (string, D.tp) Hashtbl.t option;
}

let empty_env = { locals = Bwd.Emp; local_tps = Bwd.Emp ; local_names = Bwd.Emp ; globals = Bwd.Emp ; local_size = 0 ; pattern_vars = None }

include Algaeff.Reader.Make(struct type nonrec env = elab_env end)

let in_pattern () =
  let env = read () in
  Option.is_some env.pattern_vars

let bind ~id ~tp f =
  let env = read () in
  let arg = D.lvl tp env.local_size in
  scope (fun env -> {env with 
    locals = env.locals <: arg; 
    local_tps = env.local_tps <: tp;
    local_names = env.local_names <: (id, arg);
    local_size = env.local_size + 1
  }) @@ fun () -> f arg


let lookup_pattern_var id = (read ()).pattern_vars |> Option.fold ~none:None ~some:(fun tbl -> Hashtbl.find_opt tbl id)
let lookup_local id = (read ()).local_names |> Bwd.find_opt (fun (id', _) -> String.equal id id') |> Option.map snd
let lookup_global id = (read ()).globals |> Bwd.find_opt (fun (id', _) -> String.equal id id') |> Option.map snd


let nbe_env () =
  let env = read () in
  D.{locals = env.locals; globals = env.globals}

let eval tm =
  Nbe.Eval.eval_in_env (nbe_env ()) tm

let elim_clo = Nbe.Eval.elim_clo 

let quote tm =
  Nbe.Quote.quote_in_env (nbe_env ()) tm

let equate tm =
  Nbe.Conv.equate_in_env (nbe_env ()) tm



  (* 
     
  def f-id : (x : [A,B]A -> B) -> (A -> B)
  *)
open Bwd
open Bwd.Infix

module U = User_syntax
module S = Nbe.Syntax
module D = Nbe.Domain

module Terminal = Asai.Tty.Make(Logger.ErrorCode)

let eval_cmd globals (cmd : U.cmd) =
  match cmd.value with
    | Def {name ; tp ; tm} ->
      let tp,tm =
        match tp with
          | Some tp ->
            let tp = Elaborator.Elab.check_top globals tp D.Univ |> Nbe.Eval.eval_top globals in
            let tm = Elaborator.Elab.check_top globals tm tp in
            let tm = tm |> Nbe.Eval.eval_top globals in
            tp,tm
          | None ->
            let tp, tm = Elaborator.Elab.infer_top globals tm () in
            tp,Nbe.Eval.eval_top globals tm
      in
      print_endline @@ Printf.sprintf "def %s : %s := %s" name (tp |> Nbe.Quote.quote_top D.Univ |> S.show) (tm |> Nbe.Quote.quote_top tp |> S.show) ;
      globals <: (name, {tm ; tp})

let eval_cmds = List.fold_left eval_cmd Bwd.Emp

let run_program file =
  let cmds = Parse_cmd.parse_file file in
  Logger.run ~emit:Terminal.interactive_trace ~fatal:Terminal.interactive_trace @@ fun () ->
  Logger.Locate.run ~env:None @@ fun () ->
  let _ = eval_cmds cmds in
  print_endline @@ "Evaluted " ^ file
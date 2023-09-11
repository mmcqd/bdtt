open Bwd
open Bwd.Infix

module U = User_syntax
module S = Nbe.Syntax
module D = Nbe.Domain

let eval_cmd globals (cmd : U.cmd) =
  match cmd.value with
    | Def {name ; tp ; tm} ->
      let tm =
        match tp with
          | Some tp ->
            let tp = Elaborator.Elab.check_top globals tp {data = D.Univ ; tp = D.Univ} |> Nbe.Eval.eval_top globals in
            let tm = Elaborator.Elab.check_top globals tm tp |> Nbe.Eval.eval_top globals in
            tm
          | None ->
            let tm = Elaborator.Elab.infer_top globals tm () in
            Nbe.Eval.eval_top globals tm
      in
      print_endline @@ Printf.sprintf "def %s : %s := %s" name ({data = tm.tp ; tp = D.Univ} |> Nbe.Quote.quote_top |> S.show) (tm |> Nbe.Quote.quote_top |> S.show) ;
      globals <: (name, tm)

let eval_cmds = List.fold_left eval_cmd Bwd.Emp

let run_program file =
  let cmds = Parse_cmd.parse_file file in
  let _ = eval_cmds cmds in
  print_endline @@ "Evaluted " ^ file
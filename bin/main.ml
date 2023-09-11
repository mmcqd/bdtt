let () = 
  let filename = Sys.argv.(1) in
  Interpreter.run_program filename
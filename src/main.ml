open Syntax

let expr_of_file f = 
  Parser.expr Lexer.token (Lexing.from_channel (open_in f))

let expr_of_string s =
  Parser.expr Lexer.token (Lexing.from_string s)

let exec f = 
  try 
    Syntax.string_of_expr (expr_of_file f) 
  with exn -> "ML error: "^(Printexc.to_string exn)

let exec_string s = 
  try
    Syntax.string_of_expr (expr_of_string s) 
  with exn -> "ML error: "^(Printexc.to_string exn)

let _ =
  let src = "let rec fact = fun x -> if x <= 1 then x else ((fact (x - 1)) * x )in fact 3" in
    let p  = (Evaluator.eval Prelude.env (expr_of_string src))
    in print_endline (Syntax.string_of_value p)
    (* try Printf.printf "ML result: %s \n" (exec_string src) *)
    (* with _ -> () *)

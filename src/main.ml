
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
  let src = "let add = fun x -> fun y -> y in add 2 3" in
    try Printf.printf "ML result: %s \n" (exec_string src)
    with _ -> ()

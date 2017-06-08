exception ParserError of string
exception LexerError of string

type expr =   
  Const of int 
| True   
| False      
| EmptyExpr
| Var of string    
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| LetRec of string * expr * expr
	
type value =  
    Int of int		
  | Bool of bool          
  | Closure of env * string option * string * expr 
  | Empty                    
  | Pair of value * value     
  | MonadV of (dnval lazy_t -> dnval)
and dnval = value

and env = (string * value) list

let rec string_of_value v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (string_of_env evn) fs x (string_of_expr e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (string_of_value v1) (string_of_value v2) 
  | Empty -> 
      "[]"
  | MonadV (_) -> "<MonadV>"

and string_of_env evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (string_of_value v)) evn in
  "["^(String.concat ";" xs)^"]"

and string_of_expr e =
  match e with
      Const i ->
        Printf.sprintf "Const(%d)" i
    | True -> 
        "True" 
    | False -> 
        "False"
    | EmptyExpr -> 
        "EmptyExpr"
    | Var x -> 
        Printf.sprintf "Var(%s)" x
    | If (e1,e2,e3) -> 
        Printf.sprintf "If(%s, %s, %s)" 
        (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "Let(%s, %s, %s)" 
        x (string_of_expr e1) (string_of_expr e2) 
    | App (e1,e2) -> 
        Printf.sprintf "App(%s, %s)" (string_of_expr e1) (string_of_expr e2)
    | Fun (x,e) -> 
        Printf.sprintf "Fun(%s, %s)" x (string_of_expr e) 
    | LetRec (x,e1,e2) -> 
        Printf.sprintf "LetRec(%s, %s, %s)" 
        x (string_of_expr e1) (string_of_expr e2) 


open Syntax

let ($) f g x = f (g x)

let unary f = MonadV f
let binary f = MonadV (unary $ f)

let lookup x env =
    try
      List.assoc x env
    with
      Not_found -> failwith ("Variable not bound: "^x)

let process_closure eval env _ b c = match eval env b with 
  | Closure(clenv, Some a, left, right) -> eval ((a, Closure(clenv, Some a, left, right))::env) c
  | _ -> assert false

let rec eval env = function
  | Const i -> Int i
  | True -> Bool true
  | False -> Bool false
  | EmptyExpr -> Empty
  | App(x, y) -> (match eval env x with
    | Closure (clenv, clid, left, right) -> (match clid with
      | Some i -> eval ((i, Closure(clenv, clid, left, right))::((left, (eval env y))::clenv)) right
      | None -> eval ((left, (eval env y))::clenv) right )
    | MonadV unary -> unary (lazy (eval env y))
    | _ -> assert false )
  | Var x -> lookup x env
  | If (cond, consequent, alternative) -> if (eval env cond) = (Bool true) then (eval env consequent) else (eval env alternative)
  | Let (x, e1, e2) ->
     let y = eval env e1 in
     eval ( (x, y) :: env ) e2
  | LetRec (a,b,c) -> (match b with
    | Fun (_, _) -> (match eval env b with
      | Closure (clenv, _, left, right) -> eval ((a, Closure(clenv, Some a, left, right))::env) c
      | _ -> process_closure eval env a b c )
    | _ -> assert false)
  | Fun (a,b) -> Closure (env, None, a, b)
  ;;


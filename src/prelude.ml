open Evaluator
open Syntax

let p_add = binary
  (fun (lazy lhs) (lazy rhs) ->
    match lhs, rhs with
      | Int l, Int r -> Int (l + r)
      | _, _ -> assert false)


let env =
  List.fold_left
    (fun (venv) (id, v) ->
      ((id,v)::venv))
		[]
    [
     ("+", p_add)
		]

open Evaluator
open Syntax

let p_add = binary
  (fun (lazy lhs) (lazy rhs) ->
    match lhs, rhs with
      | Int l, Int r -> Int (l + r)
      | _, _ -> assert false)

let p_mul = binary
  (fun (lazy lhs) (lazy rhs) ->
    match lhs, rhs with
      | Int l, Int r -> Int (l * r)
      | _, _ -> assert false)

let p_sub = binary
  (fun (lazy lhs) (lazy rhs) ->
    match lhs, rhs with
      | Int l, Int r -> Int (l - r)
      | _, _ -> assert false)

let p_lt = binary
  (fun (lazy lhs) (lazy rhs) ->
    let lt l r =
      match l, r with
        | Int l, Int r -> l < r
        | (Closure _ | MonadV _), (Closure _ | MonadV _) ->
          failwith "Functional values are not comparable: <"
        | _, _ -> assert false
    in
    Bool (lt lhs rhs))

let p_lte = binary
  (fun (lazy lhs) (lazy rhs) ->
    let lt l r =
      match l, r with
        | Int l, Int r -> l <= r
        | (Closure _ | MonadV _), (Closure _ | MonadV _) ->
          failwith "Functional values are not comparable: <"
        | _, _ -> assert false
    in
    Bool (lt lhs rhs))

let env =
  List.fold_left
    (fun (venv) (id, v) ->
      ((id,v)::venv))
		[]
    [
      ("+", p_add);
      ("-", p_sub);
      ("*", p_mul);
      ("<", p_lt);
      ("<=", p_lte)
		]

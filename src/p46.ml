type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let table2 a b expr =
  let eval va vb =
    let rec aux = function
      | Var var -> if var = a then va else if var = b then vb else raise Not_found
      | Not e -> not (aux e)
      | And (x, y) -> aux x && aux y
      | Or (x, y) -> aux x || aux y
    in
    aux expr
  in
  [ true, true, eval true true
  ; true, false, eval true false
  ; false, true, eval false true
  ; false, false, eval false false
  ]
;;

assert (
  table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))
  = [ true, true, true; true, false, true; false, true, false; false, false, false ])
;;

assert (
  table2 "a" "b" (Not (And (Var "a", Or (Var "a", Var "b"))))
  = [ true, true, false; true, false, false; false, true, true; false, false, true ])

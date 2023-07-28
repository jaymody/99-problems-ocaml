module StringMap = Map.Make (String)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let table vars expr =
  let initial_varmap =
    List.fold_left
      (fun map var -> StringMap.add var false map)
      StringMap.empty vars
  in
  let eval varmap =
    let rec aux = function
      | Var var -> StringMap.find var varmap
      | Not e -> not (aux e)
      | And (x, y) -> aux x && aux y
      | Or (x, y) -> aux x || aux y
    in
    aux expr
  in
  let rec aux varmap path acc vars =
    match vars with
    | [] -> (List.rev path, eval varmap) :: acc
    | h :: t ->
        let acc =
          aux (StringMap.add h false varmap) ((h, false) :: path) acc t
        in
        aux (StringMap.add h true varmap) ((h, true) :: path) acc t
  in
  aux initial_varmap [] [] vars
;;

assert (
  table [ "a"; "b" ] (And (Var "a", Or (Var "a", Var "b")))
  = [
      ([ ("a", true); ("b", true) ], true);
      ([ ("a", true); ("b", false) ], true);
      ([ ("a", false); ("b", true) ], false);
      ([ ("a", false); ("b", false) ], false);
    ])

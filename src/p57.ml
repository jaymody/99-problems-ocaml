open Tree

let construct list =
  let rec add t x' =
    match t with
    | Empty -> Node (x', Empty, Empty)
    | Node (x, l, r) -> if x' < x then Node (x, add l x', r) else Node (x, l, add r x')
  in
  List.fold_left add Empty list
;;

let%test _ = P56.is_symmetric (construct [ 5; 3; 18; 1; 4; 12; 21 ])
let%test _ = not (P56.is_symmetric (construct [ 3; 2; 5; 7; 4 ]))

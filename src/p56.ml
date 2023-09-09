type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let is_symmetric =
  let rec aux l r =
    match l, r with
    | Empty, Empty -> true
    | Node (_, ll, lr), Node (_, rl, rr) -> aux ll rr && aux lr rl
    | _ -> false
  in
  function
  | Empty -> true
  | Node (_, l, r) -> aux l r
;;

let%test _ = is_symmetric (Node ('x', Empty, Empty))
let%test _ = is_symmetric (Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)))
let%test _ = not (is_symmetric (Node ('x', Empty, Node ('x', Empty, Empty))))

let%test _ =
  is_symmetric
    (Node
       ( 'x'
       , Node ('x', Empty, Node ('x', Empty, Empty))
       , Node ('x', Node ('x', Empty, Empty), Empty) ))
;;

open Tree

let rec count_leaves = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r
;;

let%test _ = count_leaves Empty = 0
let%test _ = count_leaves (Node ('x', Empty, Empty)) = 1
let%test _ = count_leaves (Node ('x', Node ('x', Empty, Empty), Empty)) = 1

let%test _ =
  count_leaves (Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), Empty)) = 1
;;

let%test _ =
  count_leaves
    (Node ('x', Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), Empty), Empty))
  = 1
;;

let%test _ =
  count_leaves (Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))) = 2
;;

let%test _ =
  count_leaves
    (Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), Node ('x', Empty, Empty)))
  = 2
;;

let%test _ =
  count_leaves
    (Node
       ( 'x'
       , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))
       , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)) ))
  = 4
;;

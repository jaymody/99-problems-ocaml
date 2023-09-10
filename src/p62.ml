open Tree

let collect_leaves =
  let rec aux acc = function
    | Empty -> acc
    | Node (x, Empty, Empty) -> x :: acc
    | Node (_, l, r) -> aux (aux acc r) l
  in
  aux []
;;

let%test _ = collect_leaves Empty = []
let%test _ = collect_leaves (Node ('a', Empty, Empty)) = [ 'a' ]
let%test _ = collect_leaves (Node ('a', Node ('b', Empty, Empty), Empty)) = [ 'b' ]

let%test _ =
  collect_leaves (Node ('a', Node ('b', Node ('c', Empty, Empty), Empty), Empty))
  = [ 'c' ]
;;

let%test _ =
  collect_leaves
    (Node ('a', Node ('b', Node ('c', Node ('d', Empty, Empty), Empty), Empty), Empty))
  = [ 'd' ]
;;

let%test _ =
  collect_leaves (Node ('a', Node ('b', Empty, Empty), Node ('c', Empty, Empty)))
  = [ 'b'; 'c' ]
;;

let%test _ =
  collect_leaves
    (Node ('a', Node ('b', Node ('c', Empty, Empty), Empty), Node ('d', Empty, Empty)))
  = [ 'c'; 'd' ]
;;

let%test _ =
  collect_leaves
    (Node
       ( 'a'
       , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
       , Node ('c', Node ('f', Empty, Empty), Node ('g', Empty, Empty)) ))
  = [ 'd'; 'e'; 'f'; 'g' ]
;;

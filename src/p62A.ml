open Tree

let internals t =
  let rec aux acc = function
    | Empty | Node (_, Empty, Empty) -> acc
    | Node (x, l, r) -> aux (aux (x :: acc) l) r
  in
  List.rev (aux [] t)
;;

let%test _ = internals Empty = []
let%test _ = internals (Node ('a', Empty, Empty)) = []
let%test _ = internals (Node ('a', Node ('b', Empty, Empty), Empty)) = [ 'a' ]

let%test _ =
  internals (Node ('a', Node ('b', Node ('c', Empty, Empty), Empty), Empty))
  = [ 'a'; 'b' ]
;;

let%test _ =
  internals
    (Node ('a', Node ('b', Node ('c', Node ('d', Empty, Empty), Empty), Empty), Empty))
  = [ 'a'; 'b'; 'c' ]
;;

let%test _ =
  internals (Node ('a', Node ('b', Empty, Empty), Node ('c', Empty, Empty))) = [ 'a' ]
;;

let%test _ =
  internals
    (Node ('a', Node ('b', Node ('c', Empty, Empty), Empty), Node ('d', Empty, Empty)))
  = [ 'a'; 'b' ]
;;

let%test _ =
  internals
    (Node
       ( 'a'
       , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
       , Node ('c', Node ('f', Empty, Empty), Node ('g', Empty, Empty)) ))
  = [ 'a'; 'b'; 'c' ]
;;

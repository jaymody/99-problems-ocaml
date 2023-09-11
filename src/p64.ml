open Tree

let at_level t n =
  let rec aux acc d = function
    | Empty -> acc
    | Node (x, _, _) when d = n -> x :: acc
    | Node (_, l, r) -> aux (aux acc (d + 1) l) (d + 1) r
  in
  List.rev (aux [] 1 t)
;;

let%test _ =
  let tree =
    Node
      ( 'a'
      , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
      , Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)) )
  in
  at_level tree 1 = [ 'a' ]
  && at_level tree 2 = [ 'b'; 'c' ]
  && at_level tree 3 = [ 'd'; 'e'; 'f' ]
  && at_level tree 4 = [ 'g' ]
  && at_level tree 5 = []
;;

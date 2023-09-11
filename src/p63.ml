open Tree

let complete_binary_tree list =
  let arr = Array.of_list list in
  let len = Array.length arr in
  let rec aux i =
    if i > len then Empty else Node (arr.(i - 1), aux (2 * i), aux ((2 * i) + 1))
  in
  aux 1
;;

let%test _ = complete_binary_tree [] = Empty
let%test _ = complete_binary_tree [ 1 ] = Node (1, Empty, Empty)
let%test _ = complete_binary_tree [ 1; 2 ] = Node (1, Node (2, Empty, Empty), Empty)

let%test _ =
  complete_binary_tree [ 1; 2; 3 ]
  = Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty))
;;

let%test _ =
  complete_binary_tree [ 1; 2; 3; 4 ]
  = Node (1, Node (2, Node (4, Empty, Empty), Empty), Node (3, Empty, Empty))
;;

let%test _ =
  complete_binary_tree [ 1; 2; 3; 4; 5; 6 ]
  = Node
      ( 1
      , Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty))
      , Node (3, Node (6, Empty, Empty), Empty) )
;;

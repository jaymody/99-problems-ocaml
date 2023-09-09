open Tree

let rec hbal_tree =
  let make l r = Node ('x', l, r) in
  let acc = [] in
  function
  | 0 -> [ Empty ]
  | 1 -> [ make Empty Empty ]
  | h ->
    let subtrees1 = hbal_tree (h - 1) in
    let subtrees2 = hbal_tree (h - 2) in
    let acc =
      List.fold_left
        (fun acc (a, b) -> make a b :: acc)
        acc
        (P55.cartesian_product subtrees1 subtrees1)
    in
    List.fold_left
      (fun acc (a, b) -> make a b :: make b a :: acc)
      acc
      (P55.cartesian_product subtrees1 subtrees2)
;;

let%test _ = hbal_tree 0 = [ Empty ]
let%test _ = hbal_tree 1 = [ Node ('x', Empty, Empty) ]

let%test _ =
  hbal_tree 2
  = [ Node ('x', Node ('x', Empty, Empty), Empty)
    ; Node ('x', Empty, Node ('x', Empty, Empty))
    ; Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))
    ]
;;

let%test _ =
  hbal_tree 3
  = [ Node
        ( 'x'
        , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))
        , Node ('x', Empty, Empty) )
    ; Node
        ( 'x'
        , Node ('x', Empty, Empty)
        , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)) )
    ; Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), Node ('x', Empty, Empty))
    ; Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Node ('x', Empty, Empty)))
    ; Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), Node ('x', Empty, Empty))
    ; Node ('x', Node ('x', Empty, Empty), Node ('x', Node ('x', Empty, Empty), Empty))
    ; Node
        ( 'x'
        , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))
        , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)) )
    ; Node
        ( 'x'
        , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))
        , Node ('x', Empty, Node ('x', Empty, Empty)) )
    ; Node
        ( 'x'
        , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))
        , Node ('x', Node ('x', Empty, Empty), Empty) )
    ; Node
        ( 'x'
        , Node ('x', Empty, Node ('x', Empty, Empty))
        , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)) )
    ; Node
        ( 'x'
        , Node ('x', Empty, Node ('x', Empty, Empty))
        , Node ('x', Empty, Node ('x', Empty, Empty)) )
    ; Node
        ( 'x'
        , Node ('x', Empty, Node ('x', Empty, Empty))
        , Node ('x', Node ('x', Empty, Empty), Empty) )
    ; Node
        ( 'x'
        , Node ('x', Node ('x', Empty, Empty), Empty)
        , Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)) )
    ; Node
        ( 'x'
        , Node ('x', Node ('x', Empty, Empty), Empty)
        , Node ('x', Empty, Node ('x', Empty, Empty)) )
    ; Node
        ( 'x'
        , Node ('x', Node ('x', Empty, Empty), Empty)
        , Node ('x', Node ('x', Empty, Empty), Empty) )
    ]
;;

open Tree

let layout_binary_tree_1 t =
  let rec aux d n = function
    | Empty -> Empty, n
    | Node (v, l, r) ->
      let l, n = aux (d + 1) n l in
      let n = n + 1 in
      let r, ret_n = aux (d + 1) n r in
      Node ((v, n, d), l, r), ret_n
  in
  fst (aux 1 0 t)
;;

let%test _ =
  let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node
      ( 'n'
      , Node
          ( 'k'
          , Node ('c', leaf 'a', Node ('h', Node ('g', leaf 'e', Empty), Empty))
          , leaf 'm' )
      , Node ('u', Node ('p', Empty, Node ('s', leaf 'q', Empty)), Empty) )
  in
  layout_binary_tree_1 example_layout_tree
  = Node
      ( ('n', 8, 1)
      , Node
          ( ('k', 6, 2)
          , Node
              ( ('c', 2, 3)
              , Node (('a', 1, 4), Empty, Empty)
              , Node
                  ( ('h', 5, 4)
                  , Node (('g', 4, 5), Node (('e', 3, 6), Empty, Empty), Empty)
                  , Empty ) )
          , Node (('m', 7, 3), Empty, Empty) )
      , Node
          ( ('u', 12, 2)
          , Node
              ( ('p', 9, 3)
              , Empty
              , Node (('s', 11, 4), Node (('q', 10, 5), Empty, Empty), Empty) )
          , Empty ) )
;;

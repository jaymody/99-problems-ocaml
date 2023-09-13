open Tree

(*
   The y coordinate is trivial, it's just y = height.

   For the x coordinate we need to expoit some information. First, we notice
   that the space between adjacent nodes at a given depth is constant.
   Precisely, the space between nodes at some depth 2ʰ⁻ⁿ − 1 where h is the
   overall height of the tree and n is a given depth. Assuming the tree is
   complete, the x coordinate can then be described with the following
   recurrence relation:

   x₀ = 0
   xₙ = xₙ₋₁ + 2ʰ⁻ⁿ (if right child or root)
   xₙ = xₙ₋₁ - 2ʰ⁻ⁿ (if left child)

   Finally, this is assuming a complete tree, but the tree may not be complete.
   In this case, we simply need to find the minumum value of x in the tree,
   and subtract all the nodes by (x_min - 1), that is x := x - (x_min - 1).
*)

let rec height = function
  | Empty -> 0
  | Node (_, l, r) -> 1 + max (height l) (height r)
;;

(* Damn OCaml really doesn't have an integer power function eh? *)
let rec pow b n = if n < 1 then 1 else b * pow b (n - 1)

let layout_binary_tree_2 t =
  let h = height t in
  let rec aux x sep d = function
    | Empty -> Empty
    | Node (v, l, r) ->
      Node
        ((v, x, d), aux (x - sep) (sep / 2) (d + 1) l, aux (x + sep) (sep / 2) (d + 1) r)
  in
  let rec get_min_x = function
    | Empty -> max_int
    | Node ((_, x, _), l, r) -> min x (min (get_min_x l) (get_min_x r))
  in
  let t = aux (pow 2 (h - 1)) (pow 2 (h - 2)) 1 t in
  let min_x = get_min_x t in
  let rec sub_min_x = function
    | Empty -> Empty
    | Node ((v, x, y), l, r) -> Node ((v, x - min_x + 1, y), sub_min_x l, sub_min_x r)
  in
  sub_min_x t
;;

let%test _ =
  let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node
      ( 'n'
      , Node ('k', Node ('c', leaf 'a', Node ('e', leaf 'd', leaf 'g')), leaf 'm')
      , Node ('u', Node ('p', Empty, leaf 'q'), Empty) )
  in
  layout_binary_tree_2 example_layout_tree
  = Node
      ( ('n', 15, 1)
      , Node
          ( ('k', 7, 2)
          , Node
              ( ('c', 3, 3)
              , Node (('a', 1, 4), Empty, Empty)
              , Node
                  ( ('e', 5, 4)
                  , Node (('d', 4, 5), Empty, Empty)
                  , Node (('g', 6, 5), Empty, Empty) ) )
          , Node (('m', 11, 3), Empty, Empty) )
      , Node
          ( ('u', 23, 2)
          , Node (('p', 19, 3), Empty, Node (('q', 21, 4), Empty, Empty))
          , Empty ) )
;;

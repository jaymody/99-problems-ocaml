type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let cartesian_product a b = List.concat_map (fun x -> List.map (fun y -> x, y) b) a

let cbal_tree =
  let make l r = Node ('x', l, r) in
  let rec aux acc = function
    | 0 -> [ Empty ]
    | 1 -> [ make Empty Empty ]
    | n ->
      (match n - 1 with
       | n when n mod 2 = 0 ->
         let subtrees = aux acc (n / 2) in
         List.fold_left
           (fun acc (a, b) -> make a b :: acc)
           acc
           (cartesian_product subtrees subtrees)
       | n when n mod 2 = 1 ->
         let subtrees1 = aux acc (n / 2) in
         let subtrees2 = aux acc (n - (n / 2)) in
         List.fold_left
           (fun acc (a, b) -> make a b :: make b a :: acc)
           acc
           (cartesian_product subtrees1 subtrees2)
       | _ -> invalid_arg "unreachable")
  in
  aux []
;;

let%test _ = cbal_tree 0 = [ Empty ]
let%test _ = cbal_tree 1 = [ Node ('x', Empty, Empty) ]

let%test _ =
  cbal_tree 2
  = [ Node ('x', Empty, Node ('x', Empty, Empty))
    ; Node ('x', Node ('x', Empty, Empty), Empty)
    ]
;;

let%test _ =
  cbal_tree 3 = [ Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)) ]
;;

let%test _ =
  cbal_tree 4
  = [ Node ('x', Node ('x', Empty, Empty), Node ('x', Node ('x', Empty, Empty), Empty))
    ; Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), Node ('x', Empty, Empty))
    ; Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Node ('x', Empty, Empty)))
    ; Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), Node ('x', Empty, Empty))
    ]
;;

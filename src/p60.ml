open Tree

let max_nodes h = (1 lsl h) - 1

let rec min_nodes = function
  | 0 -> 0
  | 1 -> 1
  | h -> 1 + min_nodes (h - 1) + min_nodes (h - 2)
;;

let min_height n =
  let rec aux h = if max_nodes h >= n then h else aux (h + 1) in
  aux 0
;;

let max_height n =
  let rec aux h = if min_nodes h > n then h - 1 else aux (h + 1) in
  aux 0
;;

let rec count_nodes = function
  | Empty -> 0
  | Node (_, l, r) -> 1 + count_nodes l + count_nodes r
;;

let hbal_tree_nodes n =
  let min_h, max_h = min_height n, max_height n in
  List.filter
    (fun t -> count_nodes t = n)
    (List.concat_map
       (fun h -> P59.hbal_tree h)
       (List.init (max_h - min_h + 1) (fun x -> min_h + x)))
;;

let%test _ = List.length (hbal_tree_nodes 15) = 1553

module HuffmanTree = struct
  type t =
    | Leaf of string * int
    | Node of t * int * t

  let rank = function
    | Leaf (_, n) -> n
    | Node (_, n, _) -> n
  ;;

  let combine l r = Node (l, rank l + rank r, r)

  let codes =
    let rec aux acc path = function
      | Leaf (s, _) -> (s, path) :: acc
      | Node (l, _, r) -> aux (aux acc (path ^ "0") l) (path ^ "1") r
    in
    aux [] ""
  ;;

  let compare a b = compare (rank a) (rank b)
end

(*
   Ideally, we'd use a priority queue with guarenteed log N insert and pop
   but this will for now.
*)
let pop_min =
  let cmp a b = HuffmanTree.compare a b in
  let rec remove x = function
    | [] -> invalid_arg "x not found in list"
    | hd :: tl -> if cmp hd x = 0 then tl else hd :: remove x tl
  in
  function
  | [] -> None, []
  | hd :: tl as list ->
    let min_val = List.fold_left (fun a b -> if cmp a b < 0 then a else b) hd tl in
    let list = remove min_val list in
    Some min_val, list
;;

let huffman fs =
  let rec aux pq =
    let first, pq = pop_min pq in
    let second, pq = pop_min pq in
    match first, second with
    | None, None -> []
    | Some first, None -> HuffmanTree.codes first
    | Some first, Some second -> aux (HuffmanTree.combine first second :: pq)
    | None, Some _ -> invalid_arg "unreacheable"
  in
  let pq = List.fold_left (fun pq (s, n) -> HuffmanTree.Leaf (s, n) :: pq) [] fs in
  aux pq
;;

let%test _ =
  List.sort compare (huffman [ "a", 45; "b", 13; "c", 12; "d", 16; "e", 9; "f", 5 ])
  = List.sort
      compare
      [ "a", "0"; "c", "100"; "b", "101"; "f", "1100"; "e", "1101"; "d", "111" ]
;;

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

let huffman fs =
  let module PQ = Pq.MakePQ (HuffmanTree) in
  let rec aux pq =
    let first, pq = PQ.pop pq in
    let second, pq = PQ.pop pq in
    match first, second with
    | None, None -> []
    | Some first, None -> HuffmanTree.codes first
    | Some first, Some second -> aux (PQ.push (HuffmanTree.combine first second) pq)
    | None, Some _ -> invalid_arg "unreacheable"
  in
  let pq =
    List.fold_left (fun pq (s, n) -> PQ.push (HuffmanTree.Leaf (s, n)) pq) PQ.empty fs
  in
  aux pq
;;

let%test _ =
  List.sort compare (huffman [ "a", 45; "b", 13; "c", 12; "d", 16; "e", 9; "f", 5 ])
  = List.sort
      compare
      [ "a", "0"; "c", "100"; "b", "101"; "f", "1100"; "e", "1101"; "d", "111" ]
;;

(*
   Copied and modified from:

   https://github.com/jaymody/ocaml-algos/tree/4a161eb851929b5fe9f8dcaddb88a7f3e1fd242b
*)

module type Comparable = sig
  type t

  val compare : t -> t -> int
end

type comparison =
  | Eq
  | Gt
  | Lt

module MakeAVL (Key : Comparable) : sig
  type key = Key.t

  type 'a t =
    | Empty
    | Node of 'a t * key * 'a * 'a t * int * int

  val empty : 'a t
  val size : 'a t -> int
  val insert : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a option * 'a t
  val get_min : 'a t -> (key * 'a) option
  val remove_min : 'a t -> (key * 'a) option * 'a t
end = struct
  type key = Key.t

  type 'a t =
    | Empty
    | Node of 'a t * key * 'a * 'a t * int * int

  let cmp a b =
    let diff = Key.compare a b in
    if diff < 0 then Lt else if diff > 0 then Gt else Eq
  ;;

  let empty = Empty

  let size = function
    | Empty -> 0
    | Node (_, _, _, _, _, n) -> n
  ;;

  let create l k v r =
    let height = function
      | Empty -> 0
      | Node (_, _, _, _, h, _) -> h
    in
    let make (l, k, v, r) =
      Node (l, k, v, r, 1 + max (height l) (height r), 1 + size l + size r)
    in
    let rotate_left (k, v, l) (rl, rk, rv, rr) = make (l, k, v, rl), rk, rv, rr in
    let rotate_right (k, v, r) (ll, lk, lv, lr) = ll, lk, lv, make (lr, k, v, r) in
    make
      (match l, r with
       | _, Node (rl, rk, rv, rr, _, _) when height rr > height l ->
         rotate_left (k, v, l) (rl, rk, rv, rr)
       | _, Node ((Node (rll, rlk, rlv, rlr, _, _) as rl), rk, rv, rr, _, _)
         when height rl > height l ->
         rotate_right (rk, rv, rr) (rll, rlk, rlv, rlr) |> rotate_left (k, v, l)
       | Node (ll, lk, lv, (Node (lrl, lrk, lrv, lrr, _, _) as lr), _, _), _
         when height lr > height r ->
         rotate_left (lk, lv, ll) (lrl, lrk, lrv, lrr) |> rotate_right (k, v, r)
       | Node (ll, lk, lv, lr, _, _), _ when height ll > height r ->
         rotate_right (k, v, r) (ll, lk, lv, lr)
       | _ -> l, k, v, r)
  ;;

  let insert k' v' t =
    let rec aux = function
      | Empty -> create Empty k' v' empty
      | Node (l, k, v, r, _, _) ->
        (match cmp k' k with
         | Lt | Eq -> create (aux l) k v r
         | Gt -> create l k v (aux r))
    in
    aux t
  ;;

  let remove k' t =
    let rec pop_successor l k v r =
      match l with
      | Empty -> (k, v), r
      | Node (ll, lk, lv, lr, _, _) ->
        let succesor, l = pop_successor ll lk lv lr in
        succesor, create l k v r
    in
    let rec aux = function
      | Empty -> None, Empty
      | Node (l, k, v, r, _, _) ->
        (match cmp k' k with
         | Eq ->
           ( Some v
           , (match r with
              | Empty -> l
              | Node (rl, rk, rv, rr, _, _) ->
                let (k, v), r = pop_successor rl rk rv rr in
                create l k v r) )
         | Lt ->
           let e, l = aux l in
           e, create l k v r
         | Gt ->
           let e, r = aux r in
           e, create l k v r)
    in
    aux t
  ;;

  let rec get_min = function
    | Empty -> None
    | Node (Empty, k, v, _, _, _) -> Some (k, v)
    | Node (l, _, _, _, _, _) -> get_min l
  ;;

  let rec remove_min = function
    | Empty -> None, Empty
    | Node (Empty, k, v, r, _, _) -> Some (k, v), r
    | Node (l, k, v, r, _, _) ->
      let e, l = remove_min l in
      e, create l k v r
  ;;
end

module type PQ = sig
  type elt
  type queue

  val empty : queue
  val push : elt -> queue -> queue
  val pop : queue -> elt option * queue
  val peek : queue -> elt option
  val size : queue -> int
  val is_empty : queue -> bool
end

module MakePQ (Key : Comparable) : PQ with type elt = Key.t = struct
  module AvlTree = MakeAVL (Key)

  type elt = AvlTree.key
  type queue = int AvlTree.t

  let empty = AvlTree.empty
  let push e q = AvlTree.insert e 0 q

  let pop q =
    let e, q = AvlTree.remove_min q in
    Option.map fst e, q
  ;;

  (* TODO: Peek can be made O(1) if we cache the min and store it one push/pop. *)
  let peek q = Option.map fst (AvlTree.get_min q)
  let size q = AvlTree.size q
  let is_empty q = size q = 0
end

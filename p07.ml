type 'a node = One of 'a | Many of 'a node list

(*
Note: We use a :: new_list instead of new_list @ [ x ] because list is just a
singly linked list, meaning @ is an O(n) operation and :: is a O(1) operation.
*)
let flatten list =
  let rec fn new_list = function
    | One a -> a :: new_list
    | Many [] -> new_list
    | Many (h :: t) -> fn (fn new_list h) (Many t)
  in
  List.rev (fn [] (Many list))
;;

assert (
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ])

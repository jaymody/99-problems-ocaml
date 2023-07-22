type 'a node = One of 'a | Many of 'a node list

let flatten list =
  let rec aux acc = function
    | One x :: t -> aux (x :: acc) t
    | Many x :: t -> aux (aux acc x) t
    | _ -> acc
  in
  List.rev (aux [] list)
;;

assert (
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ])

let length =
  let rec aux n = function
    | [] -> n
    | _ :: t -> aux (n + 1) t
  in
  aux 0
;;

assert (length [] = 0);;
assert (length [ 1 ] = 1);;
assert (length [ 1; 2 ] = 2);;
assert (length [ 1; 6; 2 ] = 3);;
assert (length [ 8; 1; 3; 4 ] = 4);;
assert (length [ 0; 0; 0; 0 ] = 4);;
assert (length [ 4; 1; 1; -4 ] = 4);;

(* makes sure our implementation is efficient enough to run on a really long list *)
assert (length (List.init 1000000 (fun x -> x + 1)) = 1000000)

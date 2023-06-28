let rec last_two = function
  | [] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t
;;

assert (last_two [] = None);;
assert (last_two [ 1 ] = None);;
assert (last_two [ 1; 5 ] = Some (1, 5));;
assert (last_two [ 1; 5; 10 ] = Some (5, 10));;
assert (last_two [ 1; 5; 10; -5 ] = Some (10, -5));;

(* makes sure our implementation is efficient enough to run on a really long list *)
assert (last_two (List.init 1000000 (fun x -> x + 1)) = Some (999999, 1000000))

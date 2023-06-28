let rec nth n = function
    | [] -> None
    | h :: t -> if n = 0 then Some h else nth (n - 1) t;;

assert (nth 0 [] = None);;
assert (nth 1 [] = None);;

assert (nth 0 [ 1 ] = Some 1);;
assert (nth 1 [ 1 ] = None);;

assert (nth 0 [ 1 ; 5 ] = Some 1);;
assert (nth 1 [ 1 ; 5 ] = Some 5);;
assert (nth 2 [ 1 ; 5 ] = None);;

assert (nth 0 [ 1; 5; 10; -5 ] = Some 1);;
assert (nth 1 [ 1; 5; 10; -5 ] = Some 5);;
assert (nth 2 [ 1; 5; 10; -5 ] = Some 10);;
assert (nth 3 [ 1; 5; 10; -5 ] = Some (-5));;
assert (nth 4 [ 1; 5; 10; -5 ] = None);;
assert (nth 5 [ 1; 5; 10; -5 ] = None);;
assert (nth 100 [ 1; 5; 10; -5 ] = None);;

(* makes sure our implementation is efficient enough to run on a really long list *)
assert (nth 1043 (List.init 1000000 (fun x -> x + 1)) = Some 1044 );;

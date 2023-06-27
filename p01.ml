let rec last = function
    | [] -> None
    | [ x ] -> Some x
    | _ :: t -> last t;;

assert (last [] = None);;
assert (last [ 1 ] = Some (1));;
assert (last [ 1; 5 ] = Some (5));;
assert (last [ 1; 5; 10 ] = Some (10));;
assert (last [ 1; 5; 10; -5 ] = Some (-5));;

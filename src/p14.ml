let duplicate l =
  let rec fn l o = match l with h :: t -> fn t (h :: h :: o) | _ -> o in
  List.rev (fn l [])
;;

assert (duplicate [] = []);;
assert (duplicate [ "a" ] = [ "a"; "a" ]);;
assert (duplicate [ "a"; "a" ] = [ "a"; "a"; "a"; "a" ]);;

assert (
  duplicate [ "a"; "b"; "c"; "c"; "d" ]
  = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ])

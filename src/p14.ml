let duplicate list =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: h :: acc) t
  in
  List.rev (aux [] list)
;;

assert (duplicate [] = []);;
assert (duplicate [ "a" ] = [ "a"; "a" ]);;
assert (duplicate [ "a"; "a" ] = [ "a"; "a"; "a"; "a" ]);;

assert (
  duplicate [ "a"; "b"; "c"; "c"; "d" ]
  = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ])

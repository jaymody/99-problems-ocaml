let drop l n =
  let rec fn l i o =
    match l with
    | h :: t -> fn t (i + 1) (if i mod n = 0 then o else h :: o)
    | _ -> o
  in
  List.rev (fn l 1 [])
;;

assert (drop [] 3 = []);;
assert (drop [ "a"; "b"; "c"; "d"; "e" ] 1 = []);;
assert (drop [ "a"; "b"; "c"; "d"; "e" ] 2 = [ "a"; "c"; "e" ]);;

assert (
  drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ])

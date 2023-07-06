let slice l i j =
  let rec fn l i j o =
    match l with
    | h :: t -> fn t (i - 1) (j - 1) (if i > 0 || j < 0 then o else h :: o)
    | _ -> o
  in
  List.rev (fn l i j [])
;;

assert (slice [] 1 4 = []);;
assert (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 1 1 = [ "b" ])
;;

assert (
  slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 0 100
  = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
;;

assert (
  slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 0 9
  = [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
;;

assert (
  slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 1 8
  = [ "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i" ])
;;

assert (
  slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
  = [ "c"; "d"; "e"; "f"; "g" ])

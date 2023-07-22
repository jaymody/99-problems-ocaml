let slice list l r =
  let rec aux i j acc = function
    | [] -> acc
    | h :: t ->
        if j < 0 then acc
        else aux (i - 1) (j - 1) (if i > 0 then acc else h :: acc) t
  in
  List.rev (aux l r [] list)
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

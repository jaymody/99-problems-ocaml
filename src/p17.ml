let split list n =
  let rec aux i left = function
    | [] -> left, []
    | h :: t -> if i = n then left, h :: t else aux (i + 1) (h :: left) t
  in
  let left, right = aux 0 [] list in
  List.rev left, right
;;

assert (split [] 0 = ([], []));;
assert (split [] 1 = ([], []));;
assert (split [] 2 = ([], []));;
assert (split [ "a" ] 0 = ([], [ "a" ]));;
assert (split [ "a" ] 1 = ([ "a" ], []));;
assert (split [ "a"; "b"; "c" ] 1 = ([ "a" ], [ "b"; "c" ]));;

assert (
  split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]))
;;

assert (split [ "a"; "b"; "c"; "d" ] 5 = ([ "a"; "b"; "c"; "d" ], []))

let rec insert_at a i l =
  match l with
  | h :: t -> if i = 0 then a :: l else h :: insert_at a (i - 1) t
  | [] -> [ a ]
;;

assert (insert_at "a" 0 [] = [ "a" ]);;
assert (insert_at "a" 1 [] = [ "a" ]);;
assert (insert_at "a" 0 [ "a" ] = [ "a"; "a" ]);;
assert (insert_at "a" 1 [ "a" ] = [ "a"; "a" ]);;
assert (insert_at "b" 0 [ "a"; "c" ] = [ "b"; "a"; "c" ]);;
assert (insert_at "b" 1 [ "a"; "c" ] = [ "a"; "b"; "c" ]);;
assert (insert_at "b" 2 [ "a"; "c" ] = [ "a"; "c"; "b" ]);;

assert (
  insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "alfa"; "b"; "c"; "d" ])

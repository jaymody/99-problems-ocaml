(* Tail recursive version *)
(*
   let insert_at x n list =
   let rec aux i acc = function
   | [] -> if i >= 0 then x :: acc else acc
   | h :: t -> aux (i - 1) (if i = 0 then h :: x :: acc else h :: acc) t
   in
   List.rev (aux n [] list)
   ;;
*)

let rec insert_at x n = function
  | [] -> [ x ]
  | h :: t -> if n = 0 then x :: h :: t else h :: insert_at x (n - 1) t
;;

assert (insert_at "a" 0 [] = [ "a" ]);;
assert (insert_at "a" 1 [] = [ "a" ]);;
assert (insert_at "a" 0 [ "a" ] = [ "a"; "a" ]);;
assert (insert_at "a" 1 [ "a" ] = [ "a"; "a" ]);;
assert (insert_at "b" 0 [ "a"; "c" ] = [ "b"; "a"; "c" ]);;
assert (insert_at "b" 1 [ "a"; "c" ] = [ "a"; "b"; "c" ]);;
assert (insert_at "b" 2 [ "a"; "c" ] = [ "a"; "c"; "b" ]);;
assert (insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "alfa"; "b"; "c"; "d" ])

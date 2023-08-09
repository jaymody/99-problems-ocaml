(* Tail recursive version *)
(*
   let remove_at n list =
   let rec aux i acc = function
   | [] -> acc
   | h :: t -> aux (i - 1) (if i = 0 then acc else h :: acc) t
   in
   List.rev (aux n [] list)
   ;;
*)

let rec remove_at n = function
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t
;;

let%test _ = remove_at 0 [] = []
let%test _ = remove_at 1 [] = []
let%test _ = remove_at 0 [ "a" ] = []
let%test _ = remove_at 1 [ "a" ] = [ "a" ]
let%test _ = remove_at 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "c"; "d" ]
let%test _ = remove_at 0 [ "a"; "b"; "c"; "d" ] = [ "b"; "c"; "d" ]
let%test _ = remove_at 3 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c" ]
let%test _ = remove_at 4 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "d" ]

let drop list n =
  let rec aux i acc = function
    | [] -> acc
    | h :: t -> if i = n then aux 1 acc t else aux (i + 1) (h :: acc) t
  in
  List.rev (aux 1 [] list)
;;

let%test _ = drop [] 3 = []
let%test _ = drop [ "a"; "b"; "c"; "d"; "e" ] 1 = []
let%test _ = drop [ "a"; "b"; "c"; "d"; "e" ] 2 = [ "a"; "c"; "e" ]

let%test _ =
  drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
;;

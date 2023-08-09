let duplicate list =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: h :: acc) t
  in
  List.rev (aux [] list)
;;

let%test _ = duplicate [] = []
let%test _ = duplicate [ "a" ] = [ "a"; "a" ]
let%test _ = duplicate [ "a"; "a" ] = [ "a"; "a"; "a"; "a" ]

let%test _ =
  duplicate [ "a"; "b"; "c"; "c"; "d" ]
  = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]
;;

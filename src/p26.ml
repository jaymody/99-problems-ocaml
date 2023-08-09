let combinations n list =
  let rec aux n path acc list =
    match n, list with
    | 0, _ -> path :: acc
    | _, [] -> acc
    | n, h :: t ->
      let acc = aux (n - 1) (h :: path) acc t in
      aux n path acc t
  in
  aux n [] [] list
;;

let%test _ = combinations 0 [ "a"; "b"; "c"; "d" ] = [ [] ]

let%test _ =
  combinations 1 [ "a"; "b"; "c"; "d" ] = [ [ "d" ]; [ "c" ]; [ "b" ]; [ "a" ] ]
;;

let%test _ =
  combinations 2 [ "a"; "b"; "c"; "d" ]
  = [ [ "d"; "c" ]; [ "d"; "b" ]; [ "c"; "b" ]; [ "d"; "a" ]; [ "c"; "a" ]; [ "b"; "a" ] ]
;;

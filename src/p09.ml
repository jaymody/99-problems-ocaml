let collect x =
  let rec aux acc = function
    | [] -> acc, []
    | h :: t -> if h = x then aux (h :: acc) t else acc, h :: t
  in
  aux []
;;

let pack list =
  let rec aux acc = function
    | [] -> acc
    | h :: _ as list ->
      let x, t = collect h list in
      aux (x :: acc) t
  in
  List.rev (aux [] list)
;;

let%test _ = pack [] = []
let%test _ = pack [ "a" ] = [ [ "a" ] ]
let%test _ = pack [ "a"; "a" ] = [ [ "a"; "a" ] ]
let%test _ = pack [ "a"; "b"; "c"; "a" ] = [ [ "a" ]; [ "b" ]; [ "c" ]; [ "a" ] ]

let%test _ =
  pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
  = [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
;;

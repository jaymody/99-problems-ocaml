let split list n =
  let rec aux i left = function
    | [] -> left, []
    | h :: t -> if i = n then left, h :: t else aux (i + 1) (h :: left) t
  in
  let left, right = aux 0 [] list in
  List.rev left, right
;;

let%test _ = split [] 0 = ([], [])
let%test _ = split [] 1 = ([], [])
let%test _ = split [] 2 = ([], [])
let%test _ = split [ "a" ] 0 = ([], [ "a" ])
let%test _ = split [ "a" ] 1 = ([ "a" ], [])
let%test _ = split [ "a"; "b"; "c" ] 1 = ([ "a" ], [ "b"; "c" ])

let%test _ =
  split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
;;

let%test _ = split [ "a"; "b"; "c"; "d" ] 5 = ([ "a"; "b"; "c"; "d" ], [])

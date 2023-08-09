let replicate list n =
  let rec add x i acc = if i = 0 then acc else add x (i - 1) (x :: acc) in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (add h n acc) t
  in
  List.rev (aux [] list)
;;

let%test _ = replicate [] 3 = []
let%test _ = replicate [ "a" ] 1 = [ "a" ]
let%test _ = replicate [ "a" ] 4 = [ "a"; "a"; "a"; "a" ]
let%test _ = replicate [ "a"; "a" ] 2 = [ "a"; "a"; "a"; "a" ]

let%test _ =
  replicate [ "a"; "b"; "c" ] 3 = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
;;

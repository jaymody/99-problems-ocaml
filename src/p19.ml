open P17

let rotate list n =
  let left, right = split list n in
  right @ left
;;

let%test _ = rotate [] 3 = []
let%test _ = rotate [ "a" ] 3 = [ "a" ]
let%test _ = rotate [ "a"; "b" ] 0 = [ "a"; "b" ]
let%test _ = rotate [ "a"; "b" ] 1 = [ "b"; "a" ]
let%test _ = rotate [ "a"; "b" ] 2 = [ "a"; "b" ]

let%test _ =
  rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
  = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
;;

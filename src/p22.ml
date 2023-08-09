let range l r =
  let rec aux i j acc =
    if i != j then aux i (if j > i then j - 1 else j + 1) (j :: acc) else j :: acc
  in
  aux l r []
;;

let%test _ = range 4 4 = [ 4 ]
let%test _ = range 4 5 = [ 4; 5 ]
let%test _ = range 4 9 = [ 4; 5; 6; 7; 8; 9 ]
let%test _ = range 0 5 = [ 0; 1; 2; 3; 4; 5 ]
let%test _ = range 9 4 = [ 9; 8; 7; 6; 5; 4 ]

let combinations n list =
  let rec aux n list path acc =
    if n = 0 then path :: acc
    else if List.length list >= n then
      match list with
      | head :: tail ->
          let acc = aux (n - 1) tail (head :: path) acc in
          aux n tail path acc
      | [] -> [ [] ]
    else acc
  in
  aux n list [] []
;;

assert (combinations 0 [ "a"; "b"; "c"; "d" ] = [ [] ]);;

assert (
  combinations 1 [ "a"; "b"; "c"; "d" ] = [ [ "d" ]; [ "c" ]; [ "b" ]; [ "a" ] ])
;;

assert (
  combinations 2 [ "a"; "b"; "c"; "d" ]
  = [
      [ "d"; "c" ];
      [ "d"; "b" ];
      [ "c"; "b" ];
      [ "d"; "a" ];
      [ "c"; "a" ];
      [ "b"; "a" ];
    ])

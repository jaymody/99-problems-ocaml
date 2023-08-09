let rand_select list n =
  let len = List.length list in
  let rec aux n acc =
    match n with
    | 0 -> acc
    | _ -> aux (n - 1) (List.nth list (Random.int len) :: acc)
  in
  aux n []
;;

let%test _ = rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 0 = []

let%test _ =
  Random.init 123;
  rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 1 = [ "f" ]
;;

let%test _ =
  Random.init 123;
  rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3 = [ "b"; "g"; "f" ]
;;

let%test _ =
  Random.init 123;
  rand_select [ "a"; "b"; "c" ] 10 = [ "c"; "b"; "c"; "b"; "b"; "c"; "a"; "a"; "a"; "c" ]
;;

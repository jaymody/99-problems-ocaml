let lotto_select n m =
  let rec aux n acc = if n = 0 then acc else aux (n - 1) (Random.int (m + 1) :: acc) in
  aux n []
;;

let%test _ =
  Random.init 100;
  lotto_select 10 5 = [ 4; 0; 5; 1; 2; 4; 3; 3; 1; 2 ]
;;

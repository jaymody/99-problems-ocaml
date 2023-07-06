let lotto_select n m =
  let rec fn i l = if i = 0 then l else fn (i - 1) ((Random.int m + 1) :: l) in
  fn n []
;;

let () = Random.init 100 in
assert (lotto_select 6 49 = [ 10; 46; 46; 32; 49; 26 ])

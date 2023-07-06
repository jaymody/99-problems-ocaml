let range i j =
  let rec fn n acc =
    if n >= 0 then fn (n - 1) ((if j > i then i + n else i - n) :: acc) else acc
  in
  fn (abs (j - i)) []
;;

assert (range 4 4 = [ 4 ]);;
assert (range 4 5 = [ 4; 5 ]);;
assert (range 4 9 = [ 4; 5; 6; 7; 8; 9 ]);;
assert (range 0 5 = [ 0; 1; 2; 3; 4; 5 ]);;
assert (range 9 4 = [ 9; 8; 7; 6; 5; 4 ])

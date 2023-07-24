(*
Explanation of the algorithm:

1) Starting from d=2 and we check if n is divisible by d
2) If it is, we add d to the result and we recurse with d=d and n=n/d, that is
   we've found a prime divisor so we can "remove" it from n by dividing it away,
   so the problem is reduced to finding prime factors of n / d. Because we
   recurse with d=d, this means we will always find the factors in ascending
   order and thus we need not reset d=2.
3) But how do we know that d is prime? Consider a non-prime d. A non-prime d
   would have prime factors that are less than it. If n is divisible by d,
   then it is also divisible by it's prime factors, which would have been caught
   earlier in the recursion since the prime factors are less than it. By the
   time we reach a non-prime d that was a factor of n, those prime factors will
   have been divided away from n leaving an n that is no longer divisible by
   that d, whose prime factors will have been added to the answer.
*)
let factors n =
  let rec aux d = function
    | 1 -> []
    | n -> if n mod d = 0 then d :: aux d (n / d) else aux (d + 1) n
  in
  aux 2 n
;;

assert (factors 315 = [ 3; 3; 5; 7 ]);;
assert (factors 1481094 = [ 2; 3; 3; 107; 769 ])

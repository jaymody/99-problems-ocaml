(*
   Why does euclid's algorithm work?

   Firstly, to formalize the problem, given two integers a and b, we are trying
   to find the largest possible d such that:

   a = d * m1
   b = d * m2

   That is, we're trying to find the greatest common factor of a and b.

   The division of a by b can be characterized as:

   a = b * q + r

   If we substitute in b = d * m2:

   a = d * m2 * q + r

   And factor out d:

   a = d * (m2 * q + r / d)

   This suggest:

   m1 = m2 * q + r / d

   Since m1 needs to be an integer, and m2 * q is an integer, this means that
   r / d must also be an integer. In other words, r must also be divisible by d.

   If we assume that b <= a, then d <= b, and thus the problem is reduced to:

   gcd (a, b) = gcd (b, r)

   The trivial case is when b = 0, in which case we just return a.

   Okay, but the below code doens't assume b <= a? This ends up making no
   difference because if a < b, then r = a and so we get:

   gcd (a, b) = gcd (b, r) = gcd (b, a)

   Which get's us back to the case when the first arg is less than the second.
*)

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let%test _ = gcd 42 56 = 14
let%test _ = gcd 461952 116298 = 18
let%test _ = gcd 314080416 7966496 = 32
let%test _ = gcd 24826148 45296490 = 526
let%test _ = gcd 12 0 = 12
let%test _ = gcd 0 12 = 12
let%test _ = gcd 0 0 = 0
let%test _ = gcd 0 9 = 9
let%test _ = gcd 13 27 = 1
let%test _ = gcd 20536 7826 = 2

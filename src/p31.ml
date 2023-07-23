open P30

let coprime n m = gcd n m = 1;;

assert (coprime 13 27);;
assert (not (coprime 20536 7826))

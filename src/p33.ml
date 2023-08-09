open P32

let coprime n m = gcd n m = 1
let%test _ = coprime 13 27
let%test _ = not (coprime 20536 7826)

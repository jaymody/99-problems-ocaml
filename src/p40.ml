let goldbach n =
  let primes = P39.all_primes 2 n in
  let rec aux fwd rev =
    match fwd, rev with
    | a :: _, c :: _ when a + c = n -> a, c
    | a :: _, c :: t when a + c > n -> aux fwd t
    | a :: t, c :: _ when a + c < n -> aux t rev
    | _ -> raise Not_found
  in
  aux primes (List.rev primes)
;;

assert (goldbach 28 = (5, 23))

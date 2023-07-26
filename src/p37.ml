(* Sieve of Eratosthenes *)
let all_primes a b =
  let is_prime_arr = Array.init (b + 1) (fun n -> n) in
  let rec fill_primes m =
    let rec aux n =
      if n <= b then (
        Array.set is_prime_arr n 0;
        aux (n + m))
    in
    if m <= b / 2 then (
      if Array.get is_prime_arr m <> 0 then aux (2 * m);
      fill_primes (m + 1))
  in
  fill_primes 2;
  is_prime_arr |> Array.to_list |> List.filter (fun n -> n <> 0 && n >= a)
;;

assert (
  all_primes 2 97
  = [
      2;
      3;
      5;
      7;
      11;
      13;
      17;
      19;
      23;
      29;
      31;
      37;
      41;
      43;
      47;
      53;
      59;
      61;
      67;
      71;
      73;
      79;
      83;
      89;
      97;
    ])

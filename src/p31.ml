let is_prime n =
  if n <= 1
  then false
  else (
    let rec aux d = d * d <= n && (n mod d = 0 || aux (d + 1)) in
    not (aux 2))
;;

assert (
  List.init 20 (fun n -> n, is_prime n)
  = [ 0, false
    ; 1, false
    ; 2, true
    ; 3, true
    ; 4, false
    ; 5, true
    ; 6, false
    ; 7, true
    ; 8, false
    ; 9, false
    ; 10, false
    ; 11, true
    ; 12, false
    ; 13, true
    ; 14, false
    ; 15, false
    ; 16, false
    ; 17, true
    ; 18, false
    ; 19, true
    ])

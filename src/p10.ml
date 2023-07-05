let encode l =
  let rec fn l n o =
    match l with
    | a :: b :: c ->
        if a = b then fn (b :: c) (n + 1) o else fn (b :: c) 1 ((n, a) :: o)
    | [ x ] -> (n, x) :: o
    | [] -> o
  in
  List.rev (fn l 1 [])
;;

assert (encode [] = []);;
assert (encode [ "c" ] = [ (1, "c") ]);;
assert (encode [ "a"; "a"; "a"; "a" ] = [ (4, "a") ]);;

assert (
  encode [ "a"; "a"; "b"; "b"; "a"; "a" ] = [ (2, "a"); (2, "b"); (2, "a") ])
;;

assert (
  encode
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ])

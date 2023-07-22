let encode list =
  let rec aux n acc = function
    | x :: (y :: _ as t) ->
        if x = y then aux (n + 1) acc t else aux 1 ((n, x) :: acc) t
    | [ x ] -> (n, x) :: acc
    | [] -> acc
  in
  List.rev (aux 1 [] list)
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

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode list =
  let create_rle n x = if n = 1 then One x else Many (n, x) in
  let rec aux n acc = function
    | x :: (y :: _ as t) ->
      if x = y then aux (n + 1) acc t else aux 1 (create_rle n x :: acc) t
    | [ x ] -> create_rle n x :: acc
    | [] -> acc
  in
  List.rev (aux 1 [] list)
;;

assert (
  encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ])

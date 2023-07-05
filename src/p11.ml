type 'a rle = One of 'a | Many of int * 'a

let encode l =
  let g n a = if n = 1 then One a else Many (n, a) in
  let rec fn l n o =
    match l with
    | a :: (b :: _ as rest) ->
        if a = b then fn rest (n + 1) o else fn rest 1 (g n a :: o)
    | [ x ] -> g n x :: o
    | [] -> o
  in
  List.rev (fn l 1 [])
;;

assert (
  encode
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [
      Many (4, "a");
      One "b";
      Many (2, "c");
      Many (2, "a");
      One "d";
      Many (4, "e");
    ])

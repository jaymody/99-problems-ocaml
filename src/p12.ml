type 'a rle = One of 'a | Many of (int * 'a)

let decode l =
  let rec fn l o =
    match l with
    | Many (n, x) :: rest -> fn rest (List.init n (fun _ -> x) @ o)
    | One x :: rest -> fn rest (x :: o)
    | [] -> o
  in
  List.rev (fn l [])
;;

assert (
  decode
    [
      Many (4, "a");
      One "b";
      Many (2, "c");
      Many (2, "a");
      One "d";
      Many (4, "e");
    ]
  = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ])

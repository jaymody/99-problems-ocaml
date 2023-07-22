type 'a rle = One of 'a | Many of (int * 'a)

let decode list =
  let rec add x n acc = if n = 0 then acc else add x (n - 1) (x :: acc) in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> (
        match h with
        | Many (n, x) -> aux (add x n acc) t
        | One x -> aux (x :: acc) t)
  in
  List.rev (aux [] list)
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

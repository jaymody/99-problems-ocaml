let collect x =
  let rec aux acc = function
    | [] -> acc, []
    | h :: t -> if h = x then aux (h :: acc) t else acc, h :: t
  in
  aux []
;;

let pack list =
  let rec aux acc = function
    | [] -> acc
    | h :: _ as list ->
      let x, t = collect h list in
      aux (x :: acc) t
  in
  List.rev (aux [] list)
;;

assert (pack [] = []);;
assert (pack [ "a" ] = [ [ "a" ] ]);;
assert (pack [ "a"; "a" ] = [ [ "a"; "a" ] ]);;
assert (pack [ "a"; "b"; "c"; "a" ] = [ [ "a" ]; [ "b" ]; [ "c" ]; [ "a" ] ]);;

assert (
  pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
  = [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ])

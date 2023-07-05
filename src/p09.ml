let pack list =
  let rec inner l acc chr =
    match (l, acc) with
    | h :: t, _ -> if h = chr then inner t (h :: acc) chr else (l, acc)
    | _ -> (l, acc)
  in
  let rec outer l acc =
    match (l, acc) with
    | h :: t, _ ->
        let a, b = inner t [ h ] h in
        outer a (b :: acc)
    | [], _ -> acc
  in
  List.rev (outer list [])
;;

assert (pack [] = []);;
assert (pack [ "a" ] = [ [ "a" ] ]);;
assert (pack [ "a"; "a" ] = [ [ "a"; "a" ] ]);;
assert (pack [ "a"; "b"; "c"; "a" ] = [ [ "a" ]; [ "b" ]; [ "c" ]; [ "a" ] ]);;

assert (
  pack
    [
      "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e";
    ]
  = [
      [ "a"; "a"; "a"; "a" ];
      [ "b" ];
      [ "c"; "c" ];
      [ "a"; "a" ];
      [ "d"; "d" ];
      [ "e"; "e"; "e"; "e" ];
    ])

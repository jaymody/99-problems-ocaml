let permutation list =
  let pop n list =
    let rec aux n acc = function
      | [] -> raise Not_found
      | h :: t -> if n = 0 then h, acc @ t else aux (n - 1) (h :: acc) t
    in
    aux n [] list
  in
  let rec aux n acc list =
    match n with
    | 0 -> acc
    | _ ->
      let x, rest = pop (Random.int n) list in
      aux (n - 1) (x :: acc) rest
  in
  aux (List.length list) [] list
;;

let%test _ = permutation [] = []
let%test _ = permutation [ "a" ] = [ "a" ]

let%test _ =
  Random.init 123;
  permutation [ "a"; "b"; "c"; "d" ] = [ "c"; "d"; "a"; "b" ]
;;

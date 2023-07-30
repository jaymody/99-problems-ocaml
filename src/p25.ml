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

assert (permutation [] = []);;
assert (permutation [ "a" ] = [ "a" ]);;

let () = Random.init 123 in
assert (permutation [ "a"; "b"; "c"; "d" ] = [ "c"; "d"; "a"; "b" ])

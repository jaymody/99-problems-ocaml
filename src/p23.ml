let pop_at index list =
  let rec aux i l =
    match l with
    | [] -> raise Not_found
    | h :: t ->
        if i = 0 then (h, t)
        else
          let elem, rest = aux (i - 1) t in
          (elem, h :: rest)
  in
  aux index list
;;

assert (pop_at 0 [ "a" ] = ("a", []));;
assert (pop_at 1 [ "a"; "b"; "c"; "d" ] = ("b", [ "a"; "c"; "d" ]));;
assert (pop_at 0 [ "a"; "b"; "c"; "d" ] = ("a", [ "b"; "c"; "d" ]));;
assert (pop_at 3 [ "a"; "b"; "c"; "d" ] = ("d", [ "a"; "b"; "c" ]))

let rand_select l n =
  let rec fn l n o =
    if n > 0 then
      let elem, rest = pop_at (Random.int (List.length l)) l in
      fn rest (n - 1) (elem :: o)
    else o
  in
  fn l n []
;;

let () = Random.init 100 in
assert (
  rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3 = [ "e"; "h"; "a" ])

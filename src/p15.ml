let replicate list n =
  let rec add x i acc = if i = 0 then acc else add x (i - 1) (x :: acc) in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (add h n acc) t
  in
  List.rev (aux [] list)
;;

assert (replicate [] 3 = []);;
assert (replicate [ "a" ] 1 = [ "a" ]);;
assert (replicate [ "a" ] 4 = [ "a"; "a"; "a"; "a" ]);;
assert (replicate [ "a"; "a" ] 2 = [ "a"; "a"; "a"; "a" ]);;
assert (replicate [ "a"; "b"; "c" ] 3 = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ])

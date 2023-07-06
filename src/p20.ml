let remove_at i l =
  let rec fn i l o =
    match l with h :: t -> fn (i - 1) t (if i = 0 then o else h :: o) | _ -> o
  in
  List.rev (fn i l [])
;;

assert (remove_at 0 [] = []);;
assert (remove_at 1 [] = []);;
assert (remove_at 0 [ "a" ] = []);;
assert (remove_at 1 [ "a" ] = [ "a" ]);;
assert (remove_at 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "c"; "d" ]);;
assert (remove_at 0 [ "a"; "b"; "c"; "d" ] = [ "b"; "c"; "d" ]);;
assert (remove_at 3 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c" ]);;
assert (remove_at 4 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "d" ])

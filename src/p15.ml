let replicate l n =
  let rec dup a n o = if n = 0 then o else dup a (n - 1) (a :: o) in
  let rec fn l o = match l with h :: t -> fn t (dup h n o) | _ -> o in
  fn (List.rev l) []
;;

assert (
  replicate [ "a"; "b"; "c" ] 3
  = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ])

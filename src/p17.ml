let split l n =
  let rec fn l r n =
    match r with
    | h :: t -> if n = 0 then (l, r) else fn (h :: l) t (n - 1)
    | _ -> (l, r)
  in
  let l, r = fn [] l n in
  (List.rev l, r)
;;

assert (split [] 0 = ([], []));;
assert (split [] 1 = ([], []));;
assert (split [] 2 = ([], []));;
assert (split [ "a" ] 0 = ([], [ "a" ]));;
assert (split [ "a" ] 1 = ([ "a" ], []));;
assert (split [ "a"; "b"; "c" ] 1 = ([ "a" ], [ "b"; "c" ]));;

assert (
  split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]))
;;

assert (split [ "a"; "b"; "c"; "d" ] 5 = ([ "a"; "b"; "c"; "d" ], []))

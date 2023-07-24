let rec factors = function
  | 0 | 1 -> []
  | n ->
      let a =
        Option.get
          (Seq.find (fun x -> n mod x = 0) (Seq.init (n - 1) (fun x -> x + 2)))
      in
      let b = n / a in
      if a = 1 || b = 1 then [ max a b ] else factors a @ factors b
;;

assert (factors 315 = [ 3; 3; 5; 7 ]);;
assert (factors 1481094 = [ 2; 3; 3; 107; 769 ])

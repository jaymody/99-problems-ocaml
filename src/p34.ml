let factors n = P33.factors n |> P10.encode |> List.map (fun (a, b) -> (b, a));;

assert (factors 315 = [ (3, 2); (5, 1); (7, 1) ])

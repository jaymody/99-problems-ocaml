let goldbach_list a b =
  let rec aux n acc = if n > b then acc else aux (n + 2) ((n, P40.goldbach n) :: acc) in
  List.rev (aux (if a mod 2 = 0 then a else a + 1) [])
;;

let goldbach_limit a b lim =
  List.filter (fun (_, (a, b)) -> a > lim && b > lim) (goldbach_list a b)
;;

assert (
  goldbach_list 9 20
  = [ 10, (3, 7); 12, (5, 7); 14, (3, 11); 16, (3, 13); 18, (5, 13); 20, (3, 17) ])
;;

Printf.printf "%d\n" (List.length (goldbach_limit 3 3000 50))

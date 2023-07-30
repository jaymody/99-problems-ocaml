let compress list =
  let rec aux acc = function
    | x :: y :: t -> aux (if x = y then acc else x :: acc) (y :: t)
    | x :: _ -> x :: acc
    | _ -> acc
  in
  List.rev (aux [] list)
;;

assert (compress [] = []);;
assert (compress [ 0 ] = [ 0 ]);;
assert (compress [ 0; 0 ] = [ 0 ]);;
assert (compress [ 0; 0; 1; 0; 0 ] = [ 0; 1; 0 ]);;
assert (compress [ 0; 1; 0; 1; 0 ] = [ 0; 1; 0; 1; 0 ]);;

assert (
  compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ])

(* makes sure our implementation is efficient enough to run on a really long list *)
let _ = compress (List.init 10000000 (fun x -> x))

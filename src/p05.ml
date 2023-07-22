let rev =
  let rec aux acc = function [] -> acc | h :: t -> aux (h :: acc) t in
  aux []
;;

assert (rev [] = []);;
assert (rev [ 1 ] = [ 1 ]);;
assert (rev [ 1; 2 ] = [ 2; 1 ]);;
assert (rev [ 1; 1 ] = [ 1; 1 ]);;
assert (rev [ 1; 6; 2 ] = [ 2; 6; 1 ]);;
assert (rev [ 8; 1; 3; 4 ] = [ 4; 3; 1; 8 ]);;
assert (rev [ 0; 0; 0; 0 ] = [ 0; 0; 0; 0 ]);;
assert (rev [ 4; 1; 1; -4 ] = [ -4; 1; 1; 4 ]);;

(* makes sure our implementation is efficient enough to run on a really long list *)
rev (List.init 1000000 (fun x -> x + 1))

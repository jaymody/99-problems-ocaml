(*
Initial implementation.

let rev =
    let rec fn = function
        | [] -> []
        | h :: t -> (fn t) @ [ h ]
    in fn
;;

Note, this is not tail recursive (we need to keep track of [ h ] at each
recursive call), meaning we end up allocating stack space for each function.

The solution (below) on the other hand is tail recursive (notice, each function
call is only reliant on the input arguments, there are no "local" variables) and
thus is more efficient, as we don't need to allocate any stack space (just ends
up being a loop).
*)

let rev =
  let rec fn new_list = function
    | [] -> new_list
    | h :: t -> fn (h :: new_list) t
  in
  fn []
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

let head = function x :: _ -> Some x | [] -> None

let compress list =
  let rec fn old_list new_list =
    match (old_list, new_list) with
    | h :: t, _ ->
        fn t (if Some h = head new_list then new_list else h :: new_list)
    | _ -> new_list
  in
  List.rev (fn list [])
;;

assert (compress [] = []);;
assert (compress [ 0 ] = [ 0 ]);;
assert (compress [ 0; 0 ] = [ 0 ]);;
assert (compress [ 0; 0; 1; 0; 0 ] = [ 0; 1; 0 ]);;
assert (compress [ 0; 1; 0; 1; 0 ] = [ 0; 1; 0; 1; 0 ]);;

assert (
  compress
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ])

(* makes sure our implementation is efficient enough to run on a really long list *)
let _ = compress (List.init 10000000 (fun x -> x))

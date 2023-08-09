let length =
  let rec aux n = function
    | [] -> n
    | _ :: t -> aux (n + 1) t
  in
  aux 0
;;

let%test _ = length [] = 0
let%test _ = length [ 1 ] = 1
let%test _ = length [ 1; 2 ] = 2
let%test _ = length [ 1; 6; 2 ] = 3
let%test _ = length [ 8; 1; 3; 4 ] = 4
let%test _ = length [ 0; 0; 0; 0 ] = 4
let%test _ = length [ 4; 1; 1; -4 ] = 4

(* makes sure our implementation is efficient enough to run on a really long list *)
let%test _ = length (List.init 1000000 (fun x -> x + 1)) = 1000000

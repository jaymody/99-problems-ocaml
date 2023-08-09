let rec last_two = function
  | [] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: t -> last_two t
;;

let%test _ = last_two [] = None
let%test _ = last_two [ 1 ] = None
let%test _ = last_two [ 1; 5 ] = Some (1, 5)
let%test _ = last_two [ 1; 5; 10 ] = Some (5, 10)
let%test _ = last_two [ 1; 5; 10; -5 ] = Some (10, -5)

(* makes sure our implementation is efficient enough to run on a really long list *)
let%test _ = last_two (List.init 1000000 (fun x -> x + 1)) = Some (999999, 1000000)

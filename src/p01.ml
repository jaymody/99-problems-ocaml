let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t
;;

let%test _ = last [] = None
let%test _ = last [ 1 ] = Some 1
let%test _ = last [ 1; 5 ] = Some 5
let%test _ = last [ 1; 5; 10 ] = Some 10
let%test _ = last [ 1; 5; 10; -5 ] = Some (-5)

(* makes sure our implementation is efficient enough to run on a really long list *)
let%test _ = last (List.init 1000000 (fun x -> x + 1)) = Some 1000000

(* Damn OCaml really doesn't have an integer power function eh? *)
let rec pow b n = if n < 1 then 1 else b * pow b (n - 1)

let phi_improved n =
  P36.factors n |> List.fold_left (fun prod (p, m) -> prod * ((p - 1) * pow p (m - 1))) 1
;;

let%test _ = phi_improved 10 = 4

let%test _ =
  List.init 49 (fun n -> n + 2, phi_improved (n + 2))
  = [ 2, 1
    ; 3, 2
    ; 4, 2
    ; 5, 4
    ; 6, 2
    ; 7, 6
    ; 8, 4
    ; 9, 6
    ; 10, 4
    ; 11, 10
    ; 12, 4
    ; 13, 12
    ; 14, 6
    ; 15, 8
    ; 16, 8
    ; 17, 16
    ; 18, 6
    ; 19, 18
    ; 20, 8
    ; 21, 12
    ; 22, 10
    ; 23, 22
    ; 24, 8
    ; 25, 20
    ; 26, 12
    ; 27, 18
    ; 28, 12
    ; 29, 28
    ; 30, 8
    ; 31, 30
    ; 32, 16
    ; 33, 20
    ; 34, 16
    ; 35, 24
    ; 36, 12
    ; 37, 36
    ; 38, 18
    ; 39, 24
    ; 40, 16
    ; 41, 40
    ; 42, 12
    ; 43, 42
    ; 44, 20
    ; 45, 24
    ; 46, 22
    ; 47, 46
    ; 48, 16
    ; 49, 42
    ; 50, 20
    ]
;;

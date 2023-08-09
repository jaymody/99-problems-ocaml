let timeit f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "%fs\n" (Sys.time () -. t);
  fx
;;

(* timeit P34.phi 1209010;; *)
(* timeit P37.phi_improved 1209010;; *)

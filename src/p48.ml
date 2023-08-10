let rec gray = function
  | 1 -> [ "0"; "1" ]
  | n ->
    let codes = gray (n - 1) in
    List.map (fun x -> "0" ^ x) codes @ List.map (fun x -> "1" ^ x) (List.rev codes)
;;

let%test _ = gray 1 = [ "0"; "1" ]
let%test _ = gray 2 = [ "00"; "01"; "11"; "10" ]
let%test _ = gray 3 = [ "000"; "001"; "011"; "010"; "110"; "111"; "101"; "100" ]

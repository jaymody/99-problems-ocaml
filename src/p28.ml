let length_sort list =
  list
  |> List.map (fun l -> (List.length l, l))
  |> List.sort (fun (len1, _) (len2, _) -> compare len1 len2)
  |> List.map snd
;;

assert (
  length_sort
    [
      [ "a"; "b"; "c" ];
      [ "d"; "e" ];
      [ "f"; "g"; "h" ];
      [ "d"; "e" ];
      [ "i"; "j"; "k"; "l" ];
      [ "m"; "n" ];
      [ "o" ];
    ]
  = [
      [ "o" ];
      [ "d"; "e" ];
      [ "d"; "e" ];
      [ "m"; "n" ];
      [ "a"; "b"; "c" ];
      [ "f"; "g"; "h" ];
      [ "i"; "j"; "k"; "l" ];
    ])

module IntMap = Map.Make (Int)

let frequency_sort list =
  let list = List.map (fun l -> (List.length l, l)) list in
  let freqs =
    let inc = function None -> Some 1 | Some n -> Some (n + 1) in
    List.fold_left (fun m (len, _) -> IntMap.update len inc m) IntMap.empty list
  in
  list
  |> List.sort (fun (len1, _) (len2, _) ->
         compare (IntMap.find len1 freqs) (IntMap.find len2 freqs))
  |> List.map snd
;;

assert (
  frequency_sort
    [
      [ "a"; "b"; "c" ];
      [ "d"; "e" ];
      [ "f"; "g"; "h" ];
      [ "d"; "e" ];
      [ "i"; "j"; "k"; "l" ];
      [ "m"; "n" ];
      [ "o" ];
    ]
  = [
      [ "i"; "j"; "k"; "l" ];
      [ "o" ];
      [ "a"; "b"; "c" ];
      [ "f"; "g"; "h" ];
      [ "d"; "e" ];
      [ "d"; "e" ];
      [ "m"; "n" ];
    ])

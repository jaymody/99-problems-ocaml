let group list sizes =
  let add path subgroup = if path = [] then subgroup else path :: subgroup in
  let rec aux sizes subgroup path acc rest list =
    match (sizes, list) with
    | [], _ -> [ [] ]
    | [ 0 ], _ -> add path subgroup :: acc
    | 0 :: sizes, _ -> aux sizes (add path subgroup) [] acc [] (list @ rest)
    | _, [] -> acc
    | n :: sizes, h :: t ->
        let acc = aux ((n - 1) :: sizes) subgroup (h :: path) acc rest t in
        aux (n :: sizes) subgroup path acc (h :: rest) t
  in

  aux sizes [] [] [] [] list

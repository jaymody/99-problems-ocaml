let sym_cbal_trees n = List.filter P56.is_symmetric (P55.cbal_tree n)
let%test _ = List.length (sym_cbal_trees 57) = 256

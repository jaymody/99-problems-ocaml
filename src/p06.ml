let is_palindrome list = list = List.rev list
let%test _ = is_palindrome []
let%test _ = is_palindrome [ 1 ]
let%test _ = is_palindrome [ 10 ]
let%test _ = is_palindrome [ 10; 10 ]
let%test _ = is_palindrome [ "r"; "a"; "c"; "e"; "c"; "a"; "r" ]
let%test _ = not (is_palindrome [ 1; 2 ])
let%test _ = not (is_palindrome [ 1; 2; 2 ])

let is_palindrome list = list = List.rev list;;

assert (is_palindrome []);;
assert (is_palindrome [ 1 ]);;
assert (is_palindrome [ 10 ]);;
assert (is_palindrome [ 10; 10 ]);;
assert (is_palindrome [ "r"; "a"; "c"; "e"; "c"; "a"; "r" ]);;
assert (not (is_palindrome [ 1; 2 ]));;
assert (not (is_palindrome [ 1; 2; 2 ]))

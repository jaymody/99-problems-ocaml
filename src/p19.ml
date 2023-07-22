open P17

let rotate list n =
  let left, right = split list n in
  right @ left
;;

assert (rotate [] 3 = []);;
assert (rotate [ "a" ] 3 = [ "a" ]);;
assert (rotate [ "a"; "b" ] 0 = [ "a"; "b" ]);;
assert (rotate [ "a"; "b" ] 1 = [ "b"; "a" ]);;
assert (rotate [ "a"; "b" ] 2 = [ "a"; "b" ]);;

assert (
  rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
  = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ])

open P17

let rotate l n =
  let l, r = split l n in
  r @ l
;;

assert (rotate [] 3 = []);;
assert (rotate [ "a" ] 3 = [ "a" ]);;
assert (rotate [ "a"; "b" ] 0 = [ "a"; "b" ]);;
assert (rotate [ "a"; "b" ] 1 = [ "b"; "a" ]);;
assert (rotate [ "a"; "b" ] 2 = [ "a"; "b" ]);;

assert (
  rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
  = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ])

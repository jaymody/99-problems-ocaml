Solutions for [ocaml.org/problems](https://ocaml.org/problems).

### Usage
Dependencies:
```shell
opam switch create . -w --with-test
```

Build project:
```shell
dune build
```

Run inline tests:
```shell
dune runtest
```

Usage as library:
```ocaml
Problems.P31.is_prime 7;;
- : bool = true
```

Resources:
- [Real World OCaml](https://dev.realworldocaml.org/index.html)
- [Official OCaml Docs](https://v2.ocaml.org/manual/index.html)

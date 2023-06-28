set -e
eval $(opam env)
for filename in ./p*.ml; do
    echo "--- running test for $filename ---"
    ocamlopt $filename && ./a.out
    echo "test passed"
done

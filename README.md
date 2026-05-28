# Experiments with Generators for Hindley-Milner 

See [`lib/typecheck.ml`](./lib/typecheck.ml).

## Compiling
- Run `dune build` to compile
- Run `dune build @fmt --auto-promote` to automatically format / lint the OCaml code
- Run `dune exec -- main` to run all unit tests 
- To evaluate in a REPL, do `dune utop`, then type the following into Utop:
```
#use "typecheck.ml";;
```
To quit the REPL, type `#quit;;`.   
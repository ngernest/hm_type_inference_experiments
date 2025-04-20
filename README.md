# CS 6110 HW5 code

The starter code provided is in [`lib/typecheck.ml`](./lib/typecheck.ml).

## Compiling
- Run `dune build` to compile
- Run `dune build @fmt --auto-promote` to automatically format / lint the OCaml code
- To evaluate in a REPL, do `dune utop`, then type the following into Utop:
```
#use "typecheck.ml";;
```
To quit the REPL, type `#quit;;`.   
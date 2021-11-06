# Writing-An-Interpreter-In-Go-In-OCaml

implementation of [Writing An Interpreter In Go](https://interpreterbook.com/) in **OCaml**

## REPL

You need 

- dune 2.9.1
- opam 2.1.0
- OCaml 4.12.1
- Alcotest 1.5.0 
- fmt 0.8.10

then run

```

dune build bin/main.exe
dune exec bin/main.exe

```

## TODO

- refactoring
  - change some functions to use mutually recursive definition
  - better performance
  - implementing in more OCaml way. Now, codes are implemented as it is written in the book.
- precise error message
- hash table
- for statement or while statement

# pp_loc

Decent error reporting (for example, in a parser, a compiler, ...) typically
involves collecting *locations*, in order to indicate to the user the position
of an error in the source file.

This library provides support to additionally quote and highlight the input
fragment the input that corresponds to a location (or a set of locations).

This is the same code as in the OCaml compiler implementation (as of version
4.08), but extracted as a standalone library.

## Examples

Single-line errors: the offending line is printed back, and the error location
is highlighted using carets.

```
2 | let x = 1 + 1 +. 2 in ()
            ^^^^^
```

Multi-line error: the lines that span the location are printed, where the input
outside of the error location is replaced by ".".

```
2 | ......(1 + 1
3 |  * 3)...
```

## Documentation

https://armael.github.io/pp_loc/pp_loc/

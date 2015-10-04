# ``ppx_deriving_random``

This syntax extension can add a random generator for a custom type
definition.  Adding ``[@@deriving random]`` to the type definition ``foo``
will emit ``val random_foo : random_state -> foo``.  An additional argument
is prepended to the argument list for each type variable in the style of
other ``deriving`` extensions.  That is,

```ocaml
type ('a1, ..., 'aN) foo = ... [@@deriving random]
```

will provide

```ocaml
val random_foo : (random_state -> 'a1) ... (random_state -> 'aN) ->
                 random_state -> ('a1, ..., 'aN) foo
```

## Installation and Usage

If you use ``opam`` and don't mind adding another repo, you can install
``ppx_deriving_random`` with

```ocaml
opam repo add paurkedal https://github.com/paurkedal/opam-repo-paurkedal.git
opam install ppx_deriving_random
```

For manual installation from a fresh Git checkout, make sure you have OASIS
and ``ppx_deriving``, then
```shell
oasis setup -setup-update dynamic
ocaml setup.ml -configure --prefix your-prefix
make
make install
```
where ``your-prefix/lib`` is in your findlib path.  The syntax extension can
now be enabled with ``-package ppx_deriving_random.ppx`` or by putting
``package(ppx_deriving_random.ppx)`` in your ``_tags``.  To build the
example:
```shell
ocamlfind ocamlopt -linkpkg -package ppx_deriving_random.ppx -package ppx_deriving.show free_magma.ml
```

## Context

The following definitions are assumed to be in scope:

```ocaml
type random_state
(* State passed to the random generators. *)

val random_case : int -> random_state -> int
(* A function used to select a random constructor or row field. The first
   argument is the number of cases. The result must be in the range from 0
   to the number of cases minus one and should be uniformly distributed. *)

val random_case_30b : random_state -> int
(* When fields are weighted (see below), this is used instead of
   random_case. In the returned value, bit 29 and downwards to the desired
   precision must be random and bit 30 upwards must be zero. *)
```

These can be bound to ``Random.State.t``, ``Random.State.int``, and
``Random.State.bits``, respectively, but this is not done by default since
advanced applications may need to pass additional information in
``random_state`` or use different random number generators.

References to other types will produce additional calls to correspondingly
named generators. You should provide generators with suitable distributions
for you usage in scope of the type definition. If needed, the generator
used for a particular type reference ``u`` can be overridden with
``[@random expr]``, where ``expr : random_state -> u``. This allows using
different distributions for different instances of the same type. Another
way to tweak the distribution is to define type aliases with custom
random generators.

## Example

```ocaml
type 'a free_magma =
  | Fm_gen of 'a
  | Fm_mul of 'a free_magma * 'a free_magma
  [@@deriving random]
```

will generate

```ocaml
val random_free_magma : (random_state -> 'a) -> random_state -> 'a free_magma
```

However, there is a problem with the generated function.  By default all
branches of the variant is equally probable, leading to a possibly diverging
computation.  This can be fixed by weighing the constructors:

```ocaml
type 'a free_magma =
  | Fm_gen [@weight 3] of 'a
  | Fm_mul [@weight 2] of 'a free_magma * 'a free_magma
  [@@deriving random]
```

Weights can be int or float constants and defaults to 1.  The probability is
the weight divided by the sum of weights for the given variant.

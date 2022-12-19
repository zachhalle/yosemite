This is a type-directed compiler for a subset of ML. It's based on the starter code from Karl Crary's Higher Order Type Compilation course at Carnegie Mellon University, as well as my notes from that course, which I took in 2017.

The compiler will cover the ML core language. Modules may be added at a later time. The translation follows the strategy outlined in [Morrisett et al](https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf):

* Direct is cps-coverted to:
* Cps, which is closure coverted to:
* Closure, which is hoisted to:
* Hoist, which is annotated with explicit allocations, producing:
* Alloc, which is translated via a more-or-less syntax directed transformation that produces
* Typed Assembly Language, which can be used to codegen for an actual machine instruction set

Each of the above is a **complete**, independent progamming language on its own; at no point does the compiler rely on assumptions that the ILs have been produced by some specific translation pass.

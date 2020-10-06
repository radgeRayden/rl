This is an experiment in adapting the Scopes compiler to work as the backend for another language. It helped me understand some internals better, but is not meant to be useful, so please don't use it. The result is a lisp-like with some common special forms. It doesn't have a GC or tail call optimization, so it can't do complex things for very long without crashing.

As it happens, Scopes already uses S-expressions, so I reused the parser - this means we inherit certain syntactic elements like # starting a comment, for example. I also can't preclude the usage of "naked" (indentation based) S-expressions. 

Most of the fun work on this was smoothing out how values are treated so they are dynamically typed, whereas Scopes is fully typed.
Under the hood all functions have the type signature `RLValue f (int argc, RLValue* values)`, which helped my objective of freely passing around functions without having to run compiler stages.

You can see most of the supported features in the file `test.lisp`.

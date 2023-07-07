# Abstract machines

A simple and unified way for rewriting theory practitioners to quickly implement abstract machines for their rewriting systems. This includes the capabilities to define their own data types for syntax and, with that, the intended reduction relation.

Typical examples of rewriting systems include the different evaluation strategies from the [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus), with their differentiated operational semantics as well as their relatively similar yet distinct syntaxes. This library provides a re-usable derivation mechanism for their respective boilerplate code, thus saving programmers the trouble to re-implement common rewriting-theoretical parts during implementation and comparison of different systems.

Within the **examples** directory, there is a series of use cases showcasing how to define the syntax and operational semantics using the library. In each of them, the bulk of the evaluation strategy is defined in the **src/Types.hs** modules, whereas **src/Main.hs** simply provides a generic interface for running the corresponding abstract machine.

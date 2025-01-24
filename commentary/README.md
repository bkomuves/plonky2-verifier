Commentary on Plonky2
---------------------

These are my notes describing the inner workings of the [Plonky2](https://github.com/0xPolygonZero/plonky2/) proof system by Polygon Zero. As there is basically no existing documentation, this is a work of reverse-engineering.

A good place to start is [Overview.md](Overview.md).

Then more details can be found at:

- [Layout.md](Layout.md) - Layout of all the columns
- [Gates.md](Gates.md) - The different "custom gates" present in the Plonky2 code base
- [Selectors.md](Selectors.md) - Gate selectors and constants
- [GateConstraints.md](GateConstraints.md) - Gate constraint equations
- [Wiring.md](Wiring.md) - The permutation argument
- [Poseidon.md](Poseidon.md) - Poseidon hash function
- [FRI.md](FRI.md) - FRI commitment scheme
- [Challenges.md](Challenges.md) - Fiat-Shamir challenges
- [Protocol.md](Protocol.md) - Overview of the protocol
- [Lookups.md](Lookups.md) - Lookup gates and the lookup argument
- [Recursion.md](Recursion.md) - Recursive proofs

PDF version
-----------

Unfortunately, `github`'s support of LaTeX inside Markdown is very buggy.

As an experiment I tried to create a [PDF version](commentary.pdf) using
[Pandoc](https://pandoc.org/).
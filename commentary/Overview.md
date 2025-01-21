Overview of Plonky2
-------------------

[Plonky2](https://github.com/0xPolygonZero/plonky2/) is a proof system developed by Polygon Zero, based on "Plonkish" arithmetization and FRI polynomial commitments.

The primary design goal of Plonky2 was to allow very efficient recursive proofs, and it's still interesting in that aspect (the next-generation Plonky3 toolkit does not support recursion, or even Plonk circuits, at the time of writing this).

In this set of notes I try to describe the internal workings of Plonky2 in detail (as the original authors provided essentially no documentation at all...)

### Links to the topical notes

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

## Some basic design choices

Plonky2 uses a Plonkish arithmetization with wide rows and FRI polynomial commitment scheme, over a small (64-bit) field.

### Features

- Plonkish arithmetization: 
    - the witness is organized in a $2^n \times M$ matrix (called "advice wires"); 
    - the circuit is described by "gates" and wiring constraints
    - with optional lookup tables
- wide rows (by default $M = 135$)
- gates are single-row, and at most 1 gate in a row (no rotations a la Halo2)
- custom gates (any number of equations per gate)
- relatively high-degree gates (by default, up to 8)
- optimized for recursive proofs

Having such a large number of columns is not a problem in practice, because using FRI whole rows can be committed (and opened) at together. With KZG this would be rather expensive.

### Field choice

Plonky2 uses the Goldilocks field $\mathbb{F}_p$ with $p = 2^{64}-2^{32}+1$, and a degree two extension $\mathbb{F}_{p^2} = \widetilde{\mathbb{F}} := \mathbb{F}_{p}[X]/(X^2-7)$. This is essentially the smallest irreducible polynomial over $\mathbb{F}_p$ to use.

In theory, the code supports higher degree field extensions too, though I don't think they are actually used; the crate implements the degree 4 and 5 extension $\mathbb{F}_p[X]/(X^4-7)$ and $\mathbb{F}_p[X]/(X^5-3)$.

Recently Telos [announced](https://www.telos.net/post/introducing-the-polygon-hermez-zkevm-proof-wrapper-with-plonky2-goldibear) the integration of other fields in their fork.

### Hash choice

Plonky2 can use either Keccak or the Poseidon hash (with custom constants) with `t=12` (that is, the internal state is 12 field elements, approximately 750 bits wide).

For recursive proofs obviously Poseidon is used. To make this fast, a 135 column wide Poseidon gate is used; see [Poseidon.md](Poseidon.md) for more details.

The hash function is used for several purposes:

- the most important is the FRI commitment (both for computing linear hashes of rows and then the Merkle tree on the top);
- but also used for Fiat-Shamir heuristic;
- and handling of public inputs.

Because the public inputs are always hashed into 4 field elements (approx. 256 bits), in practice all circuits contain a Poseidon gate, and thus are 135 columns wide.

In theory it's possible to add further hash function choices, eg. Monolith (faster proofs) or Poseidon2-BN254 (more efficient EVM-compatible wrapper).


Layout
------

Plonky2 organizes all its data (constants, witness, grand product argument for wiring, etc) into a big $2^n\times \widetilde M$ matrix. The rows of this matrix correspond to [the gates](Gates.md).

The columns of this matrix can be organized into four groups:

- constant columns
- witness (or advice) columns
- "partial product" columns (used for the wiring and lookup arguments)
- quotient polynomial

These 4 matrices are committed separately, as you have to commit them in separate phases. Each commitment is a "Merkle cap", consisting by default 16 Merkle roots of $2^4=16$ subtrees. However, the FRI openings are batched, so there is only 1 opening proof for all of them.

For some of them we also need the "next row" opening at $\omega\cdot\zeta$ (namely: ``"zs"``, ``"lookup_zs"``); for the rest we don't.

### Constant columns

These columns define the structure of the circuit (together with some metadata). They consist of:

- gate selector columns
- "lookup selector" columns (only present if lookup gates are used)
- gate constant columns
- the "sigmas" defining the wiring permutation

Note: in the code, ``"constants"`` usually refer the first 3 together, while ``"constants_sigmas"`` refer to all four together.

The number of gate selectors depend on the set of gates; gates are ordered by their degree, then grouped greedily so that their degree together with selector polynomial degree is still below the maximum allowed. As there is always a Poseidon gate (degree 7), normally there are at least 2 such gate selector columns.

Lookup selectors are only present if lookup gates are used. There are `4 + #luts` of them (if present, otherwise 0), 4 shared and one more for each lookup table. See [Lookups.md](Lookups.md) for more detail.

Gate constant columns correspond roughly to the constant columns of the classical Plonk protocol: they define the coefficients in the gates. There are normally only 2 such columns. These are also used to introduce further constants in the circuit, via "constant gates" (see [Gates.md](Gates.md) for details) - this is rather inefficient, and done this way because these constant columns are "not routed", that is, they don't participate in the wiring argument.

Finally "sigmas" define the permutation of the routed witness columns. There are as many of them as many routed columns, so 80 by default.

So in total we have `(#sels+2+80)` or `(#sels+2+(4+#luts)+80)` such constant columns, where `#sels` is usually 2 or 3 and the lookup columns are optional. These are precommitted as part of building the circuit.

### Witness columns

These contain the witness, and contain two types of columns: routed columns and advice columns. The difference is that routed columns participate in the wiring constraints, that is, arbitrary equality (or wiring) constraints can be defined over them.

In the default configuration, the first 80 columns are routed, and the remaining `55 = 135 - 80` are not.

Note that constant columns are _not routed_, so you can only introduce constant constraints via constant gates, which is very inefficient.

### "Partial product" columns

These contain the data required to check the wiring constraints, and if present, the lookup constraints.

See [Wiring.md](Wiring.md) and [Lookups.md](Lookups.md) for more details, respectively.

Normally (if there are no lookups), these contain the partial products of the grand product argument used to prove the wiring permutation. Since Plonky2 allows high-degree constraints (normally degree 8+1), it's enough to store every 8th such partial product, resulting in `80/8 = 10` partial product columns for 80 routed columns. However, the check is repeated $r = \mathtt{num\_challenges}$ times, resulting in total $10r$ columns. Usually $r=2$, sometimes with very big circuits $r=3$ is chosen (?).

Remark: These are reordered in the following way: the last columns of each of the $2^n\times 10$ matrix are moved forward, resulting in $r + 9r$ columns, grouped that way... In the code the single columns are referred as `zs` and the remaining ones as `partial_products`.

Lookups add a further $(7\cdot r)$ columns (1 "RE" and 6 partial sum columns per challenge round).

### Quotient polynomials

In the final phase of the Plonk protocol (see [Protocol.md](Protocol.md), all the constraint are combined into a single one (using random linear combinations), and this (degree 9) constraint is divided by the "zero polynomial", resulting in the (degree 8) "quotient polynomial".

This polynomial (with $8\times 2^n$ coefficients) is then chunked into 8 columns.

However, this is also repeated $r$ times, resulting in $(8\cdot r)$ columns.




The Plonky2 protocol (IOP)
--------------------------

I try to collect together here the phases of the Plonky2 protocol. _Some_ of this is very briefly described in the only existing piece of official documentation, the ["whitepaper"](https://github.com/0xPolygonZero/plonky2/blob/main/plonky2/plonky2.pdf).

### Circuit building

First the static parameters of the circuit are determined:

- global parameters
- set of custom gates
- number of rows (always a power of two)
- gate selectors & gate constants
- wiring permutation
- optional lookup selectors

The constant columns are committed (see also [Layout.md](Layout.md) for the details of these), and the circuit digest (hash) is computed. The latter seems to be used primarily to seed the Fiat-Shamir challenge generator.

### Witness generation

The $2^n\times M$ witness matrix is generated based on the circuit.

This then again committed via FRI (see [FRI.md](FRI.md) for the details).

Notes:
- in practice, we want to pre-commit to the constant columns, as these describe the circuit itself.
- full rows of the (LDE extended) matrices are hashed using the sponge construction, and an Merkle tree is built on these rows, resulting in Merkle root commitment.
- in practice however instead a single root, a wider "Merkle cap" is used (this is an optimization)
 
At this point, the circuit digest, the hash of the public inputs, and these Merkle caps are added to the Fiat-Shamir transcript (which was probably empty before, though Plonky2 has support for initializing with a "proof domain separator").

### Gate constraints

For each gate type, a set of gate equations ("filtered" using the gate selector polynomials) are evaluated at $\zeta\in\widetilde{\mathbb{F}}$, and these are combined "vertically" by simple addition; so we get as many values as the maximum number of equations in a single gate.

This unusual combination (simple summation) is safe to do because the selector polynomials ensure that on each element of the multiplicative subgroup, at most 1 gate constraint do not vanish. And what we prove at the end is that (with very high probability) the resulting (vertically combined) constraints vanish on the whole multiplicative subgroup (but we do this by evaluating outside this subgroup).

See [GateConstraints.md](GateConstraints.md) for the details.

### Permutation argument

The verifier samples $\beta_i,\gamma_i\in \mathbb{F}_p$ challenges for the permutations. There are $r = \mathtt{num\_challenges}$ number of these, which is set so that $(\deg/|\mathbb{F}|)^r$ is small enough. See (see [Challenges.md](Challenges.md) for the details).

If there are lookups, the lookup challenges are also sampled here, $4\times r$ ones. However, as an inelegant optimization, Plonky2 reuses the already computed $2r$ challenges $\beta_i,\gamma_i$ for this, so there are only $(4r-2r)=2r$ new ones generated. These are called $\delta_j$.

As many "sigma columns" as there are routed advice columns (by default 80) were computed (when buildig the circuit), encoded as though the different routed columns correspond to different cosets.

The running product vectors are computed; these are compressed by adding as many terms in a single step as the degree limit allows (by default 8), so from $8k$ routed columns we get $k$ running product columns. But this is again repeated $r$ times.

See [Wiring.md](Wiring.md) for the details how it's done

These are also committed to. So we will have (at the end) the following 4 commitments:

- constants and sigmas (only depends on the circuit)
- witness (or wires)
- running products and lookups
- quotient polynomial chunks (see below)

### Lookups

When lookups are present, this is done next to the permutation argument. See [Lookups.md](Lookups.md) for the details.

### Combined constraints

Verifier samples $\alpha_i \in (\mathbb{F}_p)^r$ challenges to combine the constraints.

All constraints are combined via powers of $\alpha$, namely (in this order):

- the grand products starts/ends with 1
- partial products are built correctly - see [Wiring.md](Wiring.md)
- lookups - see [Lookups.md](Lookups.md)
- all the gate constraints - see [GateConstraints.md](GateConstraints.md)

### Quotient polynomials

Prover computes the combined quotient polynomial(s), by which we mean 1 quotient polynomial per challenge rounds, so in total $r$ many. These are then partitioned into degree N chunks:

$$
Q(x) = \sum_{k=0}^{\mathsf{maxdeg}-1} x^N \cdot Q^{(k)}(x)
$$

and these are committed to in the usual way (so this should be at most $r\times \mathsf{maxdeg}$ columns).

### Evaluation

Verifier samples $\zeta \in \widetilde{\mathbb{F}}$ evaluation challenge (in the 128 bit extension field!).

We open all commitments, that is (see [Layout.md](Layout.md) for more details):

- selectors & circuit constants & sigmas (in a typical case, $2 + 2 + 80$ columns)
- witness wires ($135$ columns)
- partial products ($r\times 10$ columns)
- optionally, lookup RE and partial sums ($r\times(1+6)$ columns, if presents) 
- quotient chunks ($r\times \mathsf{maxdeg}$ columns)

at this challenge $\zeta$, and the case of the first column "zs" of the partial products (and also the lookup ones), also at $\omega\cdot\zeta$.

Corresponding (batched) FRI evaluation proofs are also produced for all these.

That's basically all the prover does.


### Verifier

The verifier essentially does three things (?):

- checks the FRI opening proofs
- compute combined constraints (one for each challenge round $1\dots r$) at $\zeta$
- finally check the quotient equation $\mathcal{Q}(\zeta)Z_H(\zeta) = \mathcal{P}(\zeta)$

Note: the quotient polynomial is "chunked", so the verifier needs to reconstruct it as

$$\mathcal{Q(\zeta)} = \sum_{k=0}^{\mathsf{maxdeg}-1} \zeta^k \cdot Q_k(\zeta)$$

TODO: details

FRI protocol
------------

Plonky2 uses a "wide" FRI commitment (committing to whole rows), and then a batched opening proofs for all the 4 commitments (namely: constants, witness, running product and quotient polynomial).

### Initial Merkle commitment(s)

To commit to a matrix of size $2^n\times M$, the columns, interpreted as values of polynomials on a multiplicative subgroup, are "low-degree extended", that is, evaluated (via an IFFT-FFT pair) on a (coset of a) larger multiplicative subgroup of size $2^{n+\mathsf{rate}^{-1}}$. In the standard configuration we have $\mathsf{rate=1}/8$, so we get 8x larger columns, that is, size $2^{n+3}$. The coset Plonky2 uses is the one shifted by the multiplicative generator of the field

$$ g := \mathtt{0xc65c18b67785d900} = 14293326489335486720\in\mathbb{F} $$

When configured for zero-knowledge, each _row_ (Merkle leaf) is "blinded" by the addition of `SALT_SIZE = 4` extra random _columns_ (huh?).

Finally, each row is hashed (well, if the number of columns is at most 4, they are left as they are, but this should never happen in practice), and a Merkle tree is built on the top of these leaf hashes.

WARNING: before building the Merkle tree, the leaves are _reorderd_ by reversing the order of the bits in the index!!!!

So we get a Merkle tree whose leaves correspond to full rows ($2^{n+3}$ leaves).

Note that in Plonky2 we have in total 4 such matrices, resulting in 4 Merkle caps:

- constants (including selectors and sigmas)
- the witness (135 columns)
- the wiring (and lookup) protocol's running products (resp. sums)
- the quotient polynomial

#### Merkle caps

Instead of using a single Merkle root to commit, we can use any fixed layer (so the commitment will be $2^k$ hashes instead of just $1$). As the paths become shorter, this is a tradeoff between commitment size and proof size / verification cost. In case we have a lot of Merkle openings for a given tree (like in FRI low-degree proofs here), clearly this makes sense. In the default configuration Plonky2 uses Merkle caps of size $2^4=16$

### FRI configuration

An FRI proof is configured by the following parameters:
```
struct FriConfig {
  rate_bits:  usize,        // rate = 2^{-rate_bits}.
  cap_height: usize,        // Height of Merkle tree caps.
  proof_of_work_bits: u32,  // Number of bits used for grinding.
  num_query_rounds: usize,  // Number of query rounds to perform.
  reduction_strategy: FriReductionStrategy, 
}
```

Here the "reduction strategy" defines how to select the layers. For example it can always do 8->1 reduction (instead of the naive 2->1), or optimize and have different layers; also where to stop: If you already reduced to say a degree 3 polynomial, it's much more efficient to just send the 8 coefficients than doing 3 more folding steps.

The "default" `standard_recursion_config` uses rate = $1/8$ (rate_bits = 3), markle cap height = 4, proof of work (grinding) = 16 bits, query rounds = 28, reduction startegy of arity $2^4$ (16->1 folding) and final polynomial having degree (at most) $2^5$. For example for a recursive proof fitting into $2^{12}$ rows, we have the degree sequence $2^{12}\to 2^{8} \to 2^4$, with the final polynomial having degree $2^4 = 16 \le 2^5$

For recursion you don't want fancy reduction strategies, it's better to have something uniform.

Grinding is used to improve security. This means that the verifier sends a challenge $x\in\mathbb{F}$, and a prover needs to answer with a witness $w\in\mathbb{F}$ such that $H(x\|w)$ starts with as many zero bits as specified.

The conjectured security level is apparently `rate_bits * num_query_rounds + proof_of_work_bits`, in the above case $3\times 28 + 16 = 100$. Plonky2 targets 100 bits of security in general. Remark: this is measured in number of hash invocations.

### FRI proof

An FRI proof consists of:

```
struct FriProof<F: RichField + Extendable<D>, H: Hasher<F>, const D: usize> {
  commit_phase_merkle_caps: Vec<MerkleCap<F, H>>,  // A Merkle cap for each reduced polynomial in the commit phase.
  query_round_proofs: Vec<FriQueryRound<F, H, D>>, // Query rounds proofs    
  final_poly: PolynomialCoeffs<F::Extension>,      // The final polynomial in coefficient form.
  pow_witness: F,                                  // Witness showing that the prover did PoW.
}
```

The type parameters are: `F` is the base field, `D` is the extension degree (usually `D=2`), and `H` is the hash function.

During both the proof and verification process, the verifier challenges are calculated. These are:

```
struct FriChallenges<F: RichField + Extendable<D>, const D: usize> {
  fri_alpha: F::Extension,         // Scaling factor to combine polynomials.
  fri_betas: Vec<F::Extension>,    // Betas used in the FRI commit phase reductions.
  fri_pow_response: F,             // proof-of-work challenge
  fri_query_indices: Vec<usize>,   // Indices at which the oracle is queried in FRI.
}
```

Here powers of $\alpha\in\widetilde{\mathbb{F}}$ is used to combine several polynomials into a single one, and $\beta_i\in\widetilde{\mathbb{F}}$ are the coefficients used in the FRI "folding steps". Query indices (size is `num_query_rounds`) is presumably the indices in the first layer LDE where the Merkle oracle is queried.

See [Challenges.md](Challenges.md) for how these Fiat-Shamir challenges are generated.

Remark: **`batch_fri`**: There is also `batch_fri` subdirectory in the repo, which is not clear to me what actually does, as it doesn't seems to be used anywhere...

### The FRI protocol

The FRI protocol proves that a committed Reed-Solomon codeword, which is a priori just a vector, is in fact "close" to a codeword (with high probability).

This is done in two phases: The commit phase, and the query phase. Note that this "commit" is not the same as the above commitment to the witness etc!

In Plonky2, we want to execute this protocol on many polynomials (remember that each column is a separate polynomial)! That would be very expensive, so instead they (well, not exactly them) are combined by the prover with the random challenge $\alpha$, and the protocol is executed on this combined polynomial.

#### Combined polynomial

So we want to prove that $F_{i}(x_i)=y_{i}$ where $F_{i}(X)$ are a set of (column) polynomials. In our case $\{F_i\}$ consists of two _batches_, and $x_i\in\{\zeta,\omega\zeta\}$ are constants on the batches. The first batch of the all the column polynomials, the second only those which needs to be evaluated at $\omega\zeta$ too (`"zs"` and the lookup polynomials). We can then form the combined quotient polynomial:

$$ P(X) := \sum_{i} \alpha^{i} \frac{\;F_i(X) - y_i\;}{X-x_i} = \sum_b \frac{\alpha^{k_b}}{X - x_b} \sum_{j=0}^{m_b} 
\alpha^j \big[F_{b,j}(X) - y_{b,j}\big]$$

In practice this is done per batch (see the double sum), because the division is more efficient that way. This polynomial $P(X)$ is what we execute the FRI protocol on, proving a degree bound.

Remark: In the actual protocol, $F_i$ will be the columns and $y_i$ will be the openings. Combining the batches, we end up with 2 terms:

$$P(X) = P_0(X) + \alpha^{M_0}\cdot P_1(X) = 
\frac{G_0(X)-Y_0}{X-\zeta} + \alpha^M\cdot \frac{G_1(X)-Y_1}{X-\omega\zeta}$$

The pair $(Y_0,Y_1)$ are called "precomputed reduced openings" in the code (calculated from the opening set, involving _two rows_), and $X$ will be substituted with $X\mapsto \eta^{\mathsf{query\_index}}$ (calculated from the "initial tree proofs", involving _one row_). Here $\eta$ is the generator of the LDE subgroup, so $\omega = \eta^{1/\rho}$.

**NOTE1:** while the above would be _the logical version_, here _AGAIN_ Plonky2 does something twisted instead, just because:

$$ P(X) := \alpha^{M_1}\cdot P_0(X) + P_1(X)$$

where $M_0,M_1$ denote the number of terms in the sums in $P_0,P_1$, respectively.

**NOTE2:** in all these sums (well in the zero-index ones), the terms are _reordered_ so that the lookups are now at the very end. This is frankly **just stupid** because the both kind of inputs are _in a different order_, and in the case of Merkle tree openings, you couldn't even change that order...

#### Commit phase

Recall that we have a RS codeword of size $2^{n+(1/\rho)}$ (encoding the combined polynomial $P(X)$ above), which the prover committed to. 

The prover then repeatedly "folds" these vectors using the challenges $\beta_i$, until it gets something with low enough degree, then sends the coefficients of the corresponding polynomial in clear.

As example, consider a starting polynomial of degree $2^{13}-1$. With $\rho=1/8$ this gives a codeword of size $2^{16}$. This is committed to (but the see the note below!). Then a challenge $\beta_0$ is generated, and we fold this (with an arity of $2^4$), getting a codeword of size $2^{12}$, representing a polynomial of degree $2^9-1$. We commit to this too. Then generate another challenge $\beta_1$, and fold again with that. Now we get a codeword of size $2^8$, however, this is represented by a polynomial of at most degree $31$, so we just send the 32 coefficients of that instead of a commitment.

Notes: as an optimization, when creating these Merkle trees, we always put _cosets_ of size $2^{\mathsf{arity}}$ on the leaves, as we will have to open them all together anyway. To achieve this grouping, Plonky2 reverses the order of elements by reversing _the order of bits in the indices_; this way, members of cosets (which were before far from each other) become a continuous range. 

Furthermore, we use _Merkle caps_, so the proof lengths are shorter by the corresponding amount (4 by default, because we have 16 mini-roots in a cap). So the Merkle proofs are for a LDE size $2^k$ have length $k-\mathsf{arity\_bits}-\mathsf{cap\_bits}$, typically $k-8$.

| step | Degree     | LDE size | Tree depth |prf. len | fold with |send & absorb |
|------|------------|----------|------------|---------|-----------|--------------|
|   0  | $2^{13}-1$ | $2^{16}$ |    12      |   8     | $\beta_0$ | Merkle cap   |
|   1  |  $2^{9}-1$ | $2^{12}$ |     8      |   4     | $\beta_1$ | Merkle cap   |
|   2  |  $2^{5}-1$ |   $2^8$  |    --      |  --     |    --     | $2^5$ coeffs |

#### Grinding

At this point (why here?) the grinding challenge is executed.

#### Query phase

This is repeated `num_query_rounds = 28` times (in the default configuration).

A query round consists of two pieces:

- the initial tree proof
- and the folding steps

The initial tree proof consist of a single row of the 4 LDE matrices (the index of this row is determined by the query index challenges), and a Merkle proof against the 4 commited Merkle caps.

The steps consist of pairs of evaluations on cosets (of size $2^{\mathsf{arity}}$) and corresponding Merkle proofs against the commit phase Merkle caps.

From the "initial tree proof" values $\{F_j(\eta^k)\}$ and the openings $\{y_j,z_j\}$, we can evaluate the combined polynomial at $\eta^k := \eta^{\mathsf{query\_idx}}$:

$$P(\eta^k) = 
\frac{1}{\eta^k - \zeta} \sum_{j=0}^{M_1-1} 
\alpha^j \big[ F_j(\eta^k) - y_j\big]
+
\frac{1}{\eta^k - \omega\zeta} \sum_{j=M_1}^{M_2-1} 
\alpha^{j} \big[ F_j(\eta^k) - z_j\big]
$$

Then in each folding step, a whole coset is opened in the "upper layer", one element of which was known from the previous step (or in the very first step, can be computed from the "initial tree proofs" and the openings themselves) which is checked to match. Then the folded element of the next layer is computed by a small $2^\mathsf{arity}$ sized FFT, and this is repeated until the final step.

### Folding math details

So the combined polynomial $P(x)$ is a polynomial of degree (one less than) $N=2^{n+(1/\rho)}$ over $\widetilde{\mathbb{F}}$. We first commit to the evaluations 

$$\big\{P(g\cdot \eta^i)\;:\;0\le i < N\big\}$$

of this polynomial on the coset $gH=\{g\cdot\eta^k\}$ where $\eta$ is generator of the subgroup of size $N$, and $g\in\mathbb{F}^\times$ is a fixed generator of the whole multiplicative group of the field.

However, the commitment is done not on individual values, but smaller cosets $\mathcal{C}_k:=\{g\cdot \eta^{i(N/\mathsf{arity})+k}\;:\;0\le i < \mathsf{arity}\}$ of size $\mathsf{arity}=16$.

To achieve this, the above vector of evaluation is reordered, so that _the bits of vector indices_ are reversed. 

Then the query index (in each query round) selects a single element of the vector. Note that the bits of the `query_index` are _also reversed_ - however as the Merkle tree leaves are reordered too, this is not apparent at first! So mathematically speaking the locations look like $x_0 = g\cdot \eta^{\mathrm{rev}(\mathsf{idx})}$...

From that, we can derive its coset, which we open with a Merkle proof. We can check the consistency of the selected element (combined polynomial value) vs. the original polynomial commitments using the formula for the combined polynomial.

                           
           0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
         +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    C_k  |           |     |##|  |           |           |
         +-----------+-----------+-----------+-----------+
          \                 ^^                          /
            \ ____                             ______ /
                   \ _____            ______ /
                           \        /
                             \    /
               +--+-      -+--+--+--+-      -+--+
     C_(k>>4)  |      ...     |##|      ...     |
               +--+-      -+--+--+--+-      -+--+
                 0             ^^             15
                           (k mod 16)

Next, we want to "fold this coset", so that we can repeat this operation until we get the final polynomial, we we simply check the final folded value aginst an evaluation of the final polynomial.

On the prover side, the folding step looks like this:

$$ P(x) = \sum_{i=0}^{\mathsf{arity}-1} x^i\cdot P_i(x^\mathsf{arity}) \quad\quad\longrightarrow\quad\quad
 P'(y) := \sum_{i=0}^{\mathsf{arity}-1} \beta^i\cdot P_i(y) $$

The folded polynomial $P'(x)$ will be then evaluated on the (shrunken) coset 

$$H' := H^{\mathsf{arity}} = \big\{(g\eta^i)^\mathsf{arity}\;:\;i\big\} =\big\{g^\mathsf{arity}(\eta^{\mathsf{arity}})^{i'} \;:\; 0\le i'<N/\mathsf{arity} \big\}$$

and so on.

Now the verifier has the evaluations $\{P(x_0\cdot \eta^{iN/\mathsf{arity}})\}$ on a small coset (of size $\mathsf{arity}$) offset by $x_0$ where $x_0=g\cdot\eta^{\mathrm{rev}(\mathsf{arity}\cdot k)}$ and $k=\lfloor\mathsf{query\_idx}/\mathsf{arity}\rfloor$ (these bit reversions kill me...).

Ok, let's just try and calculate this. I'll use $\mathsf{arity}=8$ for brevity. Let $\omega$ denote an 8-th root of unity, and let's write $P(x)=\sum_i a_ix^i$ in coefficient form. Then

\begin{align*}
P_k(x) &= \sum_j a_{k+8j}x^j \\
P_k(x^8) &= \sum_j a_{k+8j}x^{8j} =
\sum_i a_{i} x^{i-k}\cdot\left\{
\begin{array}{lll}
1 & \textrm{if} & i \equiv     k \mod 8)  \\
0 & \textrm{if} & i \not\equiv k \mod 8)
\end{array}\right\} = \\
&=
\sum_i a_{i} x^{i-k}\cdot \frac{1}{8} \sum_{l=0}^7\omega^{l(i-k)} 
=\frac{1}{8}\sum_{l=0}^7 \omega^{-lk}x^{-k}\sum_i a_{i} x^{i}\omega^{li} = \\
&=\frac{1}{8}\sum_{l=0}^7
(x\omega^l)^{-k}\cdot P(x\omega^l) \\
\end{align*}

As what we really want to compute is $\sum_k \beta^k P_k(x_0^8)$, we can see that what really do here is a small inverse FFT (on a coset) of the values we have, then just combine them with $\beta$.

This can be reinterpreted as interpolating a degree `arity-1` polynomial from these values, and evaluating it at $\beta$, and indeed this is how Plonky2 implements it.

### FRI verifier summary

So what does the verifier needs to check at the end?

- check if the public parameters match the proof "shape" (arities, number of query rounds, Merkle cap sizes, etc)
- check the proof-of-work response
- then for each query round:
    - check the initial tree Merkle proofs against the 4 commitments (constants, witness, partial products and quotient)
    - compute the evaluation of the combined polynomial from these values (a single row) at the query location
    - for each folding step:
        - check the coset opening proof against the corresponding commit phase Merkle cap
        - check if the right element in the coset values matches the evaluation coming from the previous step (in the very first step, against the above compute value)
        - calculate the folded value, and pass it to the next step
    - check the final folded value against the evaluation of the final polynomial

### FRI verification cost

We can try and estimate the cost of the FRI verifier. Presumably the cost will be dominated by the hashing, so let's try to count the hash permutation calls. Let the circuit size be $N=2^n$ rows.

- public input: $\lceil \#\mathsf{PI} / 8 \rceil$ (we hash with a rate 8 sponge)
- challenges: approximately 95-120. The primary variations seem to be number (and size) of commit phase Merkle caps, and the size of the final polynomial
- then for each query round (typically 28 of them), approx 40-100 per round:
    - check the opened LDE row against the 4 matrix commitments ("initial tree proof"):
        - hash a row (typical sizes: 85, 135, 20, 16; resulting in 11, 17, 3 and 2 calls, respectively; in total `11 + 7 + 3 + 2 = 33`)
        - check a Merkle proof (size `n + rate_bits - cap_height = n + 3 - 4 = n-1`)
        - in total `33 + 4(n-1)` calls
    - check the folding steps:
        - for each step, hash the coset (16 $\widetilde{\mathbb{F}}$ elements, that's 4 permutations)
        - then recompute the Merkle root: the first one is `n+3-8` (more precisely: `n + rate_bits - cap_height - arity_bits`), the next is `n+3-12` etc

For example in the case of a recursive proof of size $N=2^{12}$, we have 114 permutation calls for the challenges, and then $28 \times (77+11+7)$, resulting in total

$$114 + 28\times (77+11+7) = 2774$$

Poseidon permutation calls (with `t=12`), which matches the actual code.

We can further break down this to sponge vs. compression calls. Let's concentrate on the FRI proof only, as that dominates:

- sponge is (33 + 4 + 4 ...) per round 
- compression is 4(n - 1) + (n-5) + (n-9) + ... per round

In case of $n=12$, we have 41 (sponge) vs. 54 (compression). As compression is more than half of the cost, it would make sense to optimize that to `t=8` (say with the Jive construction); on the other hand, use a larger arity (say `t=16`) for the sponge.

It seems also worthwhile to use even wider Merkle caps than the default $2^4$.

### Soundness

Soundness is bit tricky because of the small field. Plonky2 uses a mixture of sampling from a field extension and repeated challenges to achieve a claimed \~100 bit security:

| Sub-protocol              | Soundness boost           | 
| ------------------------- | ------------------------- | 
| Permutation argument      | parallel repeatition      |
| Combining constraints     | parallel repeatition      |
| Lookup argument           | parallel repeatition      |
| Polynomial equality test  | extension field           |
| FRI protocol              | extension field + grinding|

See [this 2023 paper](https://eprint.iacr.org/2023/1071) for more precise soundness arguments along these lines.

FRI commitment
--------------

Plonky2 uses a "wide" FRI commitment (committing to whole rows), and then a batched opening proofs for all the 4 commitments (namely: constants, witness, running product and quotient polynomial).

### Commitment

To commit to a matrix of size $2^n\times M$, the columns, interpreted as values of polynomials on a multiplicative subgroup, are "low-degree extended", that is, evaluated (via an IFFT-FFT pair) on a (coset of a) larger multiplicative subgroup of size $2^{n+\mathsf{rate}^{-1}}$. In the standard configuration we have $\mathsf{rate=1}/8$, so we get 8x larger columns, that is, size $2^{n+3}$. The coset Plonky2 uses is the one shifted by the multiplicative generator of the field

$$ g := \mathtt{0xc65c18b67785d900} = 14293326489335486720\in\mathbb{F} $$

Note: There may be some reordering of the LDE values (bit reversal etc) which I'm unsure about at this point.

When configured for zero-knowledge, each row is "blinded" by the addition of `SALT_SIZE = 4` extra random columns (huh?).

Finally, each row is hashed (well, if the number of columns is at most 4, they are leaved as they are, but this should never happen in practice), and a Merkle tree is built on the top of these leaf hashes.

So we get a Merkle tree whose leaves correspond to full rows ($2^{n+3}$ leaves).

#### Merkle caps

Instead of using a single Merkle root to commit, we can use any fixed layer (so the commitment will be $2^k$ hashes instead of just $1$). As the paths become shorter, this is a tradeoff between commitment size and proof size / verification cost. In case we have a lot of Merkle openings for a given tree (like in FRI low-degree proofs here), clearly this makes sense.

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

Here the "reduction strategy" defines how to select the layers. For example it can always do 8->1 reduction (instead of the naive 2->1), or optimize and have different layers; also where to stop.

The "default" `standard_recursion_config` uses rate = $1/8$ (rate_bits = 3), markle cap height = 4, proof of work (grinding) = 16 bits, query rounds = 28, and reduction startegy of arity $2^4$ and final polynomial having degree $2^5$.

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

### Low-degree test

TODO: describe the FRI low-degree test

### Opening proofs

TODO: describe the opening proofs

### Soundness

Soundness is bit tricky because of the small field. Plonky2 uses a mixture of sampling from a field extension and repeated challenges to achieve a claimed \~100 bit security:

| Sub-protocol              | Soundness boost          | 
| ------------------------- | ------------------------ | 
| Permutation argument      | parallel repeatition     |
| Combining constraints     | parallel repeatition     |
| Lookup argument           | parallel repeatition     |
| Polynomial equality test  | extension field          |
| FRI protocol              | extension field + grinding
|

See [this 2023 paper](https://eprint.iacr.org/2023/1071) for more precise soundness arguments along these lines.

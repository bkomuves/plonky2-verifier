Fiat-Shamir Challenges
----------------------

The verifier challenges are genered via Fiat-Shamir heuristics.

This uses the hash permutation in a duplex construction, alternatively absorbing the transcript and squeezing challenge elements. This is implemented in `iop/challenger.rs`.

All the challenges in the proof are summarized in the following data structure

```rs
struct ProofChallenges<F: RichField + Extendable<D>, const D: usize> {
  plonk_betas:    Vec<F>,         // Random values used in Plonk's permutation argument.
  plonk_gammas:   Vec<F>,         // Random values used in Plonk's permutation argument.
  plonk_alphas:   Vec<F>,         // Random values used to combine PLONK constraints.
  plonk_deltas:   Vec<F>,         // Lookup challenges (4 x num_challenges many). 
  plonk_zeta:     F::Extension,   // Point at which the PLONK polynomials are opened.
  fri_challenges: FriChallenges<F, D>,
}
```

And the FRI-specific challenges are:

```rs
struct FriChallenges<F: RichField + Extendable<D>, const D: usize> {
  fri_alpha: F::Extension,         // Scaling factor to combine polynomials.
  fri_betas: Vec<F::Extension>,    // Betas used in the FRI commit phase reductions.
  fri_pow_response:  F,            // proof-of-work challenge response
  fri_query_indices: Vec<usize>,   // Indices at which the oracle is queried in FRI.
}
```

### Duplex construction

The duplex construction is similar the sponge construction, but it interleaves absorbing and squeezing. This is a very natural fit for Fiat-Shamir, as these are exactlty the operations we do:

- absorb the prover's message (transcript so far);
- then generate some challenges;
- then the prover replies with more messsages;
- based on which we generate more challenges;
- and so on...

We have to be careful with the details though.

To have a nice API, we want an API which can absorb and generate various data types (Goldilocks elements, field extension elements, hash digests, etc).

The duplex can be modelled as a state machine:

- We are either in absorbing or squeezing mode.
- In absorbing mode, we just collect inputs in a buffer
- In squeezing mode, we have a buffer initialized by `rate` elements of the state, from which we can produce outputs, until it becomes empty; then we do a permutation which refills the buffer
- When switching from absorbing mode to squeezing mode, we absorb the collected buffer exactly like a sponge.

#### Duplex algorithm pseudo code

```hs
type State = [F]

permute :: State -> State
permute = ...

rate = 8

data DuplexState 
  = Absorbing State [F]
  | Squeezing State [F]

initialDuplexState = Absorbing zeroState []

-- Absorb an element
absorb :: F -> DuplexState -> DuplexState
absorb what duplex = case duplex of
  Absorbing state list -> Absorbing state (list ++ [what])
  Squeezing state _    -> Absorbing state [what]

-- Squeeze an element
squeeze :: DuplexState -> (F, DuplexState)
squeeze duplex = case duplex of
  Absorbing state list -> squeezing (sponge list state)
  Squeezing state buf  -> case buf of
    []     -> squeezing (permute state)
    (y:ys) -> (y, Squeezing state ys)

-- Helper function
squeezing :: State -> (F, DuplexState)
squeezing state = squeeze $ Squeezing state (extract state)

-- Classic sponge in overwrite mode
sponge :: [F] -> State -> State
sponge = go where
  go []   state = state
  go list state = case splitAt rate list of
    (this,rest) -> go rest (permute $ overwrite this state)

-- Plonky2 uses "overwrite mode"
overwrite :: [F] -> State -> State
overwrite input state 
  | k > rate   = error "overwrite: expecting at most `rate` elements"
  | otherwise  = input ++ drop k state
  where
    k = length input

-- The reverse is because Plonky2 uses `pop` (from the end of) the buffer 
extract :: State -> [F]
extract = reverse . take rate

```

### Transcript

Usually the communication (in an IOP) between the prover and the verifier is called "the transcript", and the Fiat-Shamir challenger should absorb all messages of the prover.

The duplex state is initialized by absorbing the "circuit digest".

This is the hash of the following data:

- the Merkle cap of the constant columns (including the selectors and permutation sigmas)
- the _hash_ of the optional domain separator data (which is by default an empty vector)
- the size (number of rows) of the circuit

Thus the challenge generation starts by absorbing:

- the circuit digest
- the hash of the public inputs
- the Merkle cap of the witness matrix commitment

Then the $\beta\in\mathbb{F}^r$ and $\gamma\in\mathbb{F}^r$ challenges are generated, where `r = num_challenges`.

If lookups are present, next the lookup challenges are generated. This is a bit ugly. We need $4\times r$ such challenges, but as an optimization, the $\beta,\gamma$ are reused. So $2\times r$ more $\delta$ challenges are generated, then these are concatenated into ($\beta\|\gamma\|\delta)\in\mathbb{F}^{4r}$, and finally this vector is chunked into $r$ pieces of 4-vectors...

Next, the Merkle cap of the partial product columns is absorbed; and after that, the $\alpha\in\mathbb{F}^r$ combining challenges are generated.

Then, the Merkle cap of the quotient polynomials is absorbed, and the $\zeta\in\widetilde{\mathbb{F}}$ evaluation point is generated.

Finally, the FRI challenges are generated.

### FRI challenges

First, we absorb all the opening (a full row, involving all the 4 committed matrix; and some parts of the "next row").

Then the $\alpha\in\widetilde{\mathbb{F}}$ combining challenge is generated (NOTE: this is different from the above $\alpha$-s!)

Next, the `commit_phase_merkle_caps` are absorbed, and after each one, a $\beta_i\in\widetilde{\mathbb{F}}$ is generated (again, different $\beta$-s from above!).

Then we absorb the coefficients of the final (low-degree) folded FRI polynomial. This is at most $2^5=32$ coefficients in the default configuration.

Next, the proof-of-work "grinding" is handled. This is done a bit strange way: _first_ we absorb the candidate prover witness, _then_ we generate the response, and check the leading zeros of that. I guess you can get away with 1 less hashing in the verifier this way...

Finally, we generate the FRI query indices. These are indices of rows in the LDE matrix, that is, $0 \le q_i < 2^{n+\mathtt{rate\_bits}}$.

For this, we generate `num_query_rounds` field elements, and take them modulo this size.



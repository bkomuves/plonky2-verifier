Selectors and constants
-----------------------

The circuit descriptions consists not only of the permutation, but also the gate selectors and gate constants.

(There are also "lookup selectors", for those see [Lookups](Lookups.md))

Typically we have 2 or 3 selector columns (to switch between the different gates) and 2 constant columns (for the constants of the arithmetic and other gates).

For example in the Fibonacci example circuit uses 5 gates, which are:

  - `NoopGate`
  - `ConstantGate { num_consts: 2 }`
  - `PublicInputGate`
  - `ArithmeticGate { num_ops: 20 }`
  - `PoseidonGate(...)<WIDTH=12>`

(in fact the `NoopGate` is used only when changing from the 100th Fibonacci number to say the 200th one, because apparently the 100 example just fills the trace exactly...)

This can be seen for example by serializing the `CommonCircuitData` struct. 

The Poseidon gate is present because Plonky2 handles public input by always hashing it into 4 field element (encoded in the public input gate).

These gates are numbered `0..4`. Looking at the selector polynomials in the Fibonacci example, in the first one we see a lot of `3` (arithmetic gates, encoding the Fibonacci computation); a `UNUSEDGATE = 2^32-1`, which is a placeholder for gates not using the given selector polynomial, a 2 (public input), an 1 (constants), and some 0-s (no-op). In the other selector columns, it's all UNUSED except one 4 (Poseidon hash gate), exactly where the other one is unused.

We can see that a full selector column is only used for Poseidon, while the first column is used for everything else.

This makes sense, as the selector strategy of Plonky2 is the following: Enumarates the gates `0..#ngates-1`; it then groups as many gates into a single selector as many is possible with the equation degree limit. If you have `K` gates $\mathsf{gate}_i$ in a single selector column, then the corresponding selector polynomials have degree `K`:

$$
S_k(x) = \prod_{i\neq k} \frac{U(x)-\mathsf{gate}_i}{\; \mathsf{gate}_k - \mathsf{gate}_i \; } \quad\quad
S_k(\omega^i) = \left\{\begin{array}{ll}
1, & \textrm{if } S(\omega^i) = \mathsf{gate}_k \\
?, & \textrm{if } S(\omega^i) = 2^{32}-1 \\
0, & \textrm{otherwise}
\end{array}\right.
$$

where $U(x)$ is the (degree $N-1$) polynomial encoding the given selector column. Observe that the normalization is actually not necessary (as the gate equations are all normalized to zero) and Plonky2 doesn't do it. The actual polynomials used by Plonky2 are instead

$$\mathcal{S}_k(x) := \big((2^{32}-1)-U(x)\big)\cdot \prod_{i\neq k} \big(\mathsf{gate}_i-U(x)\big)$$

The factor cancelling unused gates is only added if there are more than 1 selector columns, but because the Poseidon gate is always included (to handle the public input), and has degree 7, this is always the case.

The Poseidon gate has degree 7, while the degree limit is 8, so we can only use a degree 1 or 2 selector there (note that the degree limit is for the _quotient_ polynomial, which is always "one less". Though it seems to me that the code has $\pm 1$ error here, and as a result is overly cautious...). 

The arithmetic gate has degree 3 (because $\deg(c_0xy)=3$: the constant coefficients also count!); the noop has degree 0 and both the constant and public input gates have degree 1. As $4+\max(3,0,1,1)=7\le 9$ this still fits.

The constant columns contain the $c_0,c_1$ constants for the arithmetic gates (they are all 1 here); also the  values for the constant gates. For the remaining gates (Poseidon, public input and noop) they are simply set to zero.
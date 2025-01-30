Custom Gates
------------

Plonky2 has about a dozen of custom gates included by default (but the user can in theory add more). Most of these are intended for implementing the recursive verifier circuit.

A custom gate is essentially several (low-degree) polynomial constraints over the witness cells of a single row, plus recipes to calculate some of these (for example in the Poseidon gate, all cells apart from the 12 inputs are calculated). The latter is encoded in a so called "Generator" using the `SimpleGenerator` trait.

On the user side, it seems that custom gates are abstracted away behind "gadgets".

Unfortunately the actual gate equations never appear explicitly in the code, only routines to calculate them (several ones for different contexts...), which 1) makes it hard to read and debug; and 2) makes the code very non-modular. 

However, there is also a good reason for this: The actual equations, if described as (multivariate) polynomials, can be very big and thus inefficient to calculate, especially in the case of the Poseidon gate. This is because of the lack of sharing between intermediate variables. Instead, you need to described _an efficient algorithm_ to compute these polynomials (think for example Horner evaluation vs. naive polynomial evaluation). 

Note that while in theory a row could contain several non-overlapping gates, the way Plonky2 organizes its gate equations would make this unsound (it would also complicate the system even more). See the details at the [protocol description](Protocol.md).

### List of gates

The default gate set is:

    - arithmetic_base
    - arithmetic_extension
    - base_sum
    - constant
    - coset_interpolation
    - exponentiation
    - lookup
    - lookup_table
    - multiplication_extension
    - noop
    - poseidon
    - poseidon_mds
    - public_input
    - random_access
    - reducing
    - reducing_extension

### Arithmetic gates

These evaluate the constraint $w = c_0xy + c_1z$, either in the base field or in the quadratic extension field, possibly in many copies, but with shared $c_0,c_1\in\mathbb{F}$ (these seem to be always in the base field?)

Note: in ArithmeticExtensionGate, since the constraints are normally already calculated in the extension field, we in fact compute in a doubly-extended field $\widetilde{\mathbb{F}}[Y]/(Y^2-7)$! Similarly elsewhere when we reason about $\widetilde{\mathbb{F}}$.

### Base sum gate

This evaluates the constraint $x = \sum_{i=0}^k a_i B^i$ (where $B$ is the radix or base). It can be used for example for simple range checks.

The the coefficient ranges $0\le a_i < B$ are checked very naively as

$$ \forall\, i. \quad\prod_{k=0}^{B-1} (a_i - k) = 0 $$ 

which is a degree $B$ equation, so this is only useful for very small $B$-s (typically $B=2$).

The corresponding witness row looks like $[x,a_0,a_1,\dots a_{B-1} , 0, \dots, 0 ]$.

### Constant gate

A very simple gate enforcing the $x_i = c_i$. Here the $c_i$ are the same type of constants as in the arithmetic circuit. The reason for the existence of this is presumably that the constant columns _are not routed columns_, otherwise you could use the permutation (wiring) argument to enforce such constants.

I'm not convinced this is the right design choice, but probably depends on the circumstances.

### Coset interpolation gate

This gate interpolates a set of values $y_i\in\widetilde{\mathbb{F}}$ on a coset $\eta H$ of a small subgroup $H$ (typically of size $2^4$), and evaluates the resulting polynomial at an arbitrary location $\zeta\in\widetilde{\mathbb{F}}$.

The formula is the [barycentric form of Lagrange polynomials](https://en.wikipedia.org/wiki/Lagrange_polynomial#Barycentric_form), but slightly modified to be chunked and to be iterative.

This is used for recursion.

### Exponentiation

This computes $y=x^k$ using the standard fast exponentiation algorithm, where $k$ is number fitting into some number of bits (depending on the row width).

I believe it first decomposes $k$ into digits, and then does the normal thing. Though for some reason it claims to be a degree $4$ gate, and I think it should be degree $3$... TODO: sanity check this

### Lookups

There are two kind of lookup gates, one containing $(\mathsf{inp},\mathsf{out})$ pairs, and the other containing $(\mathsf{inp},\mathsf{out},\mathsf{mult})$ triples.

Neither imposes any constraint, as lookups are different from usual gates, as their behaviour is hardcoded in the protocol.

The 2 gates (`LookupGate` for the one without multiplicities and `LookupTableGate` for the one with) encode the lookup usage and the table(s) themselves, respectively. Plonky2 uses a logarithmic derivative based lookup argument.

See [Lookups.md](Lookups.md) for more details.

### Multiplication extension gate

This is the same as `ArithmeticExtensionGate`, except that it misses the addition. So the constraint is $z = c_0xy \in \widetilde{\mathbb{F}}$. In exchange, you can pack 13 of these into 80 columns instead of 10.

### Noop gate

This doesn't enforce any constraint. It's used as a placeholder so each row corresponds to exactly a single gate; and also lookup tables as implemented require an empty row.

### Poseidon gate

This computes the Poseidon hash (with Plonky2's custom constants and MDS matrix). 

The poseidon gate packs all the (inputs of the) nonlinear sbox-es into a 135 wide row, this results in the standard configuration being 135 advice columns.

Poseidon hash is used for several purposes: hashing the public inputs into 4 field elements; the recursion verification of FRI; and generating challenges in the verifier.

See [Poseidon.md](Poseidon.md) for more details.

### Poseidon MDS gate

This simply computes the multiplication by the 12x12 MDS matrix (so all linear constraints), but with the input vector consisting of field extension elements. It is used in the recursive proof circuit.

### Public input gate

This simply packs the hash of the public inputs (4 field elements) into the first four cells of a row.

The actual public inputs are presumably at arbitrary locations in the routed advice wires; then Poseidon gates are automatically added and wired to compute the hash of those. The final hash result is "wired" to be equal to these values.

Finally these four values are constrained (with 4 linear equations) to be the actual hash values, which I guess is how the verifier checks these (that is, the hash values are hardcoded in the equations).

### Random access gate

This gate allows dynamically indexing (relatively short) vector of size $2^n$.

The idea is to decompose the index into bits $\mathsf{idx} = \sum b_i 2^i$; then if you can write down "selectors" like for example

$$
S_{11} := S_{\mathtt{0b1011}} := b_0(1-b_1)b_2 b_3 = \left\{\begin{array}{lll}
1, &\textrm{if}& \mathsf{idx} = \mathtt{0b1011}=11 \\
0, &\textrm{if}& \mathsf{idx} \neq 11
\end{array}\right.
$$

Then $A_{\mathsf{idx}} = \sum_{i=0}^{2^{n}-1} S_i\cdot A_i$.

Or at least that's how I would do it :)

The degree of the gate is $n+1$, so they probably inline the above selector definitions.

In the actual implementation, they repeat this as many times as it fits in the routed wires, followed by (at most) 2 cells used the same way as in the constant gate, finally followed by the bit decompositions

So the cells look like (for `n=4`): `copies ++ consts ++ bits` with 

    copies = [ i,a[i],a0,...,a15; j,b[j],b0,...b15; ... ]
    consts = [ c0, c1 ]
    bits   = [ i0,i1,2,i3; j0,j1,j2,j3; ...; 0... ]
    
### Reducing gates

These compute $y = \sum_{i=0}^k \alpha^i\cdot c_i$ with the coefficients $c_i$ in either the base field or the extension field, however with $\alpha\in\widetilde{ \mathbb{F}}$ always in the extension field. 

It assumes that everything fits into a single row.

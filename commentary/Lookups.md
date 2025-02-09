Lookup tables
-------------

Plonky2 has added support lookup tables only relatively recently, and thus it is not documented at all (not even mentioned in the "whitepaper", and no example code either).

### Logarithmic derivatives

Plonky2 doesn't use the classical $\mathtt{plookup}$ protocol, but the more recent approach based on the paper [Multivariate lookups based on logarithmic derivatives](https://eprint.iacr.org/2022/1530), with the implementation following the [Tip5 paper](https://eprint.iacr.org/2023/107).

In logarithmic derivative based lookups, the core equation is

$$
\mathsf{LHS}(X) := \sum_{j=0}^{K-1} \frac{1}{X - w_j} = \sum_{i=0}^{N-1}\frac{m_i}{X - t_i} := \mathsf{RHS}(X)
$$

where $w_j$ is the witness, $t_i$ is the table, and we want to prove that $\{w_j\}\subset \{t_i\}$, by providing the multiplicities $m_i$ of $w_i$ appearing in $\{t_i\}$. This then becomes a scalar equation, after the subtitution $X\mapsto \alpha$, where $\alpha \in \mathbb{F}$ is a random number (chosen by the verifier). 

Note that the above equation is simply the logarithmic derivative (wrt. $X$) of the more familiar permutation argument:

$$
\prod_j(X - w_j) = \prod_i(X - t_i)^{m_i}
$$

The reason for the logarithmic derivative is to move the multipliers from the exponent to a simple multiplication.

### Binary lookups

Plonky2 _only_ allows lookups of the form `input->output`, encoding equations like $y=f(x)$ where the function $f$ is given by the table. In this case we have $t_i := x_i + a\cdot y_i$ (for some verifier-chosen $a\in \mathbb{F}$) and similarly for the witness.

### Gates

Plonky2 uses two types of gates for lookups:

- `LookupGate` instances encode the actual lookups
- while `LookupTableGate` instances encode the tables themselves

Each `LookupGate` can contain up to $\lfloor 80/2\rfloor = 40$ lookups, encoded as $(\mathsf{inp},\mathsf{out})$ pairs; and each `LookupTableGate` can contain up to $\lfloor 80/3\rfloor = 26$ table entries, encoded as $(\mathsf{inp},\mathsf{out},\mathsf{mult})$ triples. 
 
This (questionable) design decision has both advantages and disadvantages:

- tables can be encoded in less rows than their size, allowing them to be used in small circuits
- up to 40 lookups can be done in a single row; though they are repeated at their actual usage locations, and need to be connected by "wiring"
- on the other hand, encoding the tables in the witness means that the verifier has to do a relatively expensive (linear in the table size!) verification of the tables
- and the whole things is rather over-complicated

### The idea behind the protocol

Four challenges (repeated `num_challenges` times) are used in the lookup argument: $a,b \in \mathbb{F}$ are used to combine the input and output, in the above argument and the "table consistency argument", respectively; $\alpha\in \mathbb{F}$ is the above random element; and $\delta\in \mathbb{F}$ is the point to evaluate the lookup polynomial on. NOTE: in the actual implementation, two out of the four are reused from the from permutation argument (called $\beta,\gamma$ there).

A sum like the above can be computed using a running sum, similar to the running product used in the permutation argument:

$$ U_k:=\sum_{i=0}^{k-1} \frac{\mathsf{mult}_i}{\;\alpha - (\mathsf{inp}_i + a\cdot\mathsf{out}_i)\;} $$

Then the correctness can be ensured by a simple equation:

$$(U_{k+1} - U_k)\cdot \big(\alpha - (\mathsf{inp}_i + a\cdot\mathsf{out}_i)\big) = \mathsf{mult}_i$$

with the boundary constraints $U_0=U_{N+K}=0$, if we merge the LHS and the RHS of the original into a single sum of size $N+K$.

Similarly to the permutation argument, Plonky2 batches several such "running updates" together, since it's already have high-degree constraints:

$$U_{k+d} - U_k = \sum_{i=0}^{d-1} \frac{\mathsf{mult}_{k+i}}{\alpha - \mathsf{inp}_{k+i} - a\cdot\mathsf{out}_{k+i}  } =
\frac{\sum_{i=k}^{k+d-1}\mathsf{mult}_i\cdot\prod_{j\neq i}( \alpha - \mathsf{inp}_i - a\cdot\mathsf{out}_i)
}{\prod_{i=k}^{k+d-1}(\alpha - \mathsf{inp}_i - a\cdot\mathsf{out}_i)}$$

Also similarly to the permutation argument, they don't start from "zero", and reorder the columns so the final sum is at the beginning.

### Consistency check

Since the lookup table itself is _part of the witness_, instead of being some pre-committed polynomials, the verifier needs to check the authenticity of that too. For this purpose, what Plonky2 does is to create new combinations (called "combos" in the codebase):

$$ \mathsf{combo}'_i := \mathsf{inp}_i + b\cdot\mathsf{out}_i $$

then out of these, build a running sum of _partial Horner evaluations_ of the polynomial whose coefficients are $\mathsf{combo'}_i$:

$$ \mathtt{RE}_n := \sum_{i=0}^{26n-1}\delta^{26n-1-i}\left( \mathsf{inp}_{i} + b\cdot\mathsf{out}_{i} \right)
$$

(recall that in the standard configuration, there are $26=\lfloor 80/3\rfloor$ entries per row). This running sum is encoded in a single column (the first column of the lookup columns). 

Then the verifier recomputes all this (which have cost linear in the size of the table!), and checks the initial constraint ($\mathtt{RE}_0=0$), the final constrint $\mathtt{RE}_N$ is the expected sum, and the transition constraints:

$$ \mathtt{RE}_{n+1} = \mathtt{RE}_{n} + \sum_{j=0}^{25} \delta^{25-j} \cdot \mathsf{combo}'_{26n+j}$$

(note that this could be also written in Horner form).

### Naming conventions

As the code is based (very literally) on the Tip5 paper, it also inherits some of the really badly chosen names for there.

- `RE`, short for "running evaluation", refers to the table consistency check; basically the evaluation of a polynomial whose coefficients are the lookup table "combos" (in reverse order...)
- `LDC`, short for "logarithmic derivative ???", refers to the sum of fractions (with coefficients 1) on the left hand side of the fundamental equation, encoding the usage
- `Sum`, short for "summation" (really?!), refers the sum of fractions (with coefficients $\mathsf{mult}_i$) on the right hand side of the fundamental equation
- `SLDC` means `Sum - LDC` (we only compute a single running sum)
- `combo` means the linear combination $\mathsf{inp}_k+a\cdot \mathsf{out}_k$.

In formulas (it's a bit more complicated if there are more than 1 lookup tables, but this is the idea):

\begin{align*}
\mathtt{RE}_{\mathrm{final}}
 &= \sum_{k=0}^{K-1} \delta^{K-1-k}\big[\mathsf{inp}_k+b\cdot \mathsf{out}_k\big] \\
\mathtt{LDC}_{\mathrm{final}} &= \sum_j \frac{1}{\alpha - (\mathsf{inp}_j+a\cdot \mathsf{out}_j)}\\
\mathtt{Sum}_{\mathrm{final}} &= \sum_{k=0}^{K-1} \frac{\mathsf{mult}_k}{\alpha - (\mathsf{inp}_k+a\cdot \mathsf{out}_k\big)} \\
\mathtt{SLDC}_{k} &= \mathtt{Sum}_{k} - \mathtt{LDC}_{k}\\
\end{align*}

### Layout

Lookup tables are encoded in several `LookupTable` gates, which are just below each other, ordered from the left to right and **from bottom to top**. At the very bottom (so "before" the lookup table) there is an empty `NoopGate`. At the top of the lookup table gates are the actual lookup gates, but these are ordered **from top to bottom**. So the witness will look like this:

            |     ....      |
     ---    +---------------+
      v     |               |
      v     |     LU #1     |    <- lookups in the first  lookup table
      v     |               |
     ---    +---------------+
      ^     |               |
      ^     |               |
      ^     |    LUT #1     |    <- table entries (with  multiplicities)
      ^     |               |       of the first lookup  table
     ---    +---------------+
            |     NOOP      |    <- empty row (all zeros)
     ---    +---------------+
      v     |               |
      v     |     LU #2     |    <- lookups in the second lookup table
     ---    +---------------+
      ^     |               |
      ^     |    LUT #2     |    <- table entries in the second table
      ^     |               |
     ---    +---------------+
            |     NOOP      |    <- empty row
            +---------------+
            |     ....      |

Both type of gates are padded if necessary to get full rows. The `Lookup` gates are padded with the first entry in the corresponding table, while the `LookupTable` gates used to be padded with zeros (which was actually a soundness bug, as you could prove that `(0,0)` is an element of any table whose length is not divisible by 26), but this was fixed in [commit 091047f](https://github.com/0xPolygonZero/plonky2/commit/091047f7f10cae082716f3738ad59a583835f7b6), and now that's also padded by the first entry.


### Lookup selectors

Lookups come with their own selectors. There are always `4 + #luts` of them, nested between the gate selector columns and the constant columns.

These encode:

- 0: indicator function for the `LookupTable` gates rows
- 1: indicator function for the `Lookup` gate rows
- 2: indicator for the empty rows, or `{last_lut_row + 1}` (a singleton set for each LUT)
- 3: indicator for `{first_lu_row}`
- 4,5...: indicators for `{first_lut_row}` (each a singleton set)

Note: the lookup table gates are in bottom-to-top order, so the meaning of "first" and "last" in the code are sometimes not switched up... I tried to use the logical meaning above.

In the code these are called, respectively:

- 0: `TransSre` is for Sum and RE transition constraints.
- 1: `TransLdc` is for LDC transition constraints.
- 2: `InitSre` is for the initial constraint of Sum and Re.
- 3: `LastLdc` is for the final (S)LDC constraint.
- 4: `StartEnd` indicates where lookup end selectors begin.

These are used in the lookup equations.

This ASCII art graphics should be useful:

             witness           0 1 2 3 4 5
        +---------------+  -  +-+-+-+-+-+-+ 
        |               |     | |#| |#|   |
        |     LU #1     |     | |#| | |   |
        |               |     | |#| | |   |
        +---------------+  -  +-+-+-+-+-+-+
        |               |     |#|     |#| |
        |               |     |#|     | | |
        |    LUT #1     |     |#|     | | |
        |               |     |#|     | | |
        +---------------+  -  +-+-+-+-+-+-+
        |     NOOP      |     |   |#|     | 
        +---------------+  -  +-+-+-+-+-+-+ 
        |               |     | |#| |#|   |
        |     LU #2     |     | |#| | |   |
        +---------------+  -  +-+-+-+-+-+-+
        |               |     |#|       |#|
        |    LUT #2     |     |#|       | |
        |               |     |#|       | |
        |               |     |#|       | |
        +---------------+  -  +-+-+-+-+-+-+
        |     NOOP      |     |   |#|     | 
        +---------------+  -  +-+-+-+-+-+-+
             witness           0 1 2 3 4 5
                 
If there are several lookup tables, the last column is repeated for each of them; hence we have `4+#luts` lookup selector columns in total.

### The constraints

The number of constraints is `4 + #luts + 2 * #sldc`, where `#luts` denotes the number of different lookup tables, and `#sldc` denotes the number of SLDC columns (these contain the running sums). The latter is determined as 

$$ \#\mathtt{sldc} = \frac{\#\mathtt{lu\_slots}}{\;\mathtt{lookup\_deg}\;} = \left\lceil \frac{\lfloor 80/2 \rfloor}{\mathtt{maxdeg}-1} \right\rceil = \lceil 40 / 7 \rceil = 6 $$

Let's denote the SLDC columns with $A_i$ (for $0\le i<6$), the RE column with $R$, the witness columns with $W_j$, and the selectors columns with $S_k$. Similarly to the permutation argument, the running sum is encoded in row-major order with degree 7 "chunks", but here it's read _from the bottom to the top_ order...

Then these constraints are:

- the final SLDC sum is zero: $S_{\mathsf{LastLDC}}(x)\cdot A_5(x) = 0$ (this corresponds to actual core equation above)
- the initial `Sum` is zero: $S_{\mathsf{InitSre}}(x)\cdot A_0(x)=0$
- the inital `RE` is zero: $S_{\mathsf{InitSre}}(x)\cdot R(x)=0$
- final `RE` is the expected value, separately for each LUT: $S_{4+i}(x)\cdot R(x) = \mathsf{expected}_i$ where $i$ runs over the set of lookup tables, and the expected value is calculated simply as a polynomial evaluation as described above
- `RE` row transition constraint: $$ S_{\mathsf{TransSre}}(x)\left[ R(x) - \delta^{26}R(\omega x) - \sum_{j=0}^{25} \delta^{25-j}\big(W_{3j}(x)+b W_{3j+1}(x)\big) \right] = 0 $$
- row transition constraints for `LDC` and `SUM`, separately:

$$ 0 = S_\mathsf{TransLDC}(x)\left\{
A_{k}(x) - A_{k-1}(x) - 
\frac{\sum_{i=7k}^{7k+6}\cdot\prod_{j\neq i}( \alpha -  W_{2i}(x) - a W_{2i+1}(x))
}{\prod_{i=7k}^{7k+6}(\alpha - W_{2i}(x) - a W_{2i+1}(x))}
\right\}
$$

where for convience, let's have $A_{-1}(x) := A_{5}(\omega x)$

and similarly for the `SUM` constraint (but with mulitplicities included there). This is very similar to the how the [partial products](Wiring.md) work, just way over-complicated...

In the source code, all this can be found in the `check_lookup_constraints()` function in `vanishing_poly.rs`.

Remark: we have `lookup_deg = 7` instead of 8 because the selector adds an extra plus 1 to the degree. Since 40 is not divisible by 7, the last column will have a smaller product. Similarly, in the `SUM` case they use a smaller degree instead, namely: $\mathtt{lut_degree} = \lceil 26/7 \rceil = 4$; also with a truncated last column.

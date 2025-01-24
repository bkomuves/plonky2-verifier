Wiring (or permutation argument)
--------------------------------

The wiring constraints enfore equalities between concrete cells in the (routed) witness columns (by default the first 80 columns out of the 135 in total).

This is done essentially the same way as in the original Plonk paper, except generalized for more columns (80 instead of 3), and reducing the number of required running product columns because Plonky2 needs high-degree constraints anyway.

### Sub-protocol

The verifier samples $\beta_i,\gamma_i\in \mathbb{F}_p$ challenges for the permutations. There are $r = \mathtt{num\_challenges}$ number of these, which is set so that $(\deg/|\mathbb{F}|)^r$ is small enough. Typically $r\in\{2,3\}$, in this case $r=2$.

If "lookup gates" are present, the corresponding $\Delta_j$ challenges ($4r$ in total, but $2r$ reuses $\beta_i,\gamma_i$) are also sampled at this point.

The permutation $\sigma$ polynomials are encoded on as many disjoint cosets $k_iH$ as many routed columns are. The $k_i\in\mathbb{F}$ is chosen simply as $g^i$ with $g$ being a multiplicative generator of $\mathbb{F}^\times$ (however, these $k_i$-s are listed in the `CircuitCommonData`, with the very helpful name `"k_is"`...) They use the following generators:

\begin{align*}
g &= \mathtt{0xc65c18b67785d900} = 14293326489335486720 \\
h &= \mathtt{0x64fdd1a46201e246} = 7277203076849721926 = g^{(p-1)/2^{32}} \\
\omega &= h^{(2^{32}/2^{n})} \quad\quad\textrm{where}\quad\quad H=\langle \omega\rangle
\end{align*}

(remark: the smallest generator would be $g=7$).

So the Plonk permutation $\sigma$ is a permutation of $[N]\times[M]$ where there are $N=2^n$ rows and $M$ routed columns (in the default configuration we have $M=80$. The cells indices are then mapped into $\mathbb{F}$ by
 
\begin{align*}
 \phi &: [N]\times[M] \;\, \longrightarrow \mathbb{F}^\times \\
      &\quad\; (i\;\;,\;\;j)\quad \longmapsto k_j\cdot \omega^i
\end{align*}

where $\omega$ is the generator of the subgroup $H\subset \mathbb{F}^\times$ of size $n$.

We can then form the $2M$ polynomials encoding the permutation (these are part of the fixed circuit description - though in practice we don't store $S_\mathsf{id}$ as that's easy to compute):

\begin{align*}
S_\mathsf{id}^{(j)}( \omega^i ) &:= \phi(\phantom{\sigma}(i,j)) \\
S_\sigma^{(j)}     ( \omega^i ) &:= \phi(\sigma(i,j)) \\
\end{align*}

Next, we would normally compute the partial products

$$
A_k := \prod_{i=0}^{k-1} \prod_{j=0}^{M-1} \frac{W_{i,j} + \beta\cdot \phi((i,j)) + \gamma} {\; W_{i,j} + \beta\cdot\phi(\sigma(i,j))+\gamma \; }
$$

for $k\le 0<M$, exactly as in the Plonk protocol. However, a problem with this is that corresponding equations would have degree M, which is too big (recall that in our case $M=80$).

### The actual layout

So what Plonky2 does, is to just enumerate all the $N\times M$ terms 

$$
T_{i,j} := \frac{W_{i,j} + \beta\cdot \phi((i,j)) + \gamma} {\; W_{i,j} + \beta\cdot\phi(\sigma(i,j))+\gamma \; }
$$

into chunks determined by the maximal degree we allow (`max_quotient_degree_factor = 8`). Note: This can be confusing when looking at the Fibonacci example, as there we also have $N=2^3=8$, but this is just a coincidence! 

So we should get $NM/\mathsf{maxdeg}$ partial products. In our case $M/\mathsf{maxdeg} = 80/8 = 10$, so we can organize this into an $N\times 10$ matrix (in row-major order), resulting 10 "partial product columns".

Essentially the 80 routed columns' permutation argument is compressed into 10 partial product columns, and we will have equations ensuring that these are constructed correctly, and that the full product is 1 (which in turn proves the wire constraints).

Note: the Plonky2 source code uses some absolute horrible names here, and then does shiftings, reorderings, basically moving the last column to the first one, and calls this "z" while the rest "partial products", and then reorders even these between the challenge rounds; but I think this is just a +1 error and programmers not understanding what they are doing... Note that the first column needs to also opened "on the next row", while the rest only on "this row", but really, that's not a valid reason to do all this shit.

Here is an ASCII graphics explaning what happens in the source code (indices denote the corresponding partial product):

                                                        zs | partial_products
    +-----------------+      +-----------------+      +----+--------------+
    |  1  2 ...  9 10 |      |  1  2 ...  9  0 |      |  0 |  1  2 ...  9 |
    | 11 12 ... 19 20 |      | 11 12 ... 19 10 |      | 10 | 11 12 ... 19 |
    | 21 22 ... 29 30 |  ->  | 21 22 ... 29 20 |  ->  | 20 | 21 22 ... 29 |
    | 31 32 ... 39 40 |      | 31 32 ... 39 30 |      | 30 | 31 32 ... 39 |
    |                 |      |              40 |      | 40 |              |
    
First, the partial products are generated as in the first table. The final one (in the bottom-right corner) should be equal to 1. Then, the last column is shifted down, with constant 1 coming (denoted by 0 index) appearing in the top-right corner. Then, the last column is moved in the front. Finally (no picture for the lack of space), the first columns are separated from the rest and bundled together, so for $r$ challeges you will get $r$ first columns (called `"zs"`), and then $9r$ of the remaning columns (called `"partial products"`). Seriously, WTF.
    
In any case, then we commit to these (reordered!) $10\times r$ columns (together, as before). If there are lookup gates, the corresponding polynomials are also included here. The commitments are added to the Fiat-Shamir transcript too.

### Constraints

There are two types constraints (repeated for all challenge rounds): that the starting (and also ending, because of the cyclic nature of the multiplicative subgroup) value should equal to 1:

$$ \mathcal{L}_0(x)\cdot\big[ A_0(x) - 1 ] $$

where $\mathcal{L}$ denotes the Lagrange polynomials and $A_0(x)$ denotes the first column; and 10 transition constraints (one for each column), ensuring that the partial products are formed correctly:

$$
A_{i+1}(x) = A_i(x) \cdot \prod_{j=0}^7 \frac{W_{8i+j} + \beta\cdot k_{8i+j} x + \gamma}{W_{8i+j} + \beta \cdot \Sigma_{8i+j}(x) + \gamma}
$$

where $A_i$ denotes the $i$-th partial product column -- with the convention $A_{10}(x) = A_0(\omega x)$ to simplify the notation --, and $\Sigma_j$ denotes the "sigma" columns encoding the permutation.

Multiplying with the denominator we get a degree 9 constraint, but that's fine because the quotient polynomial will have only degree 8.




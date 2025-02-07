Recursive proofs
----------------

One of Plonky2 main features is its support for efficient recursive proofs. In a recursive proof system, an outer circuit verifies an inner proof by re‑implementing the entire verification protocol as arithmetic constraints. This allows us to aggregate many proofs into one final (relatively small) proof. In Plonky2, recursion proceeds with the following steps:

## 1. Virtual Targets

Before the outer (recursive) circuit can verify an inner proof, it must take the inner proof’s data as circuit targets. This is done by two main helper functions:

- 1. **`add_virtual_proof_with_pis`**  
    ```rust
    let inner_proof_target = 
        builder.add_virtual_proof_with_pis(&inner_common_circuit_data); 
    ```
  This creates a virtual target (`ProofWithPublicInputsTarget`) that represents the entire inner proof along with its public inputs. this virtual target contains the same components of the plonky proofs i.e. the following
  ```rust
    pub struct ProofTarget<const D: usize> {
        pub wires_cap: MerkleCapTarget,
        pub plonk_zs_partial_products_cap: MerkleCapTarget,
        pub quotient_polys_cap: MerkleCapTarget,
        pub openings: OpeningSetTarget<D>,
        pub opening_proof: FriProofTarget<D>,
    }
  ```
  Along with the above `ProofTarget`, a vector of public input targets corresponding exactly to the number public inputs of the inner proof is also added.
  As seen above adding a virtual target for the proof requires the common circuit data for the inner proof. This is required because the proof's components (which contain vectors of targets) need to be of known size (not dynamic). For instance, the `MerkleCapTarget` requires the `cap_height` for the inner circuit. These values are taken from the common circuit data which contain many paramaters (mostly in `usize`) and some are taken to generate the targets required for the proof. For all params see [here](https://github.com/0xPolygonZero/plonky2/blob/main/plonky2/src/plonk/circuit_data.rs#L418) but most importantly for the proof we require: `cap_height`, `num_public_inputs`, `num_wires`, `num_challenges`, `num_partial_products`, `num_lookup_polys`, `num_constants`, `quotient_degree_factor`, and `FriParams`. 

- 2. **`add_virtual_verifier_data`**  
    ```rust
  let inner_verifier_data = 
    builder.add_virtual_verifier_data(cap_height);
  ```
  This function creates a target (`VerifierCircuitTarget`) representing the inner verifier’s data. Again this includes the same components as the `VerifierOnlyCircuitData`:
  
  ```rust
    pub struct VerifierCircuitTarget {
        /// A commitment to each constant polynomial and each permutation polynomial.
        pub constants_sigmas_cap: MerkleCapTarget,
        /// A digest of the "circuit" which can be used to
        /// seed Fiat-Shamir.
        pub circuit_digest: HashOutTarget,
    }
  ```
  
  - **`constants_sigmas_cap`**: A Merkle cap commitment to the constant columns and the permutation “sigma” polynomials. These values describe the fixed (circuit‑dependent) data. Because this component is `MerkleCapTarget`, it requires `cap_height`, hence the need to input `cap_height` when calling `add_virtual_verifier_data`.
  - **`circuit_digest`**: A hash computed from the circuit’s fixed data. The digest is later used to seed the Fiat–Shamir transcript.

Together, the `ProofTarget` and `VerifierCircuitTarget` allow the outer circuit to “see” all of the inner proof’s components and the associated verifier data, so that it can re‑execute the inner verifier’s checks as part of its own constraints.


## 2. Re‑Computing Fiat–Shamir Challenges

Verifying an inner proof requires re‑generating the random challenges that the inner prover originally got using the Fiat–Shamir heuristic. Plonky2’s inner proofs require several challenges (see above in section Fiat-Shamir Challenges). The outer (recursive) circuit recomputes these challenges from the committed data in the `VerifierCircuitTarget`. 

In the code, recomputing these challenges is implemented by a function `get_challenges` which uses a Challenger (or RecursiveChallenger in the recursive setting) that “observes” each commitment (with functions such as `observe_hash` and `observe_cap`) and then “squeezes” the required challenges at different stages. The challenges are then packaged into a `ProofChallengesTarget` that is later used to verify the proof.

The "Challenger" is seeded by:
   - The **circuit digest** .
   - The **hash of the public inputs**.
   - The **fri openings**

the challenges produced are:
- **`plonk_betas`**
- **`plonk_gammas`**
- **`plonk_deltas`** (if lookups are present)
- **`plonk_alphas`**
- **`plonk_zeta`**
- And the FRI challenges: **`fri_alpha`**, **`fri_betas`**, **`fri_pow_response`**, and **`fri_query_indices`**. See above in [FRI](FRI.md)  section for what these are. 


## 3. Proof Verification With the Challenges
Once the challenges are generated, the proof verification proceeds in-circuit (using the circuit builder) in similar way as out of the circuit, i.e. following the steps (in short):

1. **Evaluating the Vanishing and Quotient Polynomials**
    Inside the recursive circuit, the following steps are performed:
    
    - **Vanishing Polynomial Evaluation:**  
  The function `eval_vanishing_poly_circuit` evaluates the combined vanishing polynomial at the challenge point $\zeta$. This involves:
      - Evaluating each gate’s constraint (with selectors filtering which gate constraints are active on each row).
      - Checking lookup constraints (if present).
      - Computing the permutation argument (using the running products).
      
  
    - **Quotient Polynomial Recombination:**  
  The inner proof provides the quotient polynomial in several chunks in `OpeningSetTarget` in the proof structure. The recursive verifier uses a reducing factor (`ReducingFactorTarget`) to recombine these chunks. It then multiplies the recombined quotient polynomial by $Z_H(\zeta)$ and enforces that the result equals the evaluation of the combined constraints (from previous step). Equality is enfored by "connect" here.

2. **FRI Proof Verification Inside the Circuit:**
    The recursive verifier implements FRI verification protocol inside the circuit as follows:

    - **Merkle Cap Verification:**  
  The inner proof commits to several matrices (for the witness, constants, running products, and quotient polynomial) via “Merkle caps.” The verifier uses these caps and the Merkle proofs (in the inner FRI proof) to check that the evaluations at the queried indices are consistent with the commitments.

    - **Reduction (Folding) Steps:**  
  The FRI protocol “folds” the LDE repeatedly each round reducing the degree until a final low‑degree polynomial is reached. The recursive verifier simulates these folding steps by:
      - Decomposing the query index into bits (to select the appropriate coset in the LDE).
      - Interpolate the coset evaluations (via helpers like `compute_evaluation`) to get the folded value.
      - Verifying that the folded evaluation is consistent with the previous one.
      - Checking the Merkle proofs at every reduction round.

    - **Proof‑of‑Work (PoW) Check:**  
      A PoW challenge is enforced by checking that a provided PoW witness (a field element) has the required number of leading zeros.

For more details refer to the section on [FRI](FRI.md).

## The Recursive Workflow in Practice

The overall workflow when building a recursive proof in Plonky2 is as follows:

1. **Circuit Building and Witness Generation:**  
   - An inner proof is generated over a standard Plonky2 circuit.
   - The inner proof is then imported into a new circuit by calling `add_virtual_proof_with_pis` (for the proof) and `add_virtual_verifier_data` (for the verifier data).

2. **Proof Verification as Constraints:**  
   - The outer circuit calls its own `verify_proof` method. This method:
     - Computes the Fiat–Shamir challenges.
     - Evaluates the combined vanishing polynomial at the challenge point and enforces that it equals to the expected one.
     - Invokes the FRI verification routine to simulate the full FRI verification protocol inside the circuit.
  
3. **Finalization:**  
   - Once all the recursive verification constraints are added to the outer circuit, the outer circuit is built and proved as usual.


## Notes on The VerifierData
Verifier data are important part of the recursive proof system because they contain the verification key for the inner circuit. that is, the circuit digest and the commitment to the constant and permutation (“sigma”) polynomials. Although the outer circuit only receives the common circuit data (which describes the structure and sizing of the proof components), that data alone does not bind the outer circuit to a specific inner circuit. So, a prover could provide any proof and verifier data that matches to the common data structure, even if they do not correspond to the intended inner circuit.

To prevent this, the verifier data must be made available to the verifier by: either registering the verifier data as public input or hardcoding it in the outer circuit (this option would limit the outer circuit to the specific inner circuit).

The simplist option is that the verifier data should be registered as public inputs in the outer circuit. This is typically done by calling `add_verifier_data_public_inputs`, which simply calls `add_virtual_verifier_data` and then registers every target in the resulting `VerifierCircuitTarget` as a public input. Additionally, it is important that the inner proof’s public inputs (or at least a hash of it) are also exposed as public inputs. these steps ensure that the inner proof is linked to a specific, publicly known verification key.

Exposing the verifier data as public inputs, requires the verifier to check it "off-circuit", Plonky2 provides the function `check_cyclic_proof_verifier_data`. (ignoring its name, it applies to any recursive proof where the verifier data are public.) This function compares the verifier data extracted from the inner proof’s public inputs with the expected verifier data. this check is especially important in cyclic recursion, where every inner proof must be linked to the same verification key.

It is technically possible to hardcode the verifier data in the circuit, but plonky2 doesn't seem to support this! might require a lot of refactoring! 

## Recursion Tools
Plonky2 implements a few (limited!) number of tools (functions) for recursion, mainly it is the following two: 

### 1. Conditional Recursion

The outer (recursion) circuit can be built to conditionally verify one of two input inner proofs based on a Boolean flag. Helper function `conditionally_verify_proof` and `select_proof_with_pis` use a Boolean target to select between two sets of proof targets and verifier data. This allows a circuit, for example, to choose between verifying a “real” inner proof or a dummy proof (useful in cyclic recursion base cases). Selecting the proof and verifier data is done by basicly going through all the targets and selecting either one based on the condition, in the [code](https://github.com/0xPolygonZero/plonky2/blob/main/plonky2/src/recursion/conditional_recursive_verifier.rs) you will see all the (basic) "select" functions. 

It is also possible to verify a "real" proof or a dummy based on the condition. To do this, one can use the function `conditionally_verify_proof_or_dummy` which takes:

```rust 
pub fn conditionally_verify_proof_or_dummy<C: GenericConfig<D, F = F> + 'static>(
        &mut self,
        condition: BoolTarget,
        proof_with_pis: &ProofWithPublicInputsTarget<D>,
        inner_verifier_data: &VerifierCircuitTarget,
        inner_common_data: &CommonCircuitData<F, D>,
    ) -> anyhow::Result<()>{
        ...
}
```
This function automates the generation of the dummy proof. A dummy proof in this context is a proof that matches the same structure as the "real" proof. To generate such dummy proof, the function calls `dummy_circuit` which is a circuit with mainly "noop" gates and the same size and set of gates used in the "real" inner circuit.

```rust
pub fn dummy_circuit<F: RichField + Extendable<D>, C: GenericConfig<D, F = F>, const D: usize>(
    common_data: &CommonCircuitData<F, D>,
) -> CircuitData<F, C, D> {
    ...
}
```
Then once a dummy circuit is built, a proof for this circuit is generated, and since the dummy circuit matches the "real" inner circuit, then the dummy proof would also match the "real" inner proof. 

```rust
pub fn dummy_proof<F: RichField + Extendable<D>, C: GenericConfig<D, F = F>, const D: usize>(
    circuit: &CircuitData<F, C, D>,
    nonzero_public_inputs: HashMap<usize, F>,
) -> anyhow::Result<ProofWithPublicInputs<F, C, D>>{
    ...
}
```
One thing to note is that if the "real" inner proof public input must be set to certain values, these must also be set in the dummy proof. 

Lastly, the verifier data are set to the verifier data from the dummy circuit. Plonky2 automates setting the values for the dummy proof and verifier data targets using `DummyProofGenerator` so there is no need to keep track of the dummy proof targets. 
`DummyProofGenerator` simply implements a `SimpleGenerator` and calls `set_proof_with_pis_target` and `set_verifier_data_target`. This means that all the fields required by the recursive verifier (such as the Merkle cap commitments, openings, and the FRI proof components) are populated with dummy values.

### 2. Cyclic Recursion

Cyclic recursion is used mainly for what is called IVC (incrementally verifiable computation). Meaning (in simple terms) that you create a circuit `C` that checks correct computation of function `f(s)` on the state `s` and returns an updated state `s'` and a proof $\pi$. Then you feed the `s'` and $\pi$ into another circuit that checks $\pi$ and runs the same circuit `C` but with input `s'` so this is basicly `f(f(s))`. Note that the second circuit also ensures that the output of the first circuit `s'` equals to the input to the second circuits. This process can be repeated many times as needed.

In cyclic recursion, the outer circuit needs to verify either an actual inner proof or a dummy proof, based on a Boolean condition. When the condition indicates that no “real” inner proof is available (in the base case of a cyclic recursion), a dummy proof is generated. The dummy proof is created in the same way as described before for the conditional verification. The `DummyProofGenerator` produces a proof populated with “dummy” values for all fields except for those carrying the cyclic verifier data. 

The inner verifier data (which includes the `constants_sigmas_cap` and the `circuit_digest`) are registered as public inputs via the `add_virtual_verifier_data`. The outer circuit then “connects” (i.e. ensures equal values are set) this verifier data with its own using the function `conditionally_verify_cyclic_proof`. This ensures that every proof in the cycle uses the same verification key. At the verification step, to guarantee this consistency, the outer circuit verifier can invoke the function `check_cyclic_proof_verifier_data`. This function compares the verifier data obtained from the inner proof’s public inputs with the expected verifier data.


## Recursion Examples


### Example: Simple Recursion
Now to actually do the recursion, we must first have a base circuit. So let's run an example of how this would work:

Let's say we have a circuit with just dummy gates `NoopGate` that do nothing. So we first create such a circuit, fill in the witness values (no witnesses are here in our example), and then build and generate the proof. See below:

```rust
// define params:
// the degree of the extension 
const D: usize = 2; 
//the configuration with hash function used (internally)
type C = PoseidonGoldilocksConfig; 
// the field which is the Goldilocks field
type F = <C as GenericConfig<D>>::F;
// the circuit config 
let config = CircuitConfig::standard_recursion_config();
// create new builder
let mut builder = CircuitBuilder::<F, D>::new(config.clone()); 
for _ in 0..num_dummy_gates {
    //fill the circuit with dummy gates 
    builder.add_gate(NoopGate, vec![]); 
}
// build the circuit
let inner_data = builder.build::<C>();
// fill in the witness values (none here)
let inputs = PartialWitness::new();
// generate the proof
let inner_proof = inner_data.prove(inputs)?;
// the verifier data (to send to the verifier)
let inner_vd = data.verifier_only;
inner_data.verify(proof.clone())?; 
```
Note: the above doesn't use the ZK configuration meaning zk is disabled. To enable zk (Zero-Knowledge) use: 
```rust
let config = CircuitConfig::standard_recursion_zk_config()
```
Now once we have the proof (or multiple proofs), we can verify that proof in-circuit and generate yet another proof, the recursive proof. To do so, we need to (again): 
- Define circuit params and builder 
- Create the recursive circuit, 
- Add the witness values (inner-proof and verifier data)
- Build the circuit
- Generate the proof (recursive proof)
- Verify

See example below:

```rust
// circuit builder
let mut builder = CircuitBuilder::<F, D>::new(config.clone());
// witness allocator
let mut pw = PartialWitness::new(); 
// a virtual target for the inner proof, here the target is a witness in the recursive circuit
let inner_proof_target = builder.add_virtual_proof_with_pis(&inner_cd);
// assign the inner_proof as witness
pw.set_proof_with_pis_target(&pt, &inner_proof)?;
// a virtual verifier data, so that the recursive circuit can use it to verify the proof
// it takes a cap_height which here is the same as the cap_height as the inner_proof
let inner_data = builder.add_virtual_verifier_data(inner_data.config.fri_config.cap_height);

// set the verifier data
pw.set_verifier_data_target(&inner_data, &inner_vd)?;

// verify the proof in-circuit, ini here InnerC is the configration `C` of the inner circuit
// this config could be different than the outer circuit (the recursive circuit)
builder.verify_proof::<InnerC>(&pt, &inner_data, &inner_cd);
// build circuit
let data = builder.build::<C>();
// verify
data.verify(proof.clone())?; 
```

### Example: Conditional Verification

In some cases you may want the recursive circuit to choose between verifying a real inner proof or a dummy one. This is useful in conditional and cyclic recursion. The dummy proof is generated (using the `DummyProofGenerator`) to fill in when no “real” proof is present. 

```rust
// Assume `inner_cd` is the CommonCircuitData for the inner circuit,
// and that we have already generated a real inner proof (`inner_proof`) and its verifier data (`inner_vd`).

// Create a Boolean condition target.
let condition = builder.add_virtual_bool_target_safe();

// Create virtual targets for the "real" inner proof
let real_inner_proof_target = builder.add_virtual_proof_with_pis(&inner_cd);

// Create virtual verifier data target.
let real_verifier_target = builder.add_virtual_verifier_data(inner_cd.config.fri_config.cap_height);

// Set the witness values for the real inner proof.
pw.set_proof_with_pis_target(&real_inner_proof_target, &inner_proof)?;
pw.set_verifier_data_target(&real_verifier_target, &inner_vd)?;

// the dummy proof is generated automatically by the DummyProofGenerator when calling the following function.

// Conditionally verify: if `condition` is true, verify the real inner proof; 
// otherwise, verify the dummy proof.
builder.conditionally_verify_proof_or_dummy::<C>(
    condition,
    &real_inner_proof_target,
    &real_verifier_target,
    &inner_cd,
);
```

### Example: Cyclic Recursion with Public Verifier Data
In cyclic recursion every inner proof must use the same verification key. For this purpose the verifier data is made public and then checked for consistency. The function check_cyclic_proof_verifier_data is used to enforce that the verifier data from the inner proof (from its public inputs) matches the expected verifier data. See the following example of this:

```rust
// Builder for the recursive circuit.
let mut builder = CircuitBuilder::<F, D>::new(config.clone());
let mut pw = PartialWitness::new();

// Add virtual targets for the inner proof and its verifier data.
let inner_proof_target = builder.add_virtual_proof_with_pis(&inner_cd);
// verifier data is register as public here
// (This ensures that the inner proof’s verifier data is visible to the verifier.)
let inner_verifier_target = builder.add_verifier_data_public_inputs(inner_cd.config.fri_config.cap_height);

// Set the witness for the inner proof and verifier data.
pw.set_proof_with_pis_target(&inner_proof_target, &inner_proof)?;
pw.set_verifier_data_target(&inner_verifier_target, &inner_vd)?;

// Create a Boolean condition target.
let condition = builder.add_virtual_bool_target_safe();


// Perform cyclic conditional verification: 
// 1. Either verify a "real" inner proof or dummy (if base case)
// 2. connect the inner proof’s verifier data (extracted from its public inputs)
// with the outer circuit’s verifier data. This forces every proof in the cycle to use the same verifier key.
builder.conditionally_verify_cyclic_proof_or_dummy::<C>(
    condition,  
    &inner_proof_target,
    &inner_cd,
);

// After generating the recursive proof, the verifier can check consistency (off-circuit) using:
check_cyclic_proof_verifier_data(&proof, &inner_vd, &inner_cd)?;

```
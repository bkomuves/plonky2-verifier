
A standalone Plonky2 verifier
-----------------------------

This is a (WIP) implementation of a Plonky2 verifier written in Haskell.

[Plonky2](https://github.com/0xPolygonZero/plonky2/) is a zero-knowledge proof
system developed by Polygon Zero, optimized for recursive proofs.

The goal here is to provide an executable specification (along a with less precise,
but still detailed [human language description](commentary/README.md)) of 
the Plonky2 verification algorithm. 

Another goal is to be a basis for further tooling (for example:
estimating verifier costs, helping the design of recursive circuits, generating 
Plonky2 verifier circuits for other proof systems, etc)

Note: It's deliberately not a goal for this verifier to be efficient; instead we 
try to focus on simplicity.

This code can be a bit ugly at some places. There is a reason for that, namely, 
that the semantics we have to emulate, are _somewhat idiosyncratic_...


### Implementation status

- [x] Parsing the proof and verification key from JSON
- [ ] Parsing from Plonky2's custom binary serialization
- [x] Generating verifier challenges
- [ ] Recursive circuit subtle details (like [this](https://github.com/0xPolygonZero/plonky2/blob/356aefb6863ac881fb71f9bf851582c915428458/plonky2/src/fri/challenges.rs#L55-L64]))
- [x] Constraints check
- [ ] FRI check
- [x] Support lookup tables
- [ ] Support zero-knowledge
- [x] Documenting Plonky2 internals and the verifier algorithm (WIP)
- [ ] Cabalize

Supported gates:

- [x] ArithmeticGate
- [x] ArithmeticExtensionGate
- [x] BaseSumGate
- [x] CosetInterpolationGate
- [x] ConstantGate
- [x] ExponentiationGate
- [x] LookupGate
- [x] LookupTableGate
- [x] MulExtensionGate
- [x] NoopGate
- [x] PublicInputGate
- [x] PoseidonGate
- [x] PoseidonMdsGate
- [x] RandomAccessGate
- [x] ReducingGate
- [x] ReducingExtensionGate

Optional features:

- [ ] Supporting different hash functions
- [ ] Handle non-standard configurations
- [ ] Field extensions with degree higher than 2
- [ ] Being parametric over the field choice


#!/bin/bash

OUTPUT="commentary.pdf"

MD_FILES="\
 Overview.md\
 Gates.md\
 Selectors.md\
 GateConstraints.md\
 Wiring.md\
 Poseidon.md\
 FRI.md\
 Challenges.md\
 Protocol.md\
 Lookups.md\
 Recursion.md"

#echo ${MD_FILES}

pandoc -o $OUTPUT ${MD_FILES}

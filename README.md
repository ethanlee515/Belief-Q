# Belief-Q

Belief propagation based quantum error correction in SpinalHDL.

Usage:
* `./mill BeliefQ.test`: run an assortment of unit tests for correctness
* `./mill BeliefQ.test.runMain beliefq.test.VanillaVerilog`: generate Verilog for Vanilla BP
* `./mill BeliefQ.test.runMain beliefq.test.DMemVerilog`: generate Verilog for DMemBP
* `./mill BeliefQ.test.runMain beliefq.test.RelayVerilog`: generate Verilog for Relay BP

## Setup

We provide a [Dockerfile](./.devcontainer/Dockerfile) and its associated DevContainer setup [configurations](./.devcontainer/devcontainer.json).
It is also possible to build this project without Docker, using sufficiently recent versions of JDK and Verilator.

## Algorithms

Currently "vanilla", "DMem", and "Relay" versions of belief propagation are supported

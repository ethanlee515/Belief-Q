# Belief-Q

Belief propagation based quantum error correction in SpinalHDL.

Usage:
* `./mill BeliefQ.test.runMain beliefq.test.DoSim`: Simulate a hundred cycles or so, and log some stuff
* `./mill BeliefQ.test.runMain beliefq.test.DMemSim`: "DMemBP" version of the above
* `./mill BeliefQ.test.runMain beliefq.test.VivadoBench`: Try Vivado build. (Obviously only works if you have Vivado installed.)
* `./mill BeliefQ.test.runMain beliefq.test.VivadoBenchDmem`: "DMemBP" version of the above
* `./mill BeliefQ.test`: run an assortment of unit tests for correctness

## Setup

We provide a [Dockerfile](./.devcontainer/Dockerfile) and its associated DevContainer setup [configurations](./.devcontainer/devcontainer.json).
It is also possible to build this project without Docker, using sufficiently recent versions of JDK and Verilator.

## Algorithms

Currently "vanilla" and "DMem" versions of belief propagation are supported

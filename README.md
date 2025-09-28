# Belief-Q

Belief propagation based quantum error correction in SpinalHDL.

Usage:
* `./mill BeliefQ.runMain beliefq.CompileVerilog`: Output Verilog as "./BelieQ.v" for surface code geometry
* `./mill BeliefQ.test`: run tests. (TODO: The test sometimes fails, most likely because belief propagation does not always converge.)
* `./mill BeliefQ.test.runMain beliefq.test.DoSim`: generate some test syndromes inputs and simulate

## Setup

We provide a [Dockerfile](./.devcontainer/Dockerfile) and its associated DevContainer setup [configurations](./.devcontainer/devcontainer.json).
It is also possible to build this project without Docker, using the following dependencies:
* A relatively recent JDK
* Verilator

## Algorithms

We currently only consider a subset of [Relay-BP](https://arxiv.org/abs/2506.01779) features.
We will hopefully have something more exciting later.

## TODO

* "Checks" and "Factors" are used in an interchangable and inconsistent way. Stick to one or the other.

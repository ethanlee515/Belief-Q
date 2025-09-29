# Belief-Q

Belief propagation based quantum error correction in SpinalHDL.

Usage:
* `./mill BeliefQ.runMain beliefq.CompileVerilog`: TODO Compile and output verilog
* `./mill BeliefQ.runMain beliefq.VivadoBuild`: TODO Try Vivado build. (Obviously only works if you have Vivado installed.)
* `./mill BeliefQ.test`: run tests

## Setup

We provide a [Dockerfile](./.devcontainer/Dockerfile) and its associated DevContainer setup [configurations](./.devcontainer/devcontainer.json).
It is also possible to build this project without Docker, using relatively recent versions of JDK and Verilator.

## Algorithms

We currently only consider a subset of [Relay-BP](https://arxiv.org/abs/2506.01779) features.
We will hopefully have something more exciting later.

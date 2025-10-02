# Belief-Q

Belief propagation based quantum error correction in SpinalHDL.

Usage:
* `./mill BeliefQ.test.runMain beliefq.test.DoSim`: Simulate a hundred cycles or so, and log some stuff
* `./mill BeliefQ.test.runMain beliefq.test.VivadoBench`: Try Vivado build. (Obviously only works if you have Vivado installed.)
* `./mill BeliefQ.test`: run an assortment of unit tests

## Setup

We provide a [Dockerfile](./.devcontainer/Dockerfile) and its associated DevContainer setup [configurations](./.devcontainer/devcontainer.json).
It is also possible to build this project without Docker, using sufficiently recent versions of JDK and Verilator.

## Algorithms

This is currently only vanilla belief propagation.
Will add DMemBP soon.

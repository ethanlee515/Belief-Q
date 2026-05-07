#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
from pathlib import Path

import numpy as np
import stim


def detect_data_qubits(circuit: stim.Circuit) -> list[int]:
    """Detect data qubits as qubits measured exactly once without gate args."""
    qubit_times_measured = [0 for _ in range(circuit.num_qubits)]

    for inst in circuit:
        if inst.name.startswith("M") and not inst.gate_args_copy():
            for qubit in inst.targets_copy():
                qubit_times_measured[qubit.qubit_value] += 1

    return [
        qubit
        for qubit, times_measured in enumerate(qubit_times_measured)
        if times_measured == 1
    ]


def filter_detectors_by_basis(
    circuit: stim.Circuit,
    basis: str,
    qubits: list[int] | None = None,
) -> stim.Circuit:
    """Keep only detectors sensitive to data-qubit errors of the requested basis.

    This is the same strategy as scratch/stim_file.py, kept standalone so this
    converter can be run directly from the repository root.
    """
    if basis not in ("X", "Z"):
        raise ValueError("basis must be X or Z")

    pauli_error = "Z" if basis == "X" else "X"
    circuit = circuit.flattened()
    noiseless_circuit = circuit.without_noise()

    sampler = noiseless_circuit.compile_detector_sampler()
    reference_detectors, _ = sampler.sample(1, separate_observables=True)
    reference_detectors = reference_detectors[0, :]
    detector_is_sensitive = np.full(len(reference_detectors), False, dtype=bool)

    to_test = detect_data_qubits(noiseless_circuit) if qubits is None else list(qubits)
    to_test_set = set(to_test)

    inst_idx = 0
    while to_test:
        for qubit in to_test:
            injected_circuit = stim.Circuit()
            injected_circuit += noiseless_circuit
            injected_circuit.insert(
                inst_idx,
                stim.CircuitInstruction(f"{pauli_error}_ERROR", [qubit], [1.0]),
            )

            injected_sampler = injected_circuit.compile_detector_sampler()
            injected_detectors, _ = injected_sampler.sample(1, separate_observables=True)
            injected_detectors = injected_detectors[0, :]
            detector_is_sensitive |= reference_detectors != injected_detectors

        to_test = []
        for inst in noiseless_circuit[inst_idx:]:
            inst_idx += 1
            if inst.name.startswith("R") or inst.name.startswith("M"):
                to_test = list(to_test_set)
                break

    filtered_circuit = stim.Circuit()
    detector_idx = 0
    for inst in circuit:
        if inst.name == "DETECTOR":
            if detector_is_sensitive[detector_idx]:
                filtered_circuit.append(inst)
            detector_idx += 1
        else:
            filtered_circuit.append(inst)

    return filtered_circuit


def detector_error_model_to_chkmat(
    dem: stim.DetectorErrorModel,
) -> tuple[list[list[bool]], int, int]:
    """Convert a detector error model into chkmat.json's row-major shape."""
    dem = dem.flattened()
    num_checks = dem.num_detectors
    columns: list[set[int]] = []
    skipped_without_detectors = 0

    for inst in dem:
        if inst.type != "error":
            continue

        detectors: set[int] = set()
        for target in inst.targets_copy():
            if target.is_relative_detector_id():
                detectors.add(target.val)

        if detectors:
            columns.append(detectors)
        else:
            skipped_without_detectors += 1

    chkmat = [[False for _ in range(len(columns))] for _ in range(num_checks)]
    for var_idx, detectors in enumerate(columns):
        for check_idx in detectors:
            chkmat[check_idx][var_idx] = True

    return chkmat, len(columns), skipped_without_detectors


def write_chkmat(path: Path, chkmat: list[list[bool]]) -> None:
    with path.open("w") as f:
        json.dump(chkmat, f)
        f.write("\n")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Convert a Stim circuit into Belief-Q chkmat JSON."
    )
    parser.add_argument("stim_file", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument(
        "--detector-basis",
        choices=("X", "Z"),
        help="Optionally filter detectors by basis before building the graph.",
    )
    args = parser.parse_args()

    circuit = stim.Circuit.from_file(args.stim_file)
    original_num_detectors = circuit.num_detectors
    if args.detector_basis is not None:
        circuit = filter_detectors_by_basis(circuit, args.detector_basis)

    dem = circuit.detector_error_model(decompose_errors=False)
    chkmat, num_vars, skipped_without_detectors = detector_error_model_to_chkmat(dem)
    write_chkmat(args.output, chkmat)

    num_edges = sum(sum(row) for row in chkmat)
    print(f"input: {args.stim_file}")
    print(f"output: {args.output}")
    print(f"detectors: {original_num_detectors} -> {len(chkmat)}")
    print(f"variables: {num_vars}")
    print(f"edges: {num_edges}")
    print(f"skipped logical-only/no-detector errors: {skipped_without_detectors}")


if __name__ == "__main__":
    main()

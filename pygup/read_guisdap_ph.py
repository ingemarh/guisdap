#!/usr/bin/env python3
"""Read and inspect an E3D/GUISDAP PH file and its JSON sidecar.

OVERVIEW
========
The ``.ph`` file used here is a simple project-specific binary stream. It is
not a standard NumPy, HDF5, or GUISDAP container. The file consists of one
record per accepted radar IPP:

    [64-byte PH header][complex sample data]
    [64-byte PH header][complex sample data]
    ...

For the current Kaiseniemi VHF files, every record normally contains:

    64-byte header
    1000 complex samples
    4 bytes per complex sample (two signed int16 components)

Therefore, a normal record occupies:

    64 + 1000 * 4 = 4064 bytes

The PH file contains only accepted IPPs. Satellite/coherent-echo contaminated
IPPs have been omitted. Time is NOT compressed: every retained record has its
actual UTC start time in header field ``dt``, so removed IPPs appear as gaps
between consecutive timestamps.

WHY A JSON SIDECAR IS REQUIRED
==============================
The PH header does not contain every detail needed to reproduce the original
second-level-beamformer values. The accompanying ``.json`` file documents:

* byte order (normally little-endian);
* component order (normally ``imag-real``);
* int16-to-beamformer scale factor;
* record count and record size;
* source observation, mask, pointing, and provenance;
* clipping and read-back validation results;
* SHA-256 checksums.

The integer data are converted back to beamformer units as:

    complex_value = (real_count + 1j * imag_count) / scale

where ``scale`` is ``scale_counts_per_input_unit`` from the JSON sidecar.

HEADER LAYOUT (64 bytes, little-endian)
=======================================
``dt``   uint64     Unix UTC microseconds at the start of this IPP
``bc``   uint16     Big-code index; currently 1 for uncoded long pulse
``lc``   uint16     Little-code index; currently 0
``az``   float32    Receiver pointing azimuth in degrees
``el``   float32    Receiver pointing elevation in degrees
``pw``   uint16     Transmitted pulse width in microseconds
``bw``   uint16     Sample interval in microseconds
``ipp``  uint16     IPP length in microseconds
``ws``   uint16     First stored sample offset from IPP start, microseconds
``nd``   uint16     Number of complex samples following this header
``at``   float32    Transmitter azimuth in degrees
``et``   float32    Transmitter elevation in degrees
``rf``   float32    Centre frequency in Hz
``st``   uint16     Site index
``Pad``  uint16[10] Project-specific extension/reserved fields

Current Pad convention:

    Pad[0] = format version (1)
    Pad[1] = FSRU beam number (5)
    Pad[2] = polarization code (3 means Pol C)
    Pad[3] = cadence/format indicator (1)
    Pad[4] = number of active antenna units (5 for SE5)
    Pad[5:10] = reserved, currently zero

DATA LAYOUT
===========
The present files use signed little-endian int16 components. The normal order
is ``imag-real``:

    imag0, real0, imag1, real1, ...

This order is read from the JSON sidecar rather than silently assumed.

EXAMPLES
========
Print a summary and inspect the first three records:

    uv run python read_guisdap_ph.py observation.ph --records 3

Read records in a UTC interval and export them to NPZ as complex64:

    uv run python read_guisdap_ph.py observation.ph \
        --start-utc 2026-09-17T11:17:13Z \
        --duration-seconds 60 \
        --export-npz one_minute.npz

Verify the PH SHA-256 checksum stored in the JSON sidecar:

    uv run python read_guisdap_ph.py observation.ph --verify-sha256
"""
from __future__ import annotations

import argparse
import ctypes
import hashlib
import json
import math
import sys
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterator

import numpy as np

HEADER_BYTES = 64


class PhaHead(ctypes.LittleEndianStructure):
    """Exact in-file representation of the 64-byte PH header."""

    _layout_ = "ms"
    _pack_ = 1
    _fields_ = [
        ("dt", ctypes.c_uint64),
        ("bc", ctypes.c_uint16),
        ("lc", ctypes.c_uint16),
        ("az", ctypes.c_float),
        ("el", ctypes.c_float),
        ("pw", ctypes.c_uint16),
        ("bw", ctypes.c_uint16),
        ("ipp", ctypes.c_uint16),
        ("ws", ctypes.c_uint16),
        ("nd", ctypes.c_uint16),
        ("at", ctypes.c_float),
        ("et", ctypes.c_float),
        ("rf", ctypes.c_float),
        ("st", ctypes.c_uint16),
        ("Pad", ctypes.c_uint16 * 10),
    ]


if ctypes.sizeof(PhaHead) != HEADER_BYTES:
    raise RuntimeError(
        f"Internal error: PhaHead is {ctypes.sizeof(PhaHead)} bytes, "
        f"not {HEADER_BYTES} bytes"
    )


@dataclass(frozen=True)
class PHRecord:
    """One decoded PH record.

    ``samples`` are complex64 values in the original second-level-beamformer
    units, after reversing the global int16 scale from the JSON sidecar.
    """

    index: int
    file_offset: int
    header: PhaHead
    samples: np.ndarray

    @property
    def utc_datetime(self) -> datetime:
        return datetime.fromtimestamp(
            int(self.header.dt) / 1_000_000.0,
            tz=timezone.utc,
        )

    @property
    def fast_time_us(self) -> np.ndarray:
        return (
            int(self.header.ws)
            + np.arange(int(self.header.nd), dtype=np.float64)
            * int(self.header.bw)
        )


class PHReader:
    """Streaming reader for one PH file and its JSON sidecar."""

    def __init__(self, ph_path: Path, json_path: Path | None = None) -> None:
        self.ph_path = ph_path.expanduser().resolve()
        self.json_path = (
            json_path.expanduser().resolve()
            if json_path is not None
            else self.ph_path.with_suffix(".json")
        )

        if not self.ph_path.is_file():
            raise FileNotFoundError(self.ph_path)
        if not self.json_path.is_file():
            raise FileNotFoundError(self.json_path)

        self.metadata = json.loads(self.json_path.read_text(encoding="utf-8"))
        if not isinstance(self.metadata, dict):
            raise ValueError("JSON sidecar must contain a JSON object")

        self.byte_order = self.metadata.get("byte_order", "little-endian")
        if self.byte_order != "little-endian":
            raise ValueError(
                f"Unsupported byte order {self.byte_order!r}; "
                "this reader currently supports little-endian PH files"
            )

        self.component_order = self.metadata.get("sample_component_order")
        if self.component_order not in {"imag-real", "real-imag"}:
            raise ValueError(
                "JSON field sample_component_order must be "
                "'imag-real' or 'real-imag'"
            )

        self.scale = float(self.metadata.get("scale_counts_per_input_unit"))
        if not math.isfinite(self.scale) or self.scale <= 0:
            raise ValueError("Invalid scale_counts_per_input_unit in JSON")

        self.record_count = int(self.metadata.get("record_count", -1))
        self.samples_per_record = int(
            self.metadata.get("samples_per_record", -1)
        )
        self.record_bytes = int(self.metadata.get("record_bytes", -1))

        if self.record_count <= 0:
            raise ValueError("Invalid record_count in JSON")
        if self.samples_per_record <= 0:
            raise ValueError("Invalid samples_per_record in JSON")

        expected_record_bytes = HEADER_BYTES + 4 * self.samples_per_record
        if self.record_bytes != expected_record_bytes:
            raise ValueError(
                f"JSON record_bytes={self.record_bytes}, but header and sample "
                f"count imply {expected_record_bytes} bytes"
            )

        expected_file_bytes = self.record_count * self.record_bytes
        actual_file_bytes = self.ph_path.stat().st_size
        if actual_file_bytes != expected_file_bytes:
            raise ValueError(
                f"PH size is {actual_file_bytes} bytes, expected "
                f"{expected_file_bytes} bytes from JSON metadata"
            )

    def _decode_samples(self, raw: bytes, nd: int) -> np.ndarray:
        """Decode one record's interleaved int16 components."""

        components = np.frombuffer(raw, dtype="<i2")
        if components.size != 2 * nd:
            raise RuntimeError(
                f"Expected {2 * nd} int16 components, got {components.size}"
            )
        components = components.reshape(nd, 2)

        if self.component_order == "imag-real":
            imag_counts = components[:, 0].astype(np.float32)
            real_counts = components[:, 1].astype(np.float32)
        else:
            real_counts = components[:, 0].astype(np.float32)
            imag_counts = components[:, 1].astype(np.float32)

        # Reverse the global scale used by the PH exporter.
        return np.asarray(
            (real_counts + 1j * imag_counts) / self.scale,
            dtype=np.complex64,
        )

    def read_record(self, index: int) -> PHRecord:
        """Random-access read of one record by zero-based record index."""

        if not 0 <= index < self.record_count:
            raise IndexError(
                f"Record index {index} outside 0..{self.record_count - 1}"
            )

        file_offset = index * self.record_bytes
        with self.ph_path.open("rb") as handle:
            handle.seek(file_offset)
            header_raw = handle.read(HEADER_BYTES)
            if len(header_raw) != HEADER_BYTES:
                raise RuntimeError(f"Short header at record {index}")
            header = PhaHead.from_buffer_copy(header_raw)

            if int(header.nd) != self.samples_per_record:
                raise RuntimeError(
                    f"Record {index} has nd={header.nd}, expected "
                    f"{self.samples_per_record}"
                )

            data_raw = handle.read(int(header.nd) * 4)
            if len(data_raw) != int(header.nd) * 4:
                raise RuntimeError(f"Short data payload at record {index}")

        return PHRecord(
            index=index,
            file_offset=file_offset,
            header=header,
            samples=self._decode_samples(data_raw, int(header.nd)),
        )

    def records(
        self,
        start_utc_us: int | None = None,
        stop_utc_us: int | None = None,
    ) -> Iterator[PHRecord]:
        """Stream records, optionally selecting a half-open UTC interval.

        The interval is ``start_utc_us <= dt < stop_utc_us``. Streaming avoids
        loading the complete five-minute file into memory.
        """

        with self.ph_path.open("rb") as handle:
            for index in range(self.record_count):
                file_offset = index * self.record_bytes
                header_raw = handle.read(HEADER_BYTES)
                if len(header_raw) != HEADER_BYTES:
                    raise RuntimeError(f"Short header at record {index}")
                header = PhaHead.from_buffer_copy(header_raw)
                nd = int(header.nd)
                data_raw = handle.read(nd * 4)
                if len(data_raw) != nd * 4:
                    raise RuntimeError(f"Short data payload at record {index}")

                timestamp = int(header.dt)
                if start_utc_us is not None and timestamp < start_utc_us:
                    continue
                if stop_utc_us is not None and timestamp >= stop_utc_us:
                    break

                yield PHRecord(
                    index=index,
                    file_offset=file_offset,
                    header=header,
                    samples=self._decode_samples(data_raw, nd),
                )

    def verify_timestamps(self) -> dict[str, int | float]:
        """Check timestamp ordering and report gaps between retained IPPs."""

        timestamps = np.fromiter(
            (int(record.header.dt) for record in self.records()),
            dtype=np.int64,
            count=self.record_count,
        )
        differences = np.diff(timestamps)
        if np.any(differences <= 0):
            raise RuntimeError("PH timestamps are not strictly increasing")

        return {
            "first_utc_us": int(timestamps[0]),
            "last_utc_us": int(timestamps[-1]),
            "minimum_step_us": int(differences.min()),
            "median_step_us": float(np.median(differences)),
            "maximum_step_us": int(differences.max()),
            "steps_above_10ms": int(np.count_nonzero(differences > 10_001)),
        }

    def verify_sha256(self) -> str:
        """Calculate and compare the PH SHA-256 checksum from the JSON file."""

        expected = self.metadata.get("output_ph_sha256")
        if not expected:
            raise ValueError("JSON sidecar has no output_ph_sha256 field")

        digest = hashlib.sha256()
        with self.ph_path.open("rb") as handle:
            while True:
                block = handle.read(8 * 1024 * 1024)
                if not block:
                    break
                digest.update(block)
        actual = digest.hexdigest()

        if actual != expected:
            raise RuntimeError(
                f"SHA-256 mismatch: JSON has {expected}, file gives {actual}"
            )
        return actual


def parse_utc(text: str) -> int:
    """Parse ISO UTC text and return Unix UTC microseconds."""

    value = text.strip()
    if value.endswith("Z"):
        value = value[:-1] + "+00:00"
    parsed = datetime.fromisoformat(value)
    if parsed.tzinfo is None:
        parsed = parsed.replace(tzinfo=timezone.utc)
    parsed = parsed.astimezone(timezone.utc)
    return int(round(parsed.timestamp() * 1_000_000.0))


def iso_utc(microseconds: int) -> str:
    return datetime.fromtimestamp(
        microseconds / 1_000_000.0,
        tz=timezone.utc,
    ).isoformat().replace("+00:00", "Z")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("ph_file", type=Path, help="Input .ph file")
    parser.add_argument(
        "--json",
        type=Path,
        help="JSON sidecar; default is the PH filename with .json suffix",
    )
    parser.add_argument(
        "--records",
        type=int,
        default=1,
        help="Print this many records from the selected interval",
    )
    parser.add_argument("--start-utc", help="Optional ISO UTC selection start")
    parser.add_argument(
        "--duration-seconds",
        type=float,
        help="Selection duration; requires --start-utc",
    )
    parser.add_argument(
        "--export-npz",
        type=Path,
        help="Export selected records as a simple complex64 NPZ",
    )
    parser.add_argument(
        "--verify-sha256",
        action="store_true",
        help="Verify PH checksum against the JSON sidecar",
    )
    return parser


def print_header(record: PHRecord) -> None:
    header = record.header
    print(f"Record {record.index}")
    print(f"  File offset:       {record.file_offset} bytes")
    print(f"  UTC:               {record.utc_datetime.isoformat()}")
    print(f"  dt:                {int(header.dt)} Unix UTC us")
    print(f"  bc / lc:           {int(header.bc)} / {int(header.lc)}")
    print(f"  Az / El:           {float(header.az):.3f} / {float(header.el):.3f} deg")
    print(f"  Pulse width:       {int(header.pw)} us")
    print(f"  Sample interval:   {int(header.bw)} us")
    print(f"  IPP:               {int(header.ipp)} us")
    print(f"  Window start:      {int(header.ws)} us")
    print(f"  Complex samples:   {int(header.nd)}")
    print(f"  Tx Az / El:        {float(header.at):.3f} / {float(header.et):.3f} deg")
    print(f"  RF:                {float(header.rf):.1f} Hz")
    print(f"  Site index:        {int(header.st)}")
    print(f"  Pad:               {list(header.Pad)}")
    print(f"  Data dtype:        {record.samples.dtype}")
    print(f"  First 5 samples:   {record.samples[:5]}")
    print(f"  Fast-time range:   {record.fast_time_us[0]:.1f} to "
          f"{record.fast_time_us[-1]:.1f} us")


def main() -> None:
    args = build_parser().parse_args()
    if args.records < 0:
        raise ValueError("--records cannot be negative")
    if args.duration_seconds is not None and args.start_utc is None:
        raise ValueError("--duration-seconds requires --start-utc")

    reader = PHReader(args.ph_file, args.json)

    print("PH file summary")
    print("=" * 72)
    print(f"PH file:              {reader.ph_path}")
    print(f"JSON sidecar:         {reader.json_path}")
    print(f"Format:               {reader.metadata.get('format')}")
    print(f"Byte order:           {reader.byte_order}")
    print(f"Component order:      {reader.component_order}")
    print(f"Record count:         {reader.record_count}")
    print(f"Samples per record:   {reader.samples_per_record}")
    print(f"Record bytes:         {reader.record_bytes}")
    print(f"PH file bytes:        {reader.ph_path.stat().st_size}")
    print(f"Scale:                {reader.scale:.12g} counts/input-unit")
    print(f"Inverse scale:        {1.0 / reader.scale:.12g} input-units/count")
    print(f"Observation ID:       {reader.metadata.get('observation_id')}")
    print(f"Polarization:         {reader.metadata.get('polarization')}")
    print(f"Beam number:          {reader.metadata.get('beam_number')}")

    timing = reader.verify_timestamps()
    print()
    print("Timing")
    print("=" * 72)
    print(f"First retained IPP:   {iso_utc(timing['first_utc_us'])}")
    print(f"Last retained IPP:    {iso_utc(timing['last_utc_us'])}")
    print(f"Minimum step:         {timing['minimum_step_us']} us")
    print(f"Median step:          {timing['median_step_us']} us")
    print(f"Maximum step:         {timing['maximum_step_us']} us")
    print(f"Steps above 10 ms:    {timing['steps_above_10ms']}")
    print("  Larger steps are expected where IPPs were removed or across source-second gaps.")

    if args.verify_sha256:
        checksum = reader.verify_sha256()
        print()
        print(f"[OK] SHA-256: {checksum}")

    start_us = parse_utc(args.start_utc) if args.start_utc else None
    stop_us = None
    if start_us is not None and args.duration_seconds is not None:
        stop_us = start_us + int(round(args.duration_seconds * 1_000_000.0))

    selected_records: list[PHRecord] = []
    for record in reader.records(start_us, stop_us):
        if len(selected_records) < args.records or args.export_npz is not None:
            selected_records.append(record)
        if args.export_npz is None and len(selected_records) >= args.records:
            break

    if args.records:
        print()
        print("Selected record inspection")
        print("=" * 72)
        for record in selected_records[: args.records]:
            print_header(record)
            print()

    if args.export_npz is not None:
        if not selected_records:
            raise RuntimeError("No records selected for NPZ export")
        output = args.export_npz.expanduser().resolve()
        samples = np.stack([record.samples for record in selected_records])
        utc_microseconds = np.asarray(
            [int(record.header.dt) for record in selected_records],
            dtype=np.int64,
        )
        fast_time_us = selected_records[0].fast_time_us
        np.savez_compressed(
            output,
            complex_samples=np.asarray(samples, dtype=np.complex64),
            ipp_utc_microseconds=utc_microseconds,
            fast_time_us=fast_time_us,
            source_ph=np.asarray(str(reader.ph_path)),
            source_json=np.asarray(str(reader.json_path)),
            component_order=np.asarray(reader.component_order),
            scale_counts_per_input_unit=np.asarray(reader.scale),
        )
        print(f"[OK] Exported {samples.shape[0]} records to {output}")
        print(f"     Data shape: {samples.shape}, dtype: {samples.dtype}")


if __name__ == "__main__":
    try:
        main()
    except (
        FileNotFoundError,
        IndexError,
        KeyError,
        ValueError,
        RuntimeError,
        json.JSONDecodeError,
        OSError,
    ) as error:
        print(f"ERROR: {error}", file=sys.stderr)
        raise SystemExit(2) from error

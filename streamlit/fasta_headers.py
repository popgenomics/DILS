"""Helpers for detecting population names from DILS FASTA headers."""

from __future__ import annotations

from dataclasses import dataclass
from io import BytesIO, StringIO
from typing import Iterable, TextIO


@dataclass(frozen=True)
class FastaHeaderSummary:
    populations: list[str]
    valid_headers: int
    malformed_headers: int


def _decode_line(line: str | bytes) -> str:
    if isinstance(line, bytes):
        return line.decode("utf-8", errors="replace")
    return line


def _iter_lines(source: str | bytes | Iterable[str | bytes] | TextIO):
    if isinstance(source, str):
        yield from StringIO(source)
        return
    if isinstance(source, bytes):
        yield from BytesIO(source)
        return

    seek = getattr(source, "seek", None)
    if callable(seek):
        try:
            seek(0)
        except OSError:
            pass
    yield from source


def parse_fasta_headers(source: str | bytes | Iterable[str | bytes] | TextIO) -> FastaHeaderSummary:
    """
    Parse DILS FASTA headers and return detected population names.

    Expected header format:
        >locus|population|individual|allele

    Spaces around pipe separators are tolerated. Malformed headers are ignored.
    """
    populations: list[str] = []
    seen: set[str] = set()
    valid_headers = 0
    malformed_headers = 0

    for raw_line in _iter_lines(source):
        line = _decode_line(raw_line).strip()
        if not line.startswith(">"):
            continue

        fields = [field.strip() for field in line[1:].split("|")]
        if len(fields) < 4 or not fields[1]:
            malformed_headers += 1
            continue

        valid_headers += 1
        population = fields[1]
        if population not in seen:
            populations.append(population)
            seen.add(population)

    return FastaHeaderSummary(
        populations=populations,
        valid_headers=valid_headers,
        malformed_headers=malformed_headers,
    )

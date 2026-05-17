"""Load and extract DILS result tar.gz archives."""

from __future__ import annotations

import tarfile
import tempfile
from pathlib import Path
from typing import BinaryIO

# Repo root: streamlit/results/archive.py -> parents[2]
REPO_ROOT = Path(__file__).resolve().parents[2]
EXAMPLE_DIR = REPO_ROOT / "example"


class ArchiveError(Exception):
    """Raised when an archive cannot be loaded or parsed."""


def bundled_archive_path(label: str) -> Path | None:
    """Resolve a bundled example archive under example/."""
    from results.schema import BUNDLED_ARCHIVES

    name = BUNDLED_ARCHIVES.get(label)
    if not name:
        return None
    path = EXAMPLE_DIR / name
    return path if path.is_file() else None


def list_bundled_archives() -> dict[str, Path]:
    """Return label -> path for archives that exist on disk."""
    from results.schema import BUNDLED_ARCHIVES

    out = {}
    for label, name in BUNDLED_ARCHIVES.items():
        path = EXAMPLE_DIR / name
        if path.is_file():
            out[label] = path
    return out


def _expected_root_name(archive_path: Path) -> str:
    stem = archive_path.name
    for suffix in (".tar.gz", ".tgz", ".tar"):
        if stem.endswith(suffix):
            stem = stem[: -len(suffix)]
            break
    return stem


def extract_archive(
    archive_path: Path,
    file_obj: BinaryIO | None = None,
) -> tuple[Path, Path]:
    """
    Extract tar.gz to a temporary directory.

    Returns (extract_dir, root_dir) where root_dir is the inner timeStamp folder.
    """
    extract_dir = Path(tempfile.mkdtemp(prefix="dils_results_"))
    expected = _expected_root_name(archive_path)

    def _ensure_within_extract_dir(path: Path, label: str) -> None:
        try:
            path.resolve().relative_to(extract_dir.resolve())
        except ValueError as exc:
            raise ArchiveError(f"Unsafe archive member path: {label}") from exc

    def _prepare_member(member: tarfile.TarInfo) -> tarfile.TarInfo | None:
        member_path = extract_dir / member.name
        _ensure_within_extract_dir(member_path, member.name)

        if member.issym() or member.islnk():
            target = Path(member.linkname)
            link_target = target if target.is_absolute() else member_path.parent / target
            _ensure_within_extract_dir(link_target, f"{member.name} -> {member.linkname}")

        # Avoid ownership errors on some systems (Python 3.12+ filter API)
        member.uid = member.gid = 0
        member.uname = member.gname = ""
        return member

    if file_obj is not None:
        file_obj.seek(0)
        tf = tarfile.open(fileobj=file_obj, mode="r:gz")
    else:
        tf = tarfile.open(archive_path, mode="r:gz")

    with tf:
        def _filter(member: tarfile.TarInfo, _path: str = "") -> tarfile.TarInfo | None:
            return _prepare_member(member)

        try:
            tf.extractall(extract_dir, filter=_filter)
        except TypeError:
            # Older Python: filter receives only member
            def _filter_legacy(member: tarfile.TarInfo) -> tarfile.TarInfo | None:
                return _prepare_member(member)

            tf.extractall(extract_dir, filter=_filter_legacy)

    root = extract_dir / expected
    if root.is_dir():
        return extract_dir, root

    # Fallback: single top-level directory
    children = [p for p in extract_dir.iterdir() if p.is_dir()]
    if len(children) == 1:
        return extract_dir, children[0]

    raise ArchiveError(
        f"Could not find results folder '{expected}' inside archive. "
        f"Top-level entries: {[p.name for p in extract_dir.iterdir()]}"
    )


def load_archive(
    *,
    upload_bytes: bytes | None = None,
    upload_name: str | None = None,
    bundled_path: Path | None = None,
) -> tuple[Path, Path, str]:
    """
    Load archive from upload or path.

    Returns (extract_dir, root_dir, display_name).
    """
    import io

    if upload_bytes is not None:
        name = upload_name or "uploaded.tar.gz"
        extract_dir, root = extract_archive(
            Path(name),
            file_obj=io.BytesIO(upload_bytes),
        )
        return extract_dir, root, name

    if bundled_path is not None:
        if not bundled_path.is_file():
            raise ArchiveError(f"Bundled archive not found: {bundled_path}")
        extract_dir, root = extract_archive(bundled_path)
        return extract_dir, root, bundled_path.name

    raise ArchiveError("No archive source provided.")

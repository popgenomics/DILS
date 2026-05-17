"""Local notification scaffolding for app-managed DILS runs."""

from __future__ import annotations

from datetime import datetime, timezone
from email.message import EmailMessage
from email.utils import formatdate, make_msgid
from pathlib import Path
from typing import Any
from uuid import uuid4


_EVENT_FIELDS = {
    "submitted": (
        "notification_submitted_written",
        "notification_submitted_written_at",
    ),
    "completed": (
        "notification_completed_written",
        "notification_completed_written_at",
    ),
    "failed": (
        "notification_failed_written",
        "notification_failed_written_at",
    ),
}


def _now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def _filename_timestamp() -> str:
    return datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S%fZ")


def _clean_recipient(metadata: dict[str, Any]) -> str:
    return str(metadata.get("mail_address") or "").strip()


def _run_id(metadata: dict[str, Any], metadata_path: Path) -> str:
    return str(metadata.get("run_id") or metadata_path.parent.parent.name)


def _selected_populations(metadata: dict[str, Any]) -> str:
    names = []
    if metadata.get("nameA"):
        names.append(str(metadata["nameA"]))
    if metadata.get("nameB"):
        names.append(str(metadata["nameB"]))
    if metadata.get("nameOutgroup") and str(metadata["nameOutgroup"]) != "NA":
        names.append(f"outgroup: {metadata['nameOutgroup']}")
    return ", ".join(names) if names else "not recorded"


def _footer(settings: Any) -> str:
    parts = [
        str(settings.support_contact_text).strip(),
        "Wishing you all the best for your projects,",
        str(settings.notification_signature).strip(),
    ]
    return "\n\n".join(part for part in parts if part)


def _submitted_subject(run_id: str) -> str:
    return f"DILS analysis {run_id} submitted"


def _completed_subject(run_id: str) -> str:
    return f"DILS analysis {run_id} completed"


def _failed_subject(run_id: str, status: str) -> str:
    if status == "completed_missing_archive":
        return f"DILS analysis {run_id} completed but archive is missing"
    return f"DILS analysis {run_id} failed"


def _submitted_body(settings: Any, metadata: dict[str, Any], run_id: str) -> str:
    return (
        "Dear user,\n\n"
        f"Your DILS analysis {run_id} has been submitted.\n\n"
        f"run_id: {run_id}\n"
        f"submitted_at: {metadata.get('submitted_at') or 'not recorded'}\n"
        f"Slurm job id: {metadata.get('slurm_job_id') or 'not recorded'}\n"
        f"analysis type: {metadata.get('analysis_type') or 'not recorded'}\n"
        f"selected populations: {_selected_populations(metadata)}\n\n"
        f"{_footer(settings)}\n"
    )


def _completed_body(settings: Any, metadata: dict[str, Any], run_id: str) -> str:
    lines = [
        "Dear user,",
        "",
        f"Your DILS analysis {run_id} has completed successfully.",
        "",
        f"completed_at: {metadata.get('status_checked_at') or 'not recorded'}",
        "",
        "The final results archive has been produced.",
    ]
    if bool(getattr(settings, "local_dev_mode", False)) and metadata.get("final_archive_path"):
        lines.extend(["", f"final archive path: {metadata['final_archive_path']}"])
    if str(getattr(settings, "app_public_url", "")).strip():
        lines.extend(
            [
                "",
                "The results can also be accessed through the DILS web interface:",
                str(settings.app_public_url).strip(),
            ]
        )
    if str(getattr(settings, "manual_url", "")).strip():
        lines.extend(["", "Manual:", str(settings.manual_url).strip()])
    lines.extend(["", _footer(settings), ""])
    return "\n".join(lines)


def _failed_body(settings: Any, metadata: dict[str, Any], run_id: str) -> str:
    status = str(metadata.get("status") or "unknown")
    lines = [
        "Dear user,",
        "",
        f"Your DILS analysis {run_id} did not complete successfully.",
        "",
        f"run_id: {run_id}",
        f"status: {status}",
    ]
    if metadata.get("slurm_state"):
        lines.append(f"Slurm state: {metadata['slurm_state']}")
    if metadata.get("slurm_exit_code"):
        lines.append(f"Slurm exit code: {metadata['slurm_exit_code']}")
    if bool(getattr(settings, "local_dev_mode", False)):
        if metadata.get("stderr_log_resolved") or metadata.get("stderr_log"):
            lines.append(
                f"Slurm stderr log: {metadata.get('stderr_log_resolved') or metadata.get('stderr_log')}"
            )
        if metadata.get("submission_error"):
            lines.extend(["", "Submission error:", str(metadata["submission_error"])])
    if status == "completed_missing_archive":
        lines.extend(
            [
                "",
                "Slurm reported successful completion, but the expected results archive was not found.",
            ]
        )
    lines.extend(["", _footer(settings), ""])
    return "\n".join(lines)


def _message_for_event(
    settings: Any,
    metadata: dict[str, Any],
    metadata_path: Path,
    event: str,
) -> EmailMessage:
    run_id = _run_id(metadata, metadata_path)
    status = str(metadata.get("status") or "")
    if event == "submitted":
        subject = _submitted_subject(run_id)
        body = _submitted_body(settings, metadata, run_id)
    elif event == "completed":
        subject = _completed_subject(run_id)
        body = _completed_body(settings, metadata, run_id)
    else:
        subject = _failed_subject(run_id, status)
        body = _failed_body(settings, metadata, run_id)

    msg = EmailMessage()
    msg["From"] = str(settings.notification_from_email)
    msg["To"] = _clean_recipient(metadata)
    msg["Subject"] = subject
    msg["Date"] = formatdate(localtime=False)
    msg["Message-ID"] = make_msgid(idstring=run_id)
    msg.set_content(body)
    return msg


def _write_eml(metadata_path: Path, event: str, message: EmailMessage) -> Path:
    notification_dir = metadata_path.parent / "notifications"
    notification_dir.mkdir(parents=True, exist_ok=True)
    stem = f"{event}_{_filename_timestamp()}_{uuid4().hex[:8]}"
    tmp_path = notification_dir / f"{stem}.tmp"
    eml_path = notification_dir / f"{stem}.eml"
    tmp_path.write_bytes(message.as_bytes())
    tmp_path.replace(eml_path)
    return eml_path


def write_lifecycle_notification(
    settings: Any,
    metadata_path: Path,
    metadata: dict[str, Any],
    event: str,
) -> dict[str, Any]:
    """
    Write a local .eml notification for a lifecycle event.

    Returns metadata updates. This function never raises for delivery errors.
    """
    if not bool(getattr(settings, "notifications_enabled", False)):
        return {}
    if event not in _EVENT_FIELDS:
        return {"notification_last_error": f"Unsupported notification event: {event}"}

    written_field, written_at_field = _EVENT_FIELDS[event]
    if bool(metadata.get(written_field)):
        return {}

    recipient = _clean_recipient(metadata)
    if not recipient:
        return {"notification_skipped_reason": "missing recipient email"}

    backend = str(getattr(settings, "notification_backend", "file")).strip().lower()
    if backend != "file":
        return {
            "notification_last_error": (
                f"Unsupported notification backend for this build: {backend!r}"
            )
        }

    try:
        message = _message_for_event(settings, metadata, metadata_path, event)
        _write_eml(metadata_path, event, message)
    except Exception as exc:
        return {"notification_last_error": str(exc)}

    return {
        written_field: True,
        written_at_field: _now_iso(),
        "notification_last_error": None,
        "notification_skipped_reason": None,
    }

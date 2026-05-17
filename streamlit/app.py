"""
DILS Streamlit results viewer (v0).

Run from repository root:
    python -m streamlit run streamlit/app.py
"""

from __future__ import annotations

import sys
from pathlib import Path

_STREAMLIT_DIR = Path(__file__).resolve().parent
if str(_STREAMLIT_DIR) not in sys.path:
    sys.path.insert(0, str(_STREAMLIT_DIR))

import streamlit as st

from results.archive import list_bundled_archives, load_archive
from results.parsers import parse_archive
from settings import load_settings
from views import (
    config_builder,
    goodness_of_fit,
    help as help_view,
    model_comparison,
    new_run_preview,
    observed_stats,
    overview,
    parameters,
)

st.set_page_config(
    page_title="DILS results",
    layout="wide",
)

try:
    settings = load_settings()
except Exception as exc:
    st.error(f"Could not load app settings: {exc}")
    st.stop()

if settings.local_dev_mode:
    app_modes = [
        "Submit DILS analysis",
        "Results viewer",
        "Help",
        "Existing runs (dev/admin)",
        "YAML builder (dev)",
    ]
else:
    app_modes = ["Submit DILS analysis", "Results viewer", "Help"]

with st.sidebar:
    st.header("Mode")
    app_mode = st.radio(
        "App mode",
        app_modes,
        label_visibility="collapsed",
    )

st.markdown(
    """
    <style>
    .dils-mode-caption {
        font-size: 1.75rem;
        line-height: 1.35;
        color: rgba(49, 51, 63, 0.78);
        margin: -0.5rem 0 1.25rem 0;
        font-weight: 400;
    }
    [data-testid="stWidgetLabel"] p,
    [data-testid="stExpander"] summary p {
        font-size: 1.05rem;
        line-height: 1.35;
    }
    .dils-accent-action + div button {
        border-color: #FFBF66;
        box-shadow: inset 0 0 0 1px #FFBF66;
    }
    .dils-accent-action + div button:hover {
        border-color: #E5A84F;
        box-shadow: inset 0 0 0 1px #E5A84F;
    }
    </style>
    """,
    unsafe_allow_html=True,
)
st.title("DILS")
st.markdown(f'<div class="dils-mode-caption">{app_mode}</div>', unsafe_allow_html=True)

if app_mode == "Submit DILS analysis":
    new_run_preview.render()
    st.stop()

if app_mode == "Help":
    help_view.render()
    st.stop()

if app_mode == "Existing runs (dev/admin)":
    new_run_preview.render_existing_runs_page()
    st.stop()

if app_mode == "YAML builder (dev)":
    config_builder.render()
    st.stop()


@st.cache_data(show_spinner="Loading DILS archive…")
def _load_parsed(
    source_key: str,
    upload_bytes: bytes | None,
    upload_name: str | None,
    bundled_str: str | None,
) -> dict:
    """Cache parsed results; source_key distinguishes upload vs bundled."""
    bundled_path = Path(bundled_str) if bundled_str else None
    extract_dir, root, display_name = load_archive(
        upload_bytes=upload_bytes,
        upload_name=upload_name,
        bundled_path=bundled_path,
    )
    parsed = parse_archive(root, display_name)
    return {
        "parsed": parsed,
        "extract_dir": str(extract_dir),
        "root": str(root),
    }


def _get_parsed():
    if "parsed_bundle" not in st.session_state:
        return None
    return st.session_state.parsed_bundle["parsed"]


def _clear_load_warning() -> None:
    st.session_state.pop("load_warning", None)


def _is_unsupported_species_error(exc: BaseException) -> bool:
    return isinstance(exc, ValueError) and "supports 1- or 2-population" in str(exc)


def _set_load_warning(attempted_name: str, exc: BaseException) -> None:
    """Record a failed load while keeping the previous valid bundle."""
    if _is_unsupported_species_error(exc):
        message = "Unsupported archive. Previous valid result is still displayed."
    else:
        message = (
            "Archive could not be loaded. Previous valid result is still displayed."
        )
    st.session_state.load_warning = {
        "attempted": attempted_name,
        "message": message,
        "detail": str(exc),
    }


def _apply_loaded_bundle(bundle: dict, success_label: str) -> None:
    st.session_state.parsed_bundle = bundle
    _clear_load_warning()
    st.success(success_label)


def _handle_load_failure(attempted_name: str, exc: BaseException) -> None:
    if _get_parsed() is not None:
        _set_load_warning(attempted_name, exc)
    else:
        _clear_load_warning()
        st.error(str(exc))


def _render_load_warning() -> None:
    warn = st.session_state.get("load_warning")
    if not warn:
        return
    st.warning(warn["message"])
    st.caption(
        f"Failed load: `{warn['attempted']}` — {warn['detail']}"
    )


# --- Sidebar: data source ---
with st.sidebar:
    with st.container(border=True):
        st.header("Data source")
        bundled = list_bundled_archives()
        source = st.radio(
            "Load from",
            ["Bundled example", "Upload .tar.gz"],
            index=0,
        )

        parsed_holder = None

        if source == "Bundled example":
            if not bundled:
                st.error("No bundled archives found under example/.")
            else:
                labels = list(bundled.keys())
                choice = st.selectbox("Example archive", labels, index=0)
                path = bundled[choice]
                if st.button("Load example", width="stretch"):
                    try:
                        bundle = _load_parsed(
                            f"bundled:{path}",
                            None,
                            None,
                            str(path),
                        )
                        _apply_loaded_bundle(bundle, f"Loaded {path.name}")
                    except Exception as e:
                        _handle_load_failure(path.name, e)
        else:
            uploaded = st.file_uploader(
                "DILS results archive",
                type=["gz", "tar.gz"],
                help="Upload the .tar.gz produced by DILS (not extracted).",
            )
            if uploaded is not None and st.button(
                "Load upload", type="primary", width="stretch"
            ):
                try:
                    bundle = _load_parsed(
                        f"upload:{uploaded.name}:{uploaded.size}",
                        uploaded.getvalue(),
                        uploaded.name,
                        None,
                    )
                    _apply_loaded_bundle(bundle, f"Loaded {uploaded.name}")
                except Exception as e:
                    _handle_load_failure(uploaded.name, e)

    if _get_parsed() is not None:
        p = _get_parsed()
        st.divider()
        st.markdown(f"**Active:** `{p.display_name}`")
        if p.is_one_pop:
            st.markdown(f"**Population:** {p.name_a}")
        else:
            st.markdown(f"**Populations:** {p.name_a} / {p.name_b}")

    _render_load_warning()

data = _get_parsed()

if data is None:
    st.info(
        "Select a bundled example or upload a `.tar.gz` archive, then click **Load**."
    )
    st.markdown(
        "Supported: completed 1- or 2-population DILS light-mode results "
        "(e.g. `example/project1pop.tar.gz` or "
        "`example/2pop_light_noOut_noSFS_beta_constant_coding.tar.gz`)."
    )
    st.stop()

_render_load_warning()

# --- Main tabs ---
tab_overview, tab_models, tab_obs, tab_params, tab_gof = st.tabs(
    [
        "Overview",
        "Model comparison",
        "Observed statistics",
        "Parameters",
        "Goodness of fit",
    ]
)

with tab_overview:
    overview.render(data)

with tab_models:
    model_comparison.render(data)

with tab_obs:
    observed_stats.render(data)

with tab_params:
    parameters.render(data)

with tab_gof:
    goodness_of_fit.render(data)

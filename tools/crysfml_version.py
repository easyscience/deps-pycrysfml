from __future__ import annotations

from collections.abc import Mapping
from pathlib import Path
from typing import Any

import versioningit

__all__ = ["dynamic_metadata"]


def __dir__() -> list[str]:
    return __all__


def dynamic_metadata(
    field: str,
    settings: dict[str, object] | None = None,
    project: Mapping[str, Any] | None = None,
) -> str:
    if field != "version":
        raise ValueError("Only the 'version' field is supported")

    if settings:
        raise ValueError("No inline configuration is supported")

    return versioningit.get_version(project_dir=Path(__file__).resolve().parent.parent)
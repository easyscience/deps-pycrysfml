from __future__ import annotations

import os
import sys


def build_subprocess_environment() -> dict[str, str]:
    environment = os.environ.copy()
    if sys.platform == 'win32':
        environment.setdefault('CMAKE_GENERATOR', 'Ninja')
    return environment
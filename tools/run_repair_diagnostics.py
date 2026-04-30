from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_WHEEL_DIR = REPO_ROOT / 'dist' / 'pyCFML' / 'wheel'
FILENAME_VALIDATOR = REPO_ROOT / 'tools' / 'validate_pypi_wheel_filenames.py'
WHEEL_TEST_RUNNER = REPO_ROOT / 'tools' / 'run_installed_wheel_tests.py'

REPAIR_HELPERS = {
    'darwin': REPO_ROOT / 'tools' / 'repair_macos_wheel.py',
    'win32': REPO_ROOT / 'tools' / 'repair_windows_wheel.py',
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description='Repair a locally built wheel on the native host, validate its filename, and run installed-wheel tests.',
    )
    parser.add_argument(
        '--wheel-dir',
        type=Path,
        default=DEFAULT_WHEEL_DIR,
        help='Directory containing the built crysfml wheel to diagnose',
    )
    return parser.parse_args()


def run(command: list[str], description: str) -> None:
    print(f':::::: {description}')
    subprocess.run(command, check=True)


def require_supported_platform() -> Path:
    repair_helper = REPAIR_HELPERS.get(sys.platform)
    if repair_helper is None:
        raise SystemExit(
            'Local native repair diagnostics are only supported on macOS and Windows. '
            'Linux repaired-wheel validation remains owned by the manylinux cibuildwheel path.'
        )
    return repair_helper


def main() -> None:
    args = parse_args()
    repair_helper = require_supported_platform()
    run(
        [sys.executable, str(repair_helper), '--wheel-dir', str(args.wheel_dir)],
        f'Running native wheel repair diagnostics via {repair_helper.name}',
    )
    run(
        [sys.executable, str(FILENAME_VALIDATOR), str(args.wheel_dir)],
        'Validating repaired wheel filename',
    )
    run(
        [sys.executable, str(WHEEL_TEST_RUNNER), '--wheel-dir', str(args.wheel_dir)],
        'Running installed-wheel tests against repaired wheel',
    )


if __name__ == '__main__':
    main()

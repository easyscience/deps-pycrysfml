from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_WHEEL_DIR = REPO_ROOT / 'dist' / 'pyCFML' / 'wheel'
UNIT_TEST_DIR = REPO_ROOT / 'tests' / 'unit_tests' / 'pyCFML'
FUNCTIONAL_TEST_DIR = REPO_ROOT / 'tests' / 'functional_tests' / 'pyCFML'


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description='Install a built crysfml wheel and run the selected test mode',
    )
    parser.add_argument(
        '--wheel-dir',
        type=Path,
        default=DEFAULT_WHEEL_DIR,
        help='Directory containing a built crysfml wheel',
    )
    return parser.parse_args()


def find_wheel(wheel_dir: Path) -> Path:
    wheels = sorted(wheel_dir.glob('crysfml-*.whl'))
    if not wheels:
        raise SystemExit(f'No crysfml wheel found in {wheel_dir}')
    return wheels[0]


def run(command: list[str], description: str) -> None:
    print(f':::::: {description}')
    subprocess.run(command, check=True)


def install_wheel(wheel_path: Path) -> None:
    run(
        [sys.executable, '-m', 'pip', 'install', '--force-reinstall', str(wheel_path)],
        f'Installing wheel {wheel_path.name}',
    )


def run_unit_tests() -> None:
    run(
        [sys.executable, '-m', 'pytest', str(UNIT_TEST_DIR), '--color=yes', '--benchmark-disable'],
        f'Running unit tests from {UNIT_TEST_DIR.relative_to(REPO_ROOT)}',
    )


def run_functional_tests() -> None:
    run(
        [sys.executable, '-m', 'pytest', str(FUNCTIONAL_TEST_DIR), '--color=yes', '--benchmark-disable'],
        f'Running functional tests from {FUNCTIONAL_TEST_DIR.relative_to(REPO_ROOT)}',
    )


def main() -> None:
    args = parse_args()
    wheel_path = find_wheel(args.wheel_dir)
    install_wheel(wheel_path)
    run_unit_tests()
    run_functional_tests()


if __name__ == '__main__':
    main()
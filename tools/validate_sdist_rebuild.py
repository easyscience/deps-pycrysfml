from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_ARTIFACT_ROOT = REPO_ROOT / 'dist' / 'sdist-validation'
DEFAULT_SDIST_DIR = DEFAULT_ARTIFACT_ROOT / 'sdist'
DEFAULT_WHEEL_DIR = DEFAULT_ARTIFACT_ROOT / 'wheel'
WHEEL_TEST_SCRIPT = REPO_ROOT / 'tools' / 'run_installed_wheel_tests.py'


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description='Build an sdist, rebuild a wheel from it, and optionally test the rebuilt wheel.',
    )
    parser.add_argument(
        '--sdist-dir',
        type=Path,
        default=DEFAULT_SDIST_DIR,
        help='Directory where the built sdist should be written',
    )
    parser.add_argument(
        '--wheel-dir',
        type=Path,
        default=DEFAULT_WHEEL_DIR,
        help='Directory where the rebuilt wheel should be written',
    )
    parser.add_argument(
        '--skip-tests',
        action='store_true',
        help='Build the sdist and rebuilt wheel without running installed-wheel tests',
    )
    return parser.parse_args()


def run(command: list[str], description: str) -> None:
    print(f':::::: {description}')
    subprocess.run(command, check=True)


def recreate_dir(path: Path) -> None:
    shutil.rmtree(path, ignore_errors=True)
    path.mkdir(parents=True, exist_ok=True)


def find_single_path(directory: Path, pattern: str) -> Path:
    matches = sorted(directory.glob(pattern))
    if not matches:
        raise SystemExit(f'No files matching {pattern!r} found in {directory}')
    if len(matches) > 1:
        raise SystemExit(f'Expected exactly one file matching {pattern!r} in {directory}, found {len(matches)}')
    return matches[0]


def build_sdist(sdist_dir: Path) -> Path:
    recreate_dir(sdist_dir)
    run(
        [sys.executable, '-m', 'build', '--sdist', '--outdir', str(sdist_dir)],
        f'Building sdist into {sdist_dir}',
    )
    return find_single_path(sdist_dir, 'crysfml-*.tar.gz')


def build_wheel_from_sdist(sdist_path: Path, wheel_dir: Path) -> Path:
    recreate_dir(wheel_dir)
    run(
        [sys.executable, '-m', 'pip', 'wheel', '--no-deps', '--wheel-dir', str(wheel_dir), str(sdist_path)],
        f'Rebuilding wheel from {sdist_path.name}',
    )
    return find_single_path(wheel_dir, 'crysfml-*.whl')


def test_rebuilt_wheel(wheel_dir: Path) -> None:
    run(
        [sys.executable, str(WHEEL_TEST_SCRIPT), '--wheel-dir', str(wheel_dir)],
        f'Testing rebuilt wheel from {wheel_dir}',
    )


def main() -> None:
    args = parse_args()
    sdist_path = build_sdist(args.sdist_dir)
    wheel_path = build_wheel_from_sdist(sdist_path, args.wheel_dir)
    print(f'Built sdist: {sdist_path}')
    print(f'Rebuilt wheel: {wheel_path}')
    if not args.skip_tests:
        test_rebuilt_wheel(args.wheel_dir)


if __name__ == '__main__':
    main()

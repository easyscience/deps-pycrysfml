from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_WHEEL_DIR = REPO_ROOT / 'dist' / 'pyCFML' / 'wheel'


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description='Build a local crysfml wheel from the repository root into a clean output directory.',
    )
    parser.add_argument(
        '--wheel-dir',
        type=Path,
        default=DEFAULT_WHEEL_DIR,
        help='Directory where the built wheel should be written',
    )
    return parser.parse_args()


def run(command: list[str], description: str) -> None:
    print(f':::::: {description}')
    subprocess.run(command, check=True)


def recreate_dir(path: Path) -> None:
    shutil.rmtree(path, ignore_errors=True)
    path.mkdir(parents=True, exist_ok=True)


def find_single_wheel(directory: Path) -> Path:
    wheels = sorted(directory.glob('crysfml-*.whl'))
    if not wheels:
        raise SystemExit(f'No crysfml wheel found in {directory}')
    if len(wheels) > 1:
        raise SystemExit(f'Expected exactly one crysfml wheel in {directory}, found {len(wheels)}')
    return wheels[0]


def main() -> None:
    args = parse_args()
    recreate_dir(args.wheel_dir)
    run(
        [sys.executable, '-m', 'build', '--wheel', '--no-isolation', '--outdir', str(args.wheel_dir)],
        f'Building wheel into {args.wheel_dir}',
    )
    wheel_path = find_single_wheel(args.wheel_dir)
    print(f'Built wheel: {wheel_path}')


if __name__ == '__main__':
    main()

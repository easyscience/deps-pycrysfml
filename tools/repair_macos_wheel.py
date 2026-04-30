from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_WHEEL_DIR = REPO_ROOT / 'dist' / 'pyCFML' / 'wheel'


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description='Repair a built macOS crysfml wheel with delocate and replace the raw wheel artifact.',
    )
    parser.add_argument(
        '--wheel-dir',
        type=Path,
        default=DEFAULT_WHEEL_DIR,
        help='Directory containing the built crysfml wheel to repair',
    )
    return parser.parse_args()


def run(command: list[str], description: str) -> None:
    print(f':::::: {description}')
    subprocess.run(command, check=True)


def find_single_wheel(wheel_dir: Path) -> Path:
    wheels = sorted(wheel_dir.glob('crysfml-*.whl'))
    if not wheels:
        raise SystemExit(f'No crysfml wheel found in {wheel_dir}')
    if len(wheels) > 1:
        raise SystemExit(f'Expected exactly one crysfml wheel in {wheel_dir}, found {len(wheels)}')
    return wheels[0]


def show_dependencies(wheel_path: Path, description: str) -> None:
    run(
        [sys.executable, '-m', 'delocate.cmd.delocate_listdeps', '--all', str(wheel_path)],
        description,
    )


def repair_wheel(raw_wheel_path: Path) -> Path:
    with tempfile.TemporaryDirectory(prefix='crysfml-delocate-') as temp_dir:
        repaired_dir = Path(temp_dir)
        run(
            [
                sys.executable,
                '-m',
                'delocate.cmd.delocate_wheel',
                '-w',
                str(repaired_dir),
                '-v',
                str(raw_wheel_path),
            ],
            f'Repairing wheel {raw_wheel_path.name} with delocate',
        )
        repaired_wheel_path = find_single_wheel(repaired_dir)
        for wheel_path in raw_wheel_path.parent.glob('crysfml-*.whl'):
            wheel_path.unlink()
        final_wheel_path = raw_wheel_path.parent / repaired_wheel_path.name
        shutil.copy2(repaired_wheel_path, final_wheel_path)
        return final_wheel_path


def main() -> None:
    if sys.platform != 'darwin':
        raise SystemExit('tools/repair_macos_wheel.py must be run on macOS')

    args = parse_args()
    raw_wheel_path = find_single_wheel(args.wheel_dir)
    show_dependencies(raw_wheel_path, f'Inspecting raw wheel dependencies for {raw_wheel_path.name}')
    repaired_wheel_path = repair_wheel(raw_wheel_path)
    show_dependencies(
        repaired_wheel_path,
        f'Inspecting repaired wheel dependencies for {repaired_wheel_path.name}',
    )
    print(f'Repaired wheel: {repaired_wheel_path}')


if __name__ == '__main__':
    main()

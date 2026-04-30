from __future__ import annotations

import argparse
import os
import platform
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
    parser.add_argument(
        '--compiler',
        default='gfortran',
        help='Compiler label used for benchmark storage layout',
    )
    parser.add_argument(
        '--benchmark-mode',
        choices=['off', 'compare', 'save'],
        default='off',
        help='Benchmark behavior for functional tests',
    )
    return parser.parse_args()


def github_runner_name() -> str:
    return 'github' if 'GITHUB_ACTIONS' in os.environ else 'local'


def processor_name() -> str:
    if sys.platform == 'win32':
        return platform.machine()
    processor = platform.processor()
    return processor.split()[0]


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


def benchmark_storage_dir(compiler: str) -> str:
    return (
        f'file://./.benchmarks/pyCFML/'
        f'{github_runner_name()}/{compiler}/{processor_name()}'
    )


def run_functional_tests(benchmark_mode: str, compiler: str) -> None:
    command = [sys.executable, '-m', 'pytest', str(FUNCTIONAL_TEST_DIR), '--color=yes']

    if benchmark_mode == 'off':
        command.append('--benchmark-disable')
        description = f'Running functional tests from {FUNCTIONAL_TEST_DIR.relative_to(REPO_ROOT)}'
    else:
        command.extend(
            [
                '--benchmark-only',
                f'--benchmark-storage={benchmark_storage_dir(compiler)}',
                '--benchmark-warmup=on',
                '--benchmark-columns=median, iqr, ops',
            ]
        )
        if benchmark_mode == 'compare':
            command.extend(['--benchmark-compare', '--benchmark-compare-fail=median:50%'])
            description = (
                'Running functional benchmarks and comparing with saved results '
                f'from {FUNCTIONAL_TEST_DIR.relative_to(REPO_ROOT)}'
            )
        else:
            command.append('--benchmark-autosave')
            description = (
                'Running functional benchmarks and saving results '
                f'from {FUNCTIONAL_TEST_DIR.relative_to(REPO_ROOT)}'
            )

    run(command, description)


def main() -> None:
    args = parse_args()
    if args.benchmark_mode == 'off':
        wheel_path = find_wheel(args.wheel_dir)
        install_wheel(wheel_path)
        run_unit_tests()

    run_functional_tests(args.benchmark_mode, args.compiler)


if __name__ == '__main__':
    main()
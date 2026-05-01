from __future__ import annotations

import argparse
import shutil
import subprocess
from datetime import date
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent
VENDOR_ROOT = REPO_ROOT / 'repo' / 'CFML'
VENDOR_SRC_ROOT = VENDOR_ROOT / 'Src'
DEFAULT_BUILD_DIR = REPO_ROOT / 'build' / 'vendor-cfml-cmake'
DEFAULT_BUILD_TYPE = 'Debug'
UPSTREAM_URL = 'https://code.ill.fr/scientific-software/crysfml2008.git'
DEFAULT_BRANCH = 'master'


def run(command: list[str], description: str, cwd: Path | None = None) -> None:
    print(f':::::: {description}')
    subprocess.run(command, cwd=cwd, check=True)


def capture(command: list[str], description: str, cwd: Path | None = None) -> str:
    print(f':::::: {description}')
    result = subprocess.run(
        command,
        cwd=cwd,
        check=True,
        text=True,
        capture_output=True,
    )
    return result.stdout.strip()


def ensure_vendored_sources_exist() -> None:
    if VENDOR_SRC_ROOT.is_dir():
        return
    raise SystemExit(
        'Vendored CFML sources are missing at repo/CFML/Src. '
        'Restore the tracked vendored copy or run vendor_cfml.py refresh --force.'
    )


def write_vendored_metadata(branch: str) -> None:
    commit = capture(['git', 'rev-parse', 'HEAD'], 'Recording vendored CFML commit', cwd=VENDOR_ROOT)
    metadata_path = VENDOR_ROOT / 'VENDORED_FROM.txt'
    metadata_path.write_text(
        '\n'.join(
            [
                f'Upstream repository: {UPSTREAM_URL}',
                f'Vendored branch: {branch}',
                f'Vendored commit: {commit}',
                f'Vendored on: {date.today():%Y-%m-%d}',
                '',
                'This directory is a tracked vendored copy of the upstream CrysFML sources.',
                'It is intentionally kept as normal repository content, not as a Git submodule.',
                '',
            ]
        ),
        encoding='utf-8',
    )
    shutil.rmtree(VENDOR_ROOT / '.git', ignore_errors=True)


def refresh_sources(force: bool, branch: str, commit: str | None) -> None:
    if not force:
        ensure_vendored_sources_exist()
        print(':::::: Reusing existing local CFML sources in repo/CFML')
        return

    print(':::::: Refreshing vendored CFML sources in repo/CFML')
    shutil.rmtree(VENDOR_ROOT, ignore_errors=True)
    VENDOR_ROOT.parent.mkdir(parents=True, exist_ok=True)

    clone_command = [
        'git',
        '-c',
        'http.lowSpeedLimit=1000',
        '-c',
        'http.lowSpeedTime=30',
        'clone',
        '--filter=blob:none',
        '--depth',
        '1',
        '--single-branch',
        '--branch',
        branch,
        UPSTREAM_URL,
        str(VENDOR_ROOT),
    ]

    last_error: subprocess.CalledProcessError | None = None
    for attempt in range(1, 4):
        try:
            run(clone_command, f'Clone attempt {attempt}/3 for branch {branch}')
            if commit:
                run(
                    ['git', 'fetch', '--depth', '1', '--filter=blob:none', 'origin', commit],
                    f'Fetching commit {commit}',
                    cwd=VENDOR_ROOT,
                )
                run(['git', 'checkout', '--detach', 'FETCH_HEAD'], f'Checking out commit {commit}', cwd=VENDOR_ROOT)
            write_vendored_metadata(branch)
            return
        except subprocess.CalledProcessError as error:
            last_error = error
            print(f':::::: Clone attempt {attempt} failed; cleaning up partial checkout')
            shutil.rmtree(VENDOR_ROOT, ignore_errors=True)

    raise SystemExit('Failed to refresh vendored CFML sources after 3 attempts') from last_error


def configure_build(build_dir: Path, build_type: str, enable_vendor_tests: bool) -> None:
    ensure_vendored_sources_exist()
    command = [
        'cmake',
        '-S',
        str(REPO_ROOT),
        '-B',
        str(build_dir),
        '-DCRYSFML_ENABLE_PYCFML_EXTENSION=OFF',
        '-DCRYSFML_ENABLE_PYTHON_PACKAGE=OFF',
        f'-DCRYSFML_ENABLE_VENDOR_TESTS={"ON" if enable_vendor_tests else "OFF"}',
        f'-DCMAKE_BUILD_TYPE={build_type}',
    ]
    run(command, f'Configuring vendored CFML CMake build in {build_dir}')


def build_target(build_dir: Path, target_name: str) -> None:
    run(
        ['cmake', '--build', str(build_dir), '--target', target_name, '--parallel'],
        f'Building CMake target {target_name}',
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description='Maintain vendored CFML sources and the CFML-only maintainer build/test path.',
    )
    subparsers = parser.add_subparsers(dest='command', required=True)

    refresh_parser = subparsers.add_parser('refresh', help='Refresh vendored CFML sources under repo/CFML')
    refresh_parser.add_argument('--force', action='store_true', help='Replace the existing vendored CFML tree')
    refresh_parser.add_argument('--branch', default=DEFAULT_BRANCH, help='Upstream branch to vendor')
    refresh_parser.add_argument('--commit', default=None, help='Specific upstream commit to vendor after cloning the branch tip')

    for command_name in ('build', 'test'):
        build_parser = subparsers.add_parser(command_name, help=f'Configure and build the vendored CFML {command_name} path')
        build_parser.add_argument('--build-dir', type=Path, default=DEFAULT_BUILD_DIR, help='Out-of-tree CMake build directory')
        build_parser.add_argument(
            '--build-type',
            choices=['Debug', 'Release', 'RelWithDebInfo', 'MinSizeRel'],
            default=DEFAULT_BUILD_TYPE,
            help='CMake build type for the vendored CFML maintainer path',
        )

    return parser.parse_args()


def main() -> None:
    args = parse_args()

    if args.command == 'refresh':
        refresh_sources(force=args.force, branch=args.branch, commit=args.commit)
        return

    enable_vendor_tests = args.command == 'test'
    configure_build(args.build_dir, args.build_type, enable_vendor_tests)
    build_target(
        args.build_dir,
        'cfml_vendor_test_programs' if enable_vendor_tests else 'cfml_vendor_distribution',
    )


if __name__ == '__main__':
    main()
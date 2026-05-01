from __future__ import annotations

import argparse
import json
import re
import sys
import tomllib
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_PIXI_FILE = REPO_ROOT / 'pixi.toml'
CI_ENVIRONMENT_RE = re.compile(r'^ci-py\d+$')
PYTHON_FEATURE_RE = re.compile(r'^py\d+$')
PYTHON_VERSION_RE = re.compile(r'^(?P<major>\d+)\.(?P<minor>\d+)')


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description='Export CI Python version metadata from pixi.toml.',
    )
    parser.add_argument(
        '--pixi-file',
        type=Path,
        default=DEFAULT_PIXI_FILE,
        help='Path to the pixi.toml file to inspect',
    )
    parser.add_argument(
        '--github-output',
        action='store_true',
        help='Emit key=value lines suitable for appending to GITHUB_OUTPUT',
    )
    return parser.parse_args()


def load_pixi_file(path: Path) -> dict:
    with path.open('rb') as stream:
        return tomllib.load(stream)


def parse_python_version(version_spec: str, source_name: str) -> tuple[tuple[int, int], str]:
    match = PYTHON_VERSION_RE.match(version_spec)
    if match is None:
        raise SystemExit(f"Expected a '<major>.<minor>' Python version in {source_name!r}, got {version_spec!r}")

    major = int(match.group('major'))
    minor = int(match.group('minor'))
    return (major, minor), f'{major}.{minor}'


def collect_ci_python_entries(pixi_data: dict) -> list[tuple[tuple[int, int], str, str]]:
    feature_table = pixi_data.get('feature', {})
    environments = pixi_data.get('environments', {})
    entries: list[tuple[tuple[int, int], str, str]] = []

    for environment_name, environment_features in environments.items():
        if not CI_ENVIRONMENT_RE.fullmatch(environment_name):
            continue

        python_features = [feature for feature in environment_features if PYTHON_FEATURE_RE.fullmatch(feature)]
        if len(python_features) != 1:
            raise SystemExit(
                f"Expected exactly one Python feature in environment {environment_name!r}, got {python_features!r}"
            )

        python_feature = python_features[0]
        python_dependencies = feature_table.get(python_feature, {}).get('dependencies', {})
        python_version_spec = python_dependencies.get('python')
        if python_version_spec is None:
            raise SystemExit(
                f"Expected feature {python_feature!r} used by {environment_name!r} to define a Python dependency"
            )

        version_key, version_string = parse_python_version(python_version_spec, python_feature)
        entries.append((version_key, version_string, environment_name))

    if not entries:
        raise SystemExit('No ci-py* environments found in pixi.toml')

    entries.sort(key=lambda entry: entry[0])
    return entries


def build_output(entries: list[tuple[tuple[int, int], str, str]]) -> dict[str, str | list[str]]:
    python_versions = [version_string for _, version_string, _ in entries]
    _, oldest_python_version, oldest_ci_environment = entries[0]
    _, latest_python_version, latest_ci_environment = entries[-1]
    return {
        'python_versions': python_versions,
        'oldest_python_version': oldest_python_version,
        'oldest_ci_environment': oldest_ci_environment,
        'latest_python_version': latest_python_version,
        'latest_ci_environment': latest_ci_environment,
    }


def emit_github_output(output_data: dict[str, str | list[str]]) -> None:
    for key, value in output_data.items():
        if isinstance(value, list):
            print(f'{key}={json.dumps(value)}')
        else:
            print(f'{key}={value}')


def main() -> int:
    args = parse_args()
    pixi_data = load_pixi_file(args.pixi_file)
    output_data = build_output(collect_ci_python_entries(pixi_data))

    if args.github_output:
        emit_github_output(output_data)
    else:
        json.dump(output_data, sys.stdout, indent=2)
        print()

    return 0


if __name__ == '__main__':
    raise SystemExit(main())
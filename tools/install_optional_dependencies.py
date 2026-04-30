from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path

import tomllib


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Install optional dependency groups from pyproject.toml",
    )
    parser.add_argument(
        "groups",
        nargs="+",
        help="Optional dependency groups to install",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Ask pip to resolve dependencies without installing them",
    )
    return parser.parse_args()


def load_optional_dependencies(pyproject_path: Path) -> dict[str, list[str]]:
    with pyproject_path.open("rb") as stream:
        pyproject = tomllib.load(stream)
    return pyproject["project"]["optional-dependencies"]


def collect_dependencies(
    optional_dependencies: dict[str, list[str]],
    groups: list[str],
) -> list[str]:
    dependencies: list[str] = []
    seen: set[str] = set()

    for group in groups:
        if group not in optional_dependencies:
            raise SystemExit(f"Unknown optional dependency group: {group}")

        for dependency in optional_dependencies[group]:
            if dependency not in seen:
                dependencies.append(dependency)
                seen.add(dependency)

    return dependencies


def main() -> None:
    args = parse_args()
    repo_root = Path(__file__).resolve().parent.parent
    optional_dependencies = load_optional_dependencies(repo_root / "pyproject.toml")
    dependencies = collect_dependencies(optional_dependencies, args.groups)

    command = [sys.executable, "-m", "pip", "install"]
    if args.dry_run:
        command.append("--dry-run")
    command.extend(dependencies)
    subprocess.run(command, check=True)


if __name__ == "__main__":
    main()
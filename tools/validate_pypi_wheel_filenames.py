from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

from packaging.utils import InvalidWheelFilename, parse_wheel_filename


ALLOWED_PLATFORMS = {
    'any',
    'win32',
    'win_arm64',
    'win_amd64',
    'win_ia64',
    'manylinux1_x86_64',
    'manylinux1_i686',
    'manylinux2010_x86_64',
    'manylinux2010_i686',
    'manylinux2014_x86_64',
    'manylinux2014_i686',
    'manylinux2014_aarch64',
    'manylinux2014_armv7l',
    'manylinux2014_ppc64',
    'manylinux2014_ppc64le',
    'manylinux2014_s390x',
    'linux_armv6l',
    'linux_armv7l',
}

MACOSX_PLATFORM_RE = re.compile(r'macosx_(?P<major>\d+)_(?P<minor>\d+)_(?P<arch>.*)')
MACOSX_ARCHES = {
    'ppc',
    'ppc64',
    'i386',
    'x86_64',
    'arm64',
    'intel',
    'fat',
    'fat3',
    'fat64',
    'universal',
    'universal2',
}
MACOSX_MAJOR_VERSIONS = {'11', '12', '13', '14', '15', '26'}

LINUX_PLATFORM_RE = re.compile(r'(?P<libc>(many|musl))linux_(\d+)_(\d+)_(?P<arch>.*)')
JOINT_LINUX_ARCHES = {
    'x86_64',
    'i686',
    'aarch64',
    'armv7l',
    'ppc64le',
    's390x',
    'riscv64',
}
MANYLINUX_ARCHES = JOINT_LINUX_ARCHES | {'ppc64'}
MUSLLINUX_ARCHES = JOINT_LINUX_ARCHES

IOS_PLATFORM_RE = re.compile(r'ios_(\d+)_(\d+)_(?P<arch>.*)_(iphoneos|iphonesimulator)')
IOS_ARCHES = {'arm64', 'x86_64'}

ANDROID_PLATFORM_RE = re.compile(r'android_(\d+)_(?P<arch>.*)')
ANDROID_ARCHES = {'armeabi_v7a', 'arm64_v8a', 'x86', 'x86_64'}

PYEMSCRIPTEN_PLATFORM_RE = re.compile(r'pyemscripten_(?P<major>\d+)_(?P<minor>\d+)_wasm32')


def valid_platform_tag(platform_tag: str) -> bool:
    if platform_tag in ALLOWED_PLATFORMS:
        return True

    match = MACOSX_PLATFORM_RE.fullmatch(platform_tag)
    if match and match.group('major') == '10' and match.group('arch') in MACOSX_ARCHES:
        return True
    if (
        match
        and match.group('major') in MACOSX_MAJOR_VERSIONS
        and match.group('minor') == '0'
        and match.group('arch') in MACOSX_ARCHES
    ):
        return True

    match = LINUX_PLATFORM_RE.fullmatch(platform_tag)
    if match and match.group('libc') == 'musl':
        return match.group('arch') in MUSLLINUX_ARCHES
    if match and match.group('libc') == 'many':
        return match.group('arch') in MANYLINUX_ARCHES

    match = IOS_PLATFORM_RE.fullmatch(platform_tag)
    if match and match.group('arch') in IOS_ARCHES:
        return True

    match = ANDROID_PLATFORM_RE.fullmatch(platform_tag)
    if match and match.group('arch') in ANDROID_ARCHES:
        return True

    return bool(PYEMSCRIPTEN_PLATFORM_RE.fullmatch(platform_tag))


def iter_wheel_paths(paths: list[str]) -> list[Path]:
    wheel_paths: list[Path] = []
    for raw_path in paths:
        path = Path(raw_path)
        if path.is_dir():
            wheel_paths.extend(sorted(path.glob('*.whl')))
            continue
        wheel_paths.append(path)
    return wheel_paths


def validate_wheel_filename(filename: str) -> None:
    try:
        _, _, _, tags = parse_wheel_filename(filename)
    except InvalidWheelFilename as exc:
        raise ValueError(str(exc)) from exc

    invalid_platforms = sorted({tag.platform for tag in tags if not valid_platform_tag(tag.platform)})
    if invalid_platforms:
        invalid_platforms_str = ', '.join(repr(platform_tag) for platform_tag in invalid_platforms)
        raise ValueError(
            f"Binary wheel '{filename}' has unsupported PyPI platform tag(s): {invalid_platforms_str}"
        )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description='Validate wheel filenames against PyPI platform-tag rules.')
    parser.add_argument('paths', nargs='+', help='Wheel files or directories containing wheel files')
    args = parser.parse_args(argv)

    wheel_paths = iter_wheel_paths(args.paths)
    if not wheel_paths:
        print('ERROR: No wheels found to validate', file=sys.stderr)
        return 1

    failed = False
    for wheel_path in wheel_paths:
        filename = wheel_path.name
        try:
            validate_wheel_filename(filename)
        except ValueError as exc:
            print(f'ERROR: {exc}', file=sys.stderr)
            failed = True
            continue
        print(f'Validated wheel filename: {filename}')

    return 1 if failed else 0


if __name__ == '__main__':
    raise SystemExit(main())
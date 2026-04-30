# Colab And Linux Wheel Compatibility

Date: 2026-04-29

## Summary

`crysfml` is present on PyPI, but Google Colab can still fail with:

```text
ERROR: Could not find a version that satisfies the requirement crysfml (from versions: none)
ERROR: No matching distribution found for crysfml
```

In the current release flow, that message most likely means:

- PyPI has no wheel compatible with the Colab runtime tags.
- There is no source distribution (`sdist`) fallback.

## Current Published State

As checked on PyPI on 2026-04-29:

- Latest release exists on PyPI.
- Linux wheels are published for CPython 3.11 to 3.14.
- Published Linux wheels currently use the tag `manylinux_2_39_x86_64`.
- No source distribution is published.

This is a problem for Colab because standard Colab environments are commonly:

- `x86_64`
- `cp312`
- glibc older than 2.39

If Colab reports a compatible tag like `cp312-cp312-manylinux_2_35_x86_64`, then a wheel tagged `manylinux_2_39_x86_64` will not be selected by `pip`.

## Observed Colab Runtime

Observed on Google Colab on 2026-04-29:

```text
Python: 3.12.13
Machine: x86_64
glibc: 2.35
First supported tag: cp312-cp312-manylinux_2_35_x86_64
Platform: Linux-6.6.113+-x86_64-with-glibc2.35
```

This confirms that the current published Linux wheels tagged `manylinux_2_39_x86_64` are too new for the checked Colab runtime.

Implications:

- `cp312` is the most important Linux target for Colab.
- Any Linux wheel built specifically for this observed Colab runtime must be tagged no newer than `manylinux_2_35_x86_64`.
- A more portable and safer release target is still `manylinux2014_x86_64` / `manylinux_2_17_x86_64`.

## Current Repository Behavior

The current Linux release pipeline is not a real manylinux build.

Relevant behavior today:

- `.github/workflows/build-release.yml` builds Linux wheels directly on `ubuntu-24.04`.
- `pybuild.py` renames Linux wheels using the host glibc version.
- `scripts/pycfml_build.sh` still performs Linux runtime copying, wheel renaming, and wheel metadata repair in one custom flow.
- `pybuild.toml` currently lists Linux gfortran runtime libraries including:
  - `libgfortran.so.5`
  - `libm.so.6`
  - `libc.so.6`
  - `libgcc_s.so.1`
  - `libmvec.so.1`
- `pypi-publish.yml` publishes wheels only.
- `tools/validate_pypi_wheel_filenames.py` validates whether wheel filenames are acceptable to PyPI, but it does not make the Linux wheel genuinely manylinux-compatible.

## Why Colab Fails

The current failure mode is most likely:

1. Colab looks for a compatible `cp312` Linux wheel.
2. PyPI offers only `manylinux_2_39_x86_64` Linux wheels.
3. Colab does not claim compatibility with that floor.
4. There is no `sdist`, so `pip` has nothing else to try.

## Why Pixi And EasyDiffraction Still Fail

The EasyDiffraction failure is a different compatibility problem from Colab.

Observed Pixi resolver error:

```text
Because all versions of crysfml have no wheels with a matching platform tag
(e.g., `manylinux_2_28_x86_64`) ...
```

This means:

1. Pixi is solving the `linux-64` environment against a Linux compatibility floor around `manylinux_2_28_x86_64`.
2. Current published `crysfml` Linux wheels are `manylinux_2_35_x86_64` and `manylinux_2_39_x86_64`.
3. Those wheels are therefore too new for Pixi's default Linux solve target.
4. The package also does not publish an `sdist`, so Pixi has no source fallback.

This is why both of the following can be true at the same time:

- `pip install crysfml` works on a specific Colab runtime with glibc 2.35.
- `pixi` still refuses to solve `linux-64` for a downstream project.

## Fast Downstream Workaround

If EasyDiffraction is willing to require glibc 2.35 or newer on Linux, the downstream Pixi project can raise its Linux system requirement.

Example:

```toml
[system-requirements]
libc = "2.35"
```

If that requirement should apply only to a Linux-specific Pixi environment, put the same `system-requirements` entry on a Linux-only feature or environment instead of using `target.linux-64`, since Pixi does not support target-specific `system-requirements` in the same way it supports target-specific dependencies.

That should allow Pixi to accept the existing `manylinux_2_35_x86_64` wheel.

This is only appropriate if EasyDiffraction really wants to drop older glibc-based Linux systems from its supported matrix.

## Correct Repository-Side Fix For Pixi Consumers

If `crysfml` is meant to remain broadly installable as a PyPI dependency in Pixi-managed `linux-64` environments, the Linux release wheel must be published with a lower manylinux floor.

Recommended targets:

- preferred: `manylinux2014_x86_64` / `manylinux_2_17_x86_64`
- acceptable compromise: `manylinux_2_28_x86_64`

In practice that means:

1. stop treating host-Ubuntu retagging as the Linux release mechanism
2. build Linux wheels inside a real manylinux container
3. run `auditwheel repair`
4. publish the repaired wheel

Publishing an `sdist` would still be useful as a fallback, but it is not a substitute for a compatible manylinux wheel because downstream source builds will require a more complex Fortran-capable build environment.

## Will Ubuntu 22.04 In The Build Matrix Fix Colab?

Short answer: no, not as a correct release fix.

What changing Linux builds from `ubuntu-24.04` to `ubuntu-22.04` would do:

- It would likely reduce the host glibc floor from 2.39 to 2.35.
- Under the current rename logic, Linux wheels would likely be retagged as `manylinux_2_35_x86_64`.

Why that is still not enough:

- Building on plain Ubuntu is still not the same as building inside a manylinux container.
- The current Linux path still manually copies runtime libraries before wheel creation.
- The current Linux runtime list includes glibc/system libraries that should not be bundled for a proper manylinux wheel.
- A host-Ubuntu build may happen to install on some Colab images, but it is not a reliable or policy-sound compatibility strategy.

Conclusion:

- `ubuntu-22.04` may lower the compatibility floor and might help in some cases.
- It is not a proper substitute for a manylinux build plus `auditwheel repair`.
- It should not be treated as the long-term Linux release solution.

## Recommended Compatibility Targets

Primary Colab-oriented target:

- `cp312-cp312-manylinux2014_x86_64`
- equivalent PEP 600 floor: `cp312-cp312-manylinux_2_17_x86_64`

If the toolchain requires something newer, a secondary compromise target is:

- `cp312-cp312-manylinux_2_28_x86_64`

That may still be useful for modern Linux environments, but it is less conservative than `manylinux2014` / `manylinux_2_17`.

## Planned Migration

The safest repo-specific migration path is:

1. Keep the current macOS and Windows release flow.
2. Replace only the Linux release leg with a dedicated manylinux-container build.
3. Build CFML and the Python extension inside the manylinux container.
4. Stop using host-glibc wheel retagging for Linux.
5. Stop bundling glibc/system libraries like `libc.so.6`, `libm.so.6`, and `libmvec.so.1`.
6. Run `auditwheel show` and `auditwheel repair` on the raw Linux wheel.
7. Test the repaired Linux wheel, not the pre-repair wheel.
8. Publish an `sdist` in addition to wheels.

## Concrete Repository Changes Likely Needed

Expected areas to change:

- `.github/workflows/build-release.yml`
  - split Linux from macOS/Windows release handling
  - add containerized manylinux Linux wheel build
  - validate and test the repaired Linux wheel
- `pybuild.py`
  - disable or bypass Linux host-glibc retagging for the manylinux path
  - disable or bypass Linux manual runtime-library copying for the manylinux path
- `pybuild.toml`
  - remove glibc/system libraries from `extra-libs.linux.x86_64.gfortran`
- `scripts/pycfml_build.sh`
  - keep raw wheel build step, but do not treat manual rename as final Linux packaging
- `scripts/install_pycfml_from_wheel.sh`
  - ensure tests install the repaired Linux wheel if the repaired wheel is written to a different location
- `.github/workflows/pypi-publish.yml`
  - publish `sdist` artifacts as well as wheels

## Validation To Use During Migration

On Colab or a Colab-like environment:

```python
import sys, platform
from packaging import tags

print(sys.version)
print(platform.machine())
print(platform.libc_ver())
print(next(tags.sys_tags()))
```

On built Linux wheels:

```bash
python -m pip debug --verbose
python -m wheel tags dist/*.whl
auditwheel show dist/*.whl
auditwheel repair dist/*.whl -w wheelhouse
```

## Practical Next Step

The next atomic change should be:

- introduce a dedicated manylinux Linux wheel build in `build-release.yml`
- stop using host-Ubuntu wheel retagging as the Linux release mechanism

This is the smallest change that directly addresses Colab compatibility at the root cause.

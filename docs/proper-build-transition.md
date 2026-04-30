# Proper Build Transition

## Outcome

Move the package from a script-generated artifact assembly flow to a proper
source-buildable Python package built by scikit-build-core and a repo-owned
CMake project.

The end state is intentionally narrow and understandable:

- one packaging entry point at the repository root
- one native-build source of truth in repo-owned CMake files
- vendored CrysFML sources kept in `repo/CFML` with no network clone at build time
- one package layout installed by CMake and packed by the backend
- one sdist contract that can be rebuilt into the same kind of wheel in CI

## Current Implementation Checkpoint

This branch has moved past the original scaffold-only slice.

It now owns the active release workflow end to end in CI and the default local
maintainer wheel-validation path, but it still keeps explicit legacy fallback
tasks. It also owns the root packaging entry point and compiles native code
from the vendored sources.

The current checkpoint does fourteen things:

1. introduces a root CMake entry point owned by this repository
2. replaces the grouped scaffold manifests with explicit source lists copied
   from the validated `pybuild.toml` ordering
3. builds `cfml_core` and `pycfml_extension` for `gfortran` only
4. stages the curated package init, vendored Python helper modules, and bundled
   database file through CMake install rules
5. builds wheels through `scikit-build-core` from the repository root
6. tests installed wheels in CI without generated wheel-test scripts
7. validates the `sdist -> wheel -> installed-wheel tests` path in release CI
8. stages the validated `sdist` alongside the wheels for release publication
9. repairs macOS release wheels with `delocate` before validation and staging
10. builds Linux release wheels in a dedicated manylinux container via
    `cibuildwheel`
11. repairs Windows release wheels with `delvewheel` before validation and
    staging
12. builds macOS and Windows release wheels on native runners through
    `cibuildwheel`
13. makes the default maintainer `pixi` wheel and sdist validation tasks
    repo-owned while keeping the old script-generated path under explicit
    legacy task names
14. keeps the native macOS and Windows wheel-repair helpers as explicit local
    maintainer diagnostics through repo-owned `pixi` tasks

## Current Validated Contract

The validated package contract is currently represented by the repo-owned CMake
build, `pybuild.py` / `pybuild.toml` as parity references, and the release
workflows.

The package contract that must be preserved is:

- use the tracked vendored sources from `repo/CFML`
- compile CFML first, then the pyCFML wrapper library
- copy the curated package init from `src/__init__.py`
- copy the vendored Python helper modules from `repo/CFML/PythonAPI/Python`
- bundle the magnetic database file under `crysfml/Databases`
- bundle compiler runtime libraries needed by the built extension
- fix runtime search paths on Linux and macOS
- publish binary wheels that are tagged and marked as non-pure

The current release flow now produces a rebuildable source distribution and
builds every release wheel through repo-owned `cibuildwheel` policy. The
default local maintainer path now follows repo-owned wheel and sdist helpers as
well, macOS and Windows have explicit native repair-diagnostic tasks, but
legacy script-generated fallbacks are still present.

## Current Hybrid State

The branch now spans both worlds: repo-owned packaging, release-wheel CI, and
default maintainer validation are real, but explicit legacy fallbacks are still
available while parity is being proven.

### Latest completed slice

- `build-debug.yml` now builds wheels directly from the repository root with
  `python -m build --wheel`
- the release workflow now builds Linux, macOS, and Windows wheels through
  `cibuildwheel`, using a manylinux container on Linux and native runners on
  macOS and Windows
- those build jobs now validate the produced wheel with
  `tools/run_installed_wheel_tests.py`
- the default CI path no longer generates shell scripts or runs
  `scripts/cfml_build.sh`, `scripts/cfml_test.sh`, or `scripts/pycfml_test.sh`
- the second CI job still re-tests the downloaded wheel artifact before release
  staging
- `build-release.yml` now validates the `sdist -> wheel -> installed-wheel
  tests` path through `tools/validate_sdist_rebuild.py`
- the validated `sdist` now moves through the same draft-release and PyPI
  publication path as the validated wheels
- `tools/validate_pypi_wheel_filenames.py` now accepts `delocate`-normalized
  macOS minimum-version tags instead of a hard-coded macOS-major allowlist
- the Linux release leg now builds one wheel per Python version inside a
  `manylinux2014` container via `cibuildwheel`, runs `auditwheel show` and
  `auditwheel repair`, re-tests the repaired wheel on `ubuntu-24.04`, and
  re-tests the downloaded artifact on both `ubuntu-22.04` and `ubuntu-24.04`
- the macOS release leg now builds one wheel per Python version through the
  repo-owned `[tool.cibuildwheel.macos]` policy on `macos-14`, repairs it with
  `delocate`, re-tests that repaired artifact, and uploads the repaired wheel
- the Windows release leg now builds one wheel per Python version through the
  repo-owned `[tool.cibuildwheel.windows]` policy on `windows-2022`, repairs it
  with `delvewheel`, re-tests that repaired artifact, and uploads the repaired
  wheel
- the default maintainer `pixi` tasks `pycfml-build`, `pycfml-test`,
  `sdist-validate`, and `full` now run repo-owned wheel and sdist helpers
- the existing `tools/repair_macos_wheel.py` and
  `tools/repair_windows_wheel.py` helpers now remain first-class local
  maintainer diagnostics through `tools/run_repair_diagnostics.py` and the
  native `pixi` tasks `pycfml-repair-diagnostics-macos` and
  `pycfml-repair-diagnostics-windows`
- `tools/run_installed_wheel_tests.py` now reinstalls the built wheel with
  `pip install --force-reinstall --no-deps`, relying on the managed test
  environment instead of live index resolution during validation
- the old script-generated maintainer path remains available only through
  explicit legacy task names such as `pycfml-build-legacy`,
  `pycfml-test-legacy`, and `full-legacy`

What is already repo-owned:

- explicit CFML and pyCFML source manifests derived from `pybuild.toml`
- `cfml_core` and `pycfml_extension` targets for `gfortran` only
- CMake install rules for `src/__init__.py`, vendored Python helper modules,
  and the bundled magnetic database
- `pyproject.toml` switched to `scikit-build-core` with a repo-local
  `versioningit` metadata provider
- `pyproject.toml` now carries the repo-owned cibuildwheel policy for Linux
  `manylinux2014` + `auditwheel`, macOS arm64 + `delocate`, and Windows AMD64 +
  `delvewheel`
- `tools/build_local_wheel.py` now owns the default local wheel build helper
- `tools/run_repair_diagnostics.py` now owns the native macOS and Windows
  repaired-wheel diagnostic flow on maintainer hosts
- `pixi.toml` now points the default maintainer wheel and sdist tasks at
  repo-owned helpers and includes the backend prerequisites plus runtime
  dependencies needed for local wheel build and reinstall/test diagnostics
- `pixi.toml` now exposes explicit `repair-macos` and `repair-windows`
  environments plus native `pycfml-repair-diagnostics-*` tasks for optional
  repaired-wheel diagnostics on maintainer hosts
- default CI build and test steps that install the built wheel directly through
  `tools/run_installed_wheel_tests.py`
- release CI source-rebuild validation through `tools/validate_sdist_rebuild.py`
- Linux release-wheel build and repair through `cibuildwheel` plus `auditwheel`
- macOS release-wheel build and repair through `cibuildwheel` plus `delocate`
- Windows release-wheel build and repair through `cibuildwheel` plus
  `delvewheel`
- draft-release staging and PyPI publication that consume both validated wheels
  and the validated `sdist`
- benchmark-only CI test legs removed from the default workflow path

What has already been validated locally from the repository root:

- `cmake -S . -B <build-dir>` succeeds
- `cmake --build <build-dir> --target cfml_core` succeeds for `gfortran`
- `cmake --build <build-dir> --target pycfml_extension` succeeds for
  `gfortran`
- `python -m build --wheel --outdir <wheel-dir>` succeeds
- `python tools/build_local_wheel.py --wheel-dir <wheel-dir>` succeeds
- `python tools/repair_macos_wheel.py --wheel-dir <wheel-dir>` succeeds on the
  raw macOS wheel and produces a repaired wheel with bundled GNU runtime dylibs
- `python tools/validate_pypi_wheel_filenames.py <wheel-dir>` succeeds for the
  repaired macOS wheel artifact
- `python tools/run_installed_wheel_tests.py --wheel-dir <wheel-dir>` passes
  for the wheel built from that path
- `python tools/validate_sdist_rebuild.py` succeeds
- `pixi run --environment default pycfml-build` succeeds after adding the
  repo-owned backend prerequisites to the default environment
- `pixi run --environment wheeltest pycfml-test` passes against the built wheel
- `pixi run --environment default sdist-validate` succeeds
- `pixi run --environment repair-macos pycfml-build` succeeds on macOS
- `pixi run pycfml-repair-diagnostics-macos` succeeds on macOS, repairing the
  raw wheel with `delocate`, validating the repaired filename, and passing the
  installed-wheel test suite
- `python -m cibuildwheel --platform linux --print-build-identifiers` resolves
  the intended `cp311` to `cp314` `manylinux_x86_64` build targets from the
  repo-owned Linux cibuildwheel configuration
- `python -m cibuildwheel --platform macos --archs arm64
  --print-build-identifiers` resolves the intended `cp311` to `cp314`
  `macosx_arm64` build targets from the repo-owned macOS cibuildwheel
  configuration
- `python -m cibuildwheel --platform windows --archs AMD64
  --print-build-identifiers` resolves the intended `cp311` to `cp314`
  `win_amd64` build targets from the repo-owned Windows cibuildwheel
  configuration
- `python -m delvewheel show -h` and `python -m delvewheel repair -h` succeed
  locally, confirming the Windows repair command surface used by
  `tools/repair_windows_wheel.py`

What is still hybrid:

- explicit legacy maintainer tasks such as `full-legacy`,
  `pycfml-build-legacy`, `pycfml-test-legacy`, plus standalone script-oriented
  helpers like `scripts`, `cfml-build`, and `cfml-test`, still call
  `pybuild.py` and generated `scripts/`
- local macOS builds still emit deployment-target mismatch warnings on this
  machine before `delocate` normalizes the repaired wheel tag
- the Linux manylinux build was not run locally on this machine because no
  Docker- or Podman-compatible container runtime is available here
- the macOS native `cibuildwheel` build was not run locally on this machine end
  to end; only the repo-owned target resolution was validated locally
- the Windows native `cibuildwheel` build and repaired-wheel path were not
  exercised locally because no Windows runner is available on this machine

## Vendored CMake Audit

The vendored CMake build in `repo/CFML` is useful as a compilation reference,
but not yet correct as a package build.

What is already useful:

- it configures cleanly for a gfortran + Python API build
- it already knows the core library target and the wrapper target shapes
- it already distinguishes optional ODR and test-program slices

What must not become the final package build unchanged:

- it installs the upstream Python package layout, not the curated shipped one
- it does not bundle the curated `src/__init__.py`
- it does not bundle runtime libraries or reproduce the current RPATH fixes
- it installs into source-tree-oriented prefixes instead of wheel staging roots
- its compiler-flag layer has drifted from the currently validated pybuild flags
- its Python wrapper manifest has drifted from `pybuild.toml`

Concrete examples already observed:

- the vendored PythonAPI CMake still lists `Wraps_Laue`, while the validated
  wrapper manifest includes `Wraps_Powder`
- the vendored macOS Intel global-deps filename uses `MacOS`, but the tracked
  file is actually named `CFML_GlobalDeps_MacOs_INTEL.f90`
- the ODR CMake file builds `odr_dp` from the wrong source list

## Proposed Canonical Layout

These files become the long-term packaging entry points:

- `CMakeLists.txt`: root orchestration only
- `cmake/CfmlSourceManifest.cmake`: explicit CFML source ownership
- `cmake/PyCfmlSourceManifest.cmake`: explicit pyCFML source ownership
- `cmake/CompilerProfiles.cmake`: compiler- and platform-specific options
- `cmake/InstallLayout.cmake`: package install destinations and data files
- `cmake/RuntimeRepair.cmake`: only minimal build-time runtime-path setup
- `package/crysfml/__init__.py`: future curated package init
- `package/crysfml/*.py`: installed Python helper modules staged by CMake

The vendored tree remains input data:

- `repo/CFML/Src`: core Fortran sources
- `repo/CFML/PythonAPI/Fortran`: wrapper sources
- `repo/CFML/PythonAPI/Python`: upstream helper modules

## Planned Target Names

The new root build should stay flat and obvious.

- `cfml_core`: static core Fortran library built from vendored CrysFML sources
- `pycfml_extension`: Python extension target with output name `crysfml08lib`
- `cfml_groups08`: optional vendored test program
- `cfml_nfp`: optional vendored test program

Optional exported aliases can come later:

- `crysfml::cfml_core`
- `crysfml::pycfml_extension`

## Source Ownership Rules

There should be one source of truth for each concern.

- Packaging metadata belongs in `pyproject.toml`
- Native source manifests belong in repo-owned CMake include files
- Vendored code stays vendored; it is not edited for packaging policy unless a
  correctness fix is required upstream as well
- Runtime wheel repair belongs to standard platform tools, not handwritten shell
  scripts, once parity has been proven

During transition, `pybuild.py` remains the oracle for parity, not the final
implementation.

## Platform Release Policy

The proper build must define one release policy per artifact type, not one
generic "wheel build" step that behaves differently on each host.

### Linux release wheels

Linux wheels must become real manylinux wheels.

- initial release scope: `x86_64` + `gfortran` only
- preferred compatibility floor: `manylinux_2_17_x86_64`
- highest acceptable floor without an explicit downstream support decision:
  `manylinux_2_28_x86_64`
- never derive the Linux compatibility tag from the host glibc version
- never publish raw `linux_x86_64` wheels
- never bundle glibc or other system libraries such as `libc.so.6`, `libm.so.6`,
  or `libmvec.so.1`
- build inside a real manylinux container, not directly on a GitHub Ubuntu host
- run `auditwheel show` on the raw wheel and `auditwheel repair` on the release
  wheel
- test the repaired wheel, not the pre-repair wheel

To keep the Linux path understandable and reproducible, prefer a small
repo-owned manylinux image definition over ad hoc package installation in the CI
workflow.

That image should contain at least:

- the chosen manylinux base image
- `gfortran`
- the Python build prerequisites needed by the backend
- any native utilities required before `auditwheel repair`

The Linux release path should stay intentionally narrow at first. `ifx` remains
outside the current repo-owned build scope until the gfortran manylinux path is
stable and explicitly revisited.

### macOS release wheels

macOS wheels should be built on native GitHub macOS runners with native Python,
then repaired with `delocate`.

- build on native runners only; do not introduce cross-compilation in the first
  proper-build migration
- start with one wheel per proven architecture rather than promising
  `universal2` immediately
- preserve the currently validated package behavior for bundled Fortran runtime
  libraries and `@loader_path`-based loading
- use `delocate-listdeps` and `delocate-wheel` as the release repair steps
- test the delocated wheel in a fresh environment before artifact promotion

The first proper-build release can keep the currently proven macOS architecture
scope and widen later only if CI and runtime validation show that it is safe.

### Windows release wheels

Windows wheels should be built on native `windows-2022` runners and repaired
with `delvewheel`.

- initial release scope: `gfortran` first, matching the current release focus
- do not add `ifx` to the repo-owned build during the first proper-build
  migration
- bundle the runtime DLLs required by the final `.pyd`
- preserve the curated package init behavior that adds the package directory to
  DLL resolution on Windows
- run `delvewheel show` or equivalent dependency inspection before the final
  repair step
- test import and basic runtime behavior from a fresh environment after repair

### Non-release compiler paths

Support for `ifx`, `nagfor`, or other compilers is explicitly deferred. The
repo-owned build should implement and validate `gfortran` only until the full
sdist and wheel path is correct end to end.

Additional compiler support should be treated as a later, separate migration
phase rather than being threaded through the first proper-build implementation.

The first proper-build release goal is:

- one correct sdist
- one correct manylinux Linux wheel path
- one correct macOS wheel path
- one correct Windows wheel path

Only after that works should the matrix widen.

## sdist Contract

The sdist must become the canonical source artifact for downstream packagers.

### What the sdist must contain

- root `CMakeLists.txt` and repo-owned CMake include files
- `pyproject.toml`, `README.md`, and `LICENSE`
- the curated package files used to build the final wheel
- vendored CrysFML core sources from `repo/CFML/Src`
- vendored pyCFML Fortran and Python sources from `repo/CFML/PythonAPI`
- any build-time metadata needed to derive the same version and build graph from
  the source artifact

### What the sdist must not contain

- `build/`
- `dist/`
- generated `scripts/`
- `.benchmarks/`
- local environment folders and caches
- issue notebooks, exploratory artifacts, or unrelated repository analysis

### sdist validation contract

Each release candidate must prove all of the following:

1. `python -m build --sdist` succeeds from a clean checkout
2. the produced tarball unpacks into a self-contained source tree
3. `pip wheel <sdist>` produces a real native wheel, not a metadata-only wheel
4. the wheel built from the sdist passes at least import smoke tests and the
   package tests required for release confidence

The sdist should be built once per release from the repository root and then
published alongside the wheels from the same release pipeline.

## Planned CI Release Topology

The future proper-build CI should separate source creation, wheel creation,
wheel repair, and release publication.

### Release artifact stages

1. create a validated sdist from the repository root
2. rebuild a wheel from that sdist in a clean validation environment
3. build platform wheels from the tagged source tree using cibuildwheel
4. repair platform wheels with the standard platform tool
5. test the repaired wheels
6. stage the validated wheels and sdist on the draft GitHub release
7. publish those exact staged artifacts to PyPI

### Platform-specific execution model

- Linux: cibuildwheel + manylinux container + `auditwheel repair`
- macOS: cibuildwheel on native runners + `delocate-wheel`
- Windows: cibuildwheel on native runners + `delvewheel repair`

### Release artifact ownership

- the sdist is the source-of-truth artifact for downstream packagers
- repaired wheels are the only wheel artifacts eligible for publication
- raw wheels are CI intermediates only
- PyPI publication should consume already validated artifacts, not rebuild them

### Benchmark test simplification note

- benchmark-only CI test legs are optional during the proper-build migration
- if they continue to complicate the workflows, prefer removing benchmark test
  steps and keeping correctness-oriented import, unit, and functional tests
- benchmark collection can remain a local or explicitly opt-in maintenance task
  instead of a default release-validation requirement

## Migration Order

### Phase 1: Scaffold [landed]

- add root `CMakeLists.txt`
- add source-manifest include files
- document the target layout and commit boundaries

### Phase 2: Core target [landed]

- enable Fortran in the root project
- create the `cfml_core` target
- move compiler options from `pybuild.toml` into target-based CMake profiles
- preserve the validated release/debug behavior for `gfortran` only

### Phase 3: Python extension target [landed]

- create the `pycfml_extension` target linked for `gfortran` only
- install the vendored helper modules and curated package init into the staged
  package tree
- install bundled databases with the package

### Phase 4: Wheel semantics [landed in CI]

- switch `pyproject.toml` to scikit-build-core
- build wheels from the CMake install tree
- replace the default CI script-generated build path with repository-root
  `python -m build --wheel` plus installed-wheel tests
- remove the current manual wheel renaming and purelib metadata patching from
  the active wheel path
- ensure the backend-emitted wheel metadata remains correct without post-build
  filename surgery

### Phase 5: Runtime repair [partially landed]

- use `auditwheel` on Linux after building inside a real manylinux image in
  release CI
- use `delocate` on macOS after native wheel build in release CI
- use `delvewheel` on Windows after native wheel build in release CI
- delete the handwritten runtime-library copy and RPATH shell logic only after
  repair-based wheel parity is proven

### Phase 6: Source rebuild validation [landed in release CI]

- add CI that builds an sdist
- rebuild a wheel from that sdist in a clean environment
- run the package tests against the rebuilt wheel
- make the wheel-from-sdist check mandatory for release readiness

### Phase 7: Release migration [mostly landed in release CI]

- move the release wheel matrix to cibuildwheel
- split Linux into a dedicated manylinux-based release leg instead of
  host-Ubuntu retagging in release CI
- publish both wheels and a validated sdist to PyPI
- retire the current script-generated wheel assembly flow

## Validation Gates

Each phase should have one clear gate before moving on.

- scaffold: `cmake -S . -B <build-dir>` succeeds from the repository root
- core target: `cmake --build <build-dir> --target cfml_core` succeeds for
  `gfortran`
- extension target: local wheel imports and unit tests pass
- wheel semantics: wheel metadata shows a non-pure native wheel
- Linux release wheel: `auditwheel show` reports a valid manylinux-compatible
  wheel and `auditwheel repair` succeeds
- macOS release wheel: `delocate-listdeps` and `delocate-wheel` succeed on the
  raw wheel
- Windows release wheel: dependency inspection and `delvewheel repair` succeed
- source rebuild: `pip wheel crysfml-<version>.tar.gz` produces a usable wheel
- release publication: PyPI upload consumes the already validated wheels and
  sdist staged on the draft GitHub release

## Commit Boundaries

To keep the migration understandable, each commit should do one of these only:

- scaffold root packaging CMake
- add core source manifests
- add core target
- add Python extension target
- switch backend to scikit-build-core
- add Linux manylinux release path
- add macOS and Windows repair-based release paths
- add validated sdist release path
- migrate wheel repair to standard tools
- switch release CI to cibuildwheel

## Next Follow-Up Changes After Native Repair Diagnostics

The next implementation slice should do exactly these things:

1. delete the handwritten runtime-library copy and wheel-tag surgery only after
   repaired-wheel parity is proven across all release and maintainer paths
2. remove or archive the explicit legacy `pixi` and generated-script fallback
   tasks once maintainers no longer need the old path

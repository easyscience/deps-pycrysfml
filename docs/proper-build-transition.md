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
maintainer wheel-validation path. It also owns the root packaging entry point,
compiles native code from the vendored sources, and has now retired the
explicit legacy pyCFML wrapper tasks while replacing the remaining
script-generated vendoring path with repo-owned maintainer helpers.

The current checkpoint does twenty things:

1. introduces a root CMake entry point owned by this repository
2. replaces the grouped scaffold manifests with explicit source lists copied
  from the historically validated source ordering that has now been retired
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
    repo-owned
14. keeps the native macOS and Windows wheel-repair helpers as explicit local
    maintainer diagnostics through repo-owned `pixi` tasks
15. removes the remaining low-level `pybuild.py` fallback wheel logic now
    that the backend already emits and validates native wheels through
    repo-owned helpers
16. removes the unpackaged `dist/pyCFML` assembly helpers from the repo-owned
  maintainer surface, leaving the wheel and sdist path fully owned by the
  repository-root backend and helper tools
17. forces the Windows `cibuildwheel` + `scikit-build-core` path onto Ninja so
    MinGW `gfortran` no longer falls back to the incompatible Visual Studio
    generator inside isolated release-wheel builds
18. removes the explicit `pycfml-*-legacy` / `full-legacy` maintainer tasks
  and retires the old generated pyCFML wrapper-script surface entirely
19. replaces the remaining generated `vendor-cfml-*` helper path with
  `tools/vendor_cfml.py` plus CMake-native CFML distribution and vendored
  test-program targets
20. removes `pybuild.py`, `pybuild.toml`, and the obsolete generated
  `scripts/` helper files after validating the repo-owned vendoring helper
  path through the same maintainer `pixi` commands

## Current Validated Contract

The validated package contract is currently represented by the repo-owned CMake
build, the manifest files under `cmake/`, the repo-owned helper tools under
`tools/`, and the release workflows.

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
well, macOS and Windows have explicit native repair-diagnostic tasks, and the
remaining vendor-prefixed CFML refresh/build/test helpers now route through
`tools/vendor_cfml.py` and repo-owned CMake targets rather than a generated
script surface.

## Current Hybrid State

The branch now spans both worlds: repo-owned packaging, release-wheel CI, and
default maintainer validation are real, and the remaining vendor-prefixed CFML
maintainer surface is now repo-owned as well, but it intentionally stays
outside the downstream package build contract.

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
- the dormant pyCFML fallback wheel, install, runtime-library copy, and RPATH
  helper definitions have now been removed, so the repo no longer retains a
  second low-level wheel assembly implementation
- the repo-owned Windows `cibuildwheel` policy now sets
  `CMAKE_GENERATOR=Ninja`, and the repo-owned `scikit-build-core` config now
  requires Ninja in isolated builds, so release-wheel builds do not fall back
  to the incompatible Visual Studio generator when they probe MinGW `gfortran`
- `tools/run_installed_wheel_tests.py` now reinstalls the built wheel with
  `pip install --force-reinstall --no-deps`, relying on the managed test
  environment instead of live index resolution during validation
- the explicit `pycfml-build-legacy`, `pycfml-dist-legacy`,
  `pycfml-test-legacy`, and `full-legacy` maintainer tasks have now been
  removed from `pixi.toml`
- the remaining vendor-prefixed `pixi` tasks now call `tools/vendor_cfml.py`
  instead of `pybuild.py` and generated `scripts/*.sh`
- the repo-owned CMake build now exposes `cfml_vendor_distribution` and
  `cfml_vendor_test_programs` so the vendored CFML maintainer path is built by
  the same native build system as the package itself
- the obsolete generated `scripts/` helper surface has now been removed
  because the remaining vendor-maintenance path no longer consumes it

What is already repo-owned:

- explicit CFML and pyCFML source manifests derived from the retired historical
  pybuild manifests and now owned directly by the repository
- `cfml_core` and `pycfml_extension` targets for `gfortran` only
- `cfml_vendor_distribution` and `cfml_vendor_test_programs` targets for the
  remaining vendored CFML maintainer path
- CMake install rules for `src/__init__.py`, vendored Python helper modules,
  and the bundled magnetic database
- `pyproject.toml` switched to `scikit-build-core` with a repo-local
  `versioningit` metadata provider
- `pyproject.toml` now carries the repo-owned cibuildwheel policy for Linux
  `manylinux2014` + `auditwheel`, macOS arm64 + `delocate`, and Windows AMD64 +
  `delvewheel`
- `tools/build_local_wheel.py` now owns the default local wheel build helper
- `tools/vendor_cfml.py` now owns vendored CFML refresh/build/test maintenance
  without `pybuild.py`, `pybuild.toml`, or generated helper scripts
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
- benchmark-only CI test legs removed from the default workflow path, and the
  CI, direct installed-wheel correctness, and maintainer wheeltest paths no
  longer depend on
  `pytest-benchmark`

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
  for the wheel built from that path without `pytest-benchmark` installed in
  the validation environment
- `python tools/validate_sdist_rebuild.py` succeeds
- `pixi run --environment default --frozen vendor-cfml-build` succeeds after
  the vendored CFML maintainer path is moved onto `tools/vendor_cfml.py` and
  CMake-native CFML distribution staging
- `pixi run --environment default --frozen vendor-cfml-test` succeeds against
  the vendored CFML distribution built by that repo-owned maintainer path
- `pixi run --environment default --frozen pycfml-build` succeeds after adding
  the repo-owned backend prerequisites to the default environment
- `pixi run --environment wheeltest pycfml-test` passes against the
  built wheel
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
  configuration after adding the explicit Ninja-generator policy
- `python -m delvewheel show -h` and `python -m delvewheel repair -h` succeed
  locally, confirming the Windows repair command surface used by
  `tools/repair_windows_wheel.py`

What is still hybrid:

- vendor-prefixed maintainer helpers such as `vendor-cfml-refresh`,
  `vendor-cfml-refresh-branch`, `vendor-cfml-refresh-commit`,
  `vendor-cfml-build`, and `vendor-cfml-test` still remain as a maintainer-only
  vendoring surface even though they are now repo-owned and no longer rely on
  generated scripts
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
- its compiler-flag layer has drifted from the currently validated repo-owned
  GNU Fortran target profile
- its Python wrapper manifest has drifted from the repo-owned historical source
  manifest now carried under `cmake/`

Concrete examples already observed:

- the vendored PythonAPI CMake still lists `Wraps_Laue`, while the validated
  wrapper manifest includes `Wraps_Powder`
- the vendored macOS Intel global-deps filename uses `MacOS`, but the tracked
  file is actually named `CFML_GlobalDeps_MacOs_INTEL.f90`
- the ODR CMake file builds `odr_dp` from the wrong source list

## Current Canonical Layout

These files are the current packaging entry points:

- `CMakeLists.txt`: root orchestration, target definitions, and package install
  rules
- `cmake/CfmlSourceManifest.cmake`: explicit CFML source ownership
- `cmake/PyCfmlSourceManifest.cmake`: explicit pyCFML source ownership
- `cmake/CompilerProfiles.cmake`: compiler- and platform-specific options
- `src/__init__.py`: curated package init installed as `crysfml/__init__.py`
- `repo/CFML/PythonAPI/Python/*.py`: vendored helper modules installed into
  the package by CMake

The install layout currently lives directly in the root `CMakeLists.txt`.
Splitting that into dedicated layout or runtime-repair CMake modules is still a
possible cleanup, but it is not part of the current maintained build contract.

The vendored tree remains input data:

- `repo/CFML/Src`: core Fortran sources
- `repo/CFML/PythonAPI/Fortran`: wrapper sources
- `repo/CFML/PythonAPI/Python`: upstream helper modules

## Current Repo-owned Target Names

The current root build stays flat and explicit.

- `cfml_core`: static core Fortran library built from vendored CrysFML sources
- `cfml_vendor_distribution`: maintainer target that stages `dist/CFML/lib`
  and `dist/CFML/include` from the repo-owned core build
- `cfml_vendor_test_programs`: maintainer target that builds vendored CFML test
  programs into `dist/CFML/progs`
- `pycfml_extension`: Python extension module target with output name
  `crysfml08lib`
- `crysfml::cfml_core`: current alias for the core target
- `crysfml::pycfml_extension`: current alias for the extension target

Vendored test-program manifests are present, but optional targets such as
`cfml_groups08` and `cfml_nfp` are still reserved future expansion rather than
active repo-owned build targets.

## Source Ownership Rules

There should be one source of truth for each concern.

- Packaging metadata belongs in `pyproject.toml`
- Native source manifests belong in repo-owned CMake include files
- Vendored code stays vendored; it is not edited for packaging policy unless a
  correctness fix is required upstream as well
- Runtime wheel repair belongs to standard platform tools, not handwritten shell
  scripts, once parity has been proven

The historical `pybuild.py` parity layer has been retired; the repo-owned CMake
manifests and helper tools are now the active source of truth.

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

The current repo-owned Linux release path uses cibuildwheel's
`manylinux2014` image plus a minimal `before-all` package install for
`gcc-gfortran` and `git`. A custom derived manylinux image remains possible
follow-up cleanup, but it is not required for the current release contract.

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
- use `delocate-wheel` as the release repair step; use `delocate-listdeps` as
  a diagnostic when parity debugging is needed
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
- use `delvewheel repair` as the release repair step; use `delvewheel show` or
  equivalent dependency inspection as a diagnostic when parity debugging is
  needed
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

## Release CI Topology

The active release workflow now separates release-tag resolution, source
creation, wheel creation, wheel repair, validation, and publication. The debug
workflow remains intentionally narrower and still uses repository-root
`python -m build --wheel` plus installed-wheel tests rather than the full
release matrix.

### Release artifact stages

1. resolve the suggested release tag on `master` without pushing a remote tag
2. create a validated `sdist` from the repository root
3. rebuild a wheel from that `sdist` in a clean validation environment
4. build Linux, macOS, and Windows wheels from the tagged source tree through
  repo-owned `cibuildwheel` policy
5. repair platform wheels with the standard platform tool
6. test the repaired wheels on their producer jobs
7. re-test the downloaded wheel artifacts on consumer jobs
8. stage the validated wheels and `sdist` on the draft GitHub release
9. publish those exact staged artifacts to PyPI

### Platform-specific execution model

- Linux: `manylinux2014` via `[tool.cibuildwheel.linux]` + `auditwheel repair`
- macOS: arm64 on native runners via `[tool.cibuildwheel.macos]` +
  `delocate-wheel`
- Windows: AMD64 on native runners via `[tool.cibuildwheel.windows]` +
  `delvewheel repair`, with `CMAKE_GENERATOR=Ninja`

### Release artifact ownership

- the sdist is the source-of-truth artifact for downstream packagers
- repaired wheels are the only wheel artifacts eligible for publication
- raw wheels are CI intermediates only
- PyPI publication should consume already validated artifacts, not rebuild them

### Benchmark removal checkpoint

- benchmark-only CI test legs have been removed from the default debug and
  release workflows
- `pytest-benchmark` has been removed from the default `test` extra and from
  the installed-wheel helper path, the maintainer wheeltest environment, and
  the remaining `pixi` task surface
- the remaining powder-pattern functional tests now call the pattern builders
  directly instead of importing the benchmark plugin
- if performance measurements return later, keep them as explicit maintainer
  diagnostics or opt-in jobs rather than default release gates

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

### Phase 5: Runtime repair [landed in release CI]

- use `auditwheel` on Linux after building inside a real manylinux image in
  release CI
- use `delocate` on macOS after native wheel build in release CI
- use `delvewheel` on Windows after native wheel build in release CI
- retire the handwritten runtime-library copy and RPATH shell logic from the
  active maintainer path once repair-based wheel parity is proven

### Phase 6: Source rebuild validation [landed in release CI]

- add CI that builds an sdist
- rebuild a wheel from that sdist in a clean environment
- run the package tests against the rebuilt wheel
- make the wheel-from-sdist check mandatory for release readiness

### Phase 7: Release migration [landed in release CI]

- move the release wheel matrix to cibuildwheel
- split Linux into a dedicated manylinux-based release leg instead of
  host-Ubuntu retagging in release CI
- publish both wheels and a validated sdist to PyPI
- retire the current script-generated wheel assembly flow from active CI and
  release-publication paths

### Phase 8: Benchmark removal from the default test path [landed]

- remove benchmark-only CI legs from the default workflow path
- remove `pytest-benchmark` from the default `test` extra and helper commands
- convert remaining benchmark-wrapped correctness tests to direct calls

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
- benchmark removal: installed-wheel tests pass with `pytest-benchmark`
  absent from the validation environment, including the maintainer wheeltest
  Pixi path

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
- remove benchmark dependencies from the default correctness path

## Next Follow-Up Changes After Retiring pybuild

The next implementation slice should do exactly these things:

1. decide whether the remaining vendor-prefixed CFML maintenance tasks should
  stay as dedicated repo-owned maintainer helpers or collapse into documented
  one-off maintainer commands
2. decide whether `dist/CFML` staging should remain a source-tree maintainer
  convenience output or move fully under the CMake build tree
3. decide whether the remaining vendoring helper surface should stay in the
  source-distribution contract or move behind explicit sdist exclusions once
  vendoring maintenance is fully separated from the downstream build contract

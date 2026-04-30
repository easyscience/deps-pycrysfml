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

## Scope Of The First Migration Slice

This first slice is a scaffold only. It does not replace the current release
workflow and does not compile native code yet.

It does three things:

1. introduces a root CMake entry point owned by this repository
2. establishes manifest files that will become the native source of truth
3. freezes the migration target and file layout in one document

## Current Validated Contract

The current working behavior is defined by `pybuild.py`, `pybuild.toml`, and the
release workflows.

The package contract that must be preserved is:

- use the tracked vendored sources from `repo/CFML`
- compile CFML first, then the pyCFML wrapper library
- copy the curated package init from `src/__init__.py`
- copy the vendored Python helper modules from `repo/CFML/PythonAPI/Python`
- bundle the magnetic database file under `crysfml/Databases`
- bundle compiler runtime libraries needed by the built extension
- fix runtime search paths on Linux and macOS
- publish binary wheels that are tagged and marked as non-pure

The current release flow does that correctly enough to ship wheels, but not in a
way that produces a rebuildable source distribution.

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

## Migration Order

### Phase 1: Scaffold

- add root `CMakeLists.txt`
- add grouped source-manifest include files
- document the target layout and commit boundaries

### Phase 2: Core target

- enable Fortran in the root project
- create the `cfml_core` target
- move compiler options from `pybuild.toml` into target-based CMake profiles
- preserve the validated release/debug behavior for gfortran first

### Phase 3: Python extension target

- create the `pycfml_extension` target
- install the vendored helper modules and curated package init into the staged
  package tree
- install bundled databases with the package

### Phase 4: Wheel semantics

- switch `pyproject.toml` to scikit-build-core
- build wheels from the CMake install tree
- remove the current manual wheel renaming and purelib metadata patching

### Phase 5: Runtime repair

- use auditwheel on Linux
- use delocate on macOS
- use delvewheel on Windows
- delete the handwritten runtime-library copy and RPATH shell logic only after
  wheel parity is proven

### Phase 6: Source rebuild validation

- add CI that builds an sdist
- rebuild a wheel from that sdist in a clean environment
- run the package tests against the rebuilt wheel

### Phase 7: Release migration

- move the release wheel matrix to cibuildwheel
- publish both wheels and a validated sdist to PyPI
- retire the current script-generated wheel assembly flow

## Validation Gates

Each phase should have one clear gate before moving on.

- scaffold: `cmake -S . -B <build-dir>` succeeds from the repository root
- core target: `cmake --build <build-dir> --target cfml_core` succeeds for the
  supported release compiler
- extension target: local wheel imports and unit tests pass
- wheel semantics: wheel metadata shows a non-pure native wheel
- source rebuild: `pip wheel crysfml-<version>.tar.gz` produces a usable wheel

## Commit Boundaries

To keep the migration understandable, each commit should do one of these only:

- scaffold root packaging CMake
- add core source manifests
- add core target
- add Python extension target
- switch backend to scikit-build-core
- migrate wheel repair to standard tools
- switch release CI to cibuildwheel

## First Follow-Up Changes After This Scaffold

The next implementation slice should do exactly these things:

1. replace the grouped CFML scaffold manifest with explicit file lists copied
   from the currently validated `pybuild.toml` ordering
2. enable Fortran in the root CMake project
3. introduce `cfml_core` with target-based compile options for gfortran only
4. validate a clean local static-library build before touching wheel packaging
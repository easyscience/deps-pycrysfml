# Maintainer Workflows

This repository now has one supported maintainer path for local builds,
validation, and vendored-CFML maintenance. The goal is to keep the build surface
small, explicit, and easy to audit.

## Source Of Truth

- Native build ownership lives in the repository root `CMakeLists.txt` plus the
  source manifests under `cmake/`.
- Python packaging ownership lives in `pyproject.toml`, `scikit-build-core`,
  `versioningit`, and the helper tools under `tools/`.
- Release-wheel policy lives in `.github/workflows/build-release.yml`.
- Vendored-CFML refresh/build/test maintenance lives in `tools/vendor_cfml.py`
  and the CMake targets `cfml_vendor_distribution` and
  `cfml_vendor_test_programs`.

## Supported Compiler Scope

- The repo-owned maintainer path currently supports GNU Fortran (`gfortran`) only.
- `ifx` and `nagfor` are intentionally out of scope for now and should not be
  reintroduced into the repo-owned build path without an explicit follow-up design.

## Standard Local Validation

Use these commands for normal package maintenance:

```bash
pixi run pycfml-build
pixi run pycfml-test
pixi run sdist-validate
pixi run release-check
```

What each task does:

- `pycfml-build` builds a local wheel from the repository root.
- `pycfml-test` installs the wheel previously built by `pycfml-build` into a
  clean environment and runs the unit and functional test suites.
- `sdist-validate` builds an sdist, rebuilds a wheel from it, and tests the
  rebuilt wheel.
- `release-check` is the stable one-command entrypoint for mandatory local
  pre-release validation.
- `release-check` runs the complete standard local maintainer validation
  pipeline in this order:
  `pycfml-build` -> `pycfml-test` -> `sdist-validate`.
- `release-check` therefore covers all normal local package-validation steps:
  build a repository-root wheel, test that built wheel in a clean environment,
  build an sdist, rebuild a wheel from the sdist, and test the rebuilt wheel
  too.

## Source Rebuild Contract

- The published `sdist` is part of the supported release contract, not a
  best-effort artifact.
- Packaging changes must preserve the validated `sdist -> wheel ->
  installed-wheel tests` rebuild path.
- When changing package metadata, native install rules, or release-wheel
  behavior, rerun `pixi run sdist-validate` before treating the change as
  complete.

## Vendored CFML Maintenance

Use these commands only when maintaining the tracked vendored copy under
`repo/CFML`:

```bash
pixi run vendor-cfml-stage
pixi run vendor-cfml-test
pixi run vendor-cfml-validate
pixi run vendor-cfml-refresh
pixi run vendor-cfml-refresh-branch --branch master
pixi run vendor-cfml-refresh-commit --branch master --commit <sha>
```

Guardrails for that workflow:

- `vendor-cfml-stage` is the normal non-destructive staging step for vendored
  CFML.
- `vendor-cfml-stage` configures the repository-root CMake build with
  `CRYSFML_ENABLE_PYCFML_EXTENSION=OFF`, `CRYSFML_ENABLE_PYTHON_PACKAGE=OFF`,
  and `CRYSFML_ENABLE_VENDOR_TESTS=OFF`.
- `vendor-cfml-stage` builds the repo-owned `cfml_core` library and then runs
  the `cfml_vendor_distribution` target.
- `vendor-cfml-stage` stages the core library under `dist/CFML/lib` and the
  generated Fortran module files under `dist/CFML/include`.
- `vendor-cfml-stage` does not build a Python wheel, does not build the
  `pycfml_extension` target, and does not build the vendored CFML test
  programs.
- `vendor-cfml-test` reruns that CFML-only maintainer path with
  `CRYSFML_ENABLE_VENDOR_TESTS=ON` and builds the `cfml_vendor_test_programs`
  target into `dist/CFML/progs`.
- `vendor-cfml-validate` is the stable one-command entrypoint for that
  non-destructive vendored CFML validation path.
- `vendor-cfml-refresh*` is destructive and network-dependent. Use it only for
  an intentional vendoring refresh.
- Normal package builds and tests must continue to work from the tracked
  `repo/CFML` tree without cloning from the network.

## Release Operator Checks

Use these commands before changing release behavior or relying on a local
release-candidate build:

```bash
pixi run release-check
```

Optional native diagnostics:

- On macOS, run `pixi run pycfml-repair-diagnostics-macos` when touching
  repaired-wheel behavior.
- On Windows, run `pixi run pycfml-repair-diagnostics-windows` when touching
  repaired-wheel behavior on a native Windows host.
- The Linux manylinux wheel path is validated in CI unless the maintainer has a
  compatible local container runtime and intentionally exercises that slice.
- GitHub Actions should reuse the same repo-owned tasks through the versioned
  `ci-py311`, `ci-py312`, `ci-py313`, and `ci-py314` Pixi environments rather
  than calling the helper scripts directly.
- The supported CI Python-version matrix should be derived from those
  versioned `ci-py*` Pixi environments instead of being duplicated as
  hardcoded lists in workflow YAML.
- Jobs that only run repo-owned Pixi tasks do not need `actions/setup-python`
  or a separate host-side `pip` upgrade step.
- On GitHub Actions runners that already provide the needed GNU toolchain,
  prefer the runner-installed `gfortran-13`/`gcc-13`/`g++-13` aliases when they
  satisfy the job. This is especially important on `ubuntu-24.04`, where the
  `setup-fortran` PPA path is less reliable, and `macos-14` also provides the
  required GNU toolchain aliases.
- Maintainer build and test dependencies belong in `pixi.toml`; keep
  `pyproject.toml` focused on published package metadata and build-system
  requirements.

## Maintenance Rules

- Treat `.github/workflows/build-release.yml` as the sole repo-owned GitHub
  Actions build-and-test workflow unless an explicit replacement is introduced.
- Keep the source manifests under `cmake/` aligned with the sources staged into
  the Python package build.
- Prefer updating `pixi` task descriptions, helper tools, and this document
  together when the maintainer workflow changes.
- Do not reintroduce `pybuild.py`, `pybuild.toml`, or generated top-level
  helper scripts as alternate build entrypoints.

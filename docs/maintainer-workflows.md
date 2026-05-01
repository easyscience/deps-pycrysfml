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
- `ifx` is intentionally out of scope for now and should not be reintroduced
  into the repo-owned build path without an explicit follow-up design.

## Standard Local Validation

Use these commands for normal package maintenance:

```bash
pixi run pycfml-build
pixi run pycfml-test
pixi run sdist-validate
pixi run full
pixi run release-check
```

What each task does:

- `pycfml-build` builds a local wheel from the repository root.
- `pycfml-test` installs that wheel into a clean environment and runs tests.
- `sdist-validate` builds an sdist, rebuilds a wheel from it, and tests the
  rebuilt wheel.
- `full` runs the standard local maintainer validation pipeline.
- `release-check` is the stable one-command entrypoint for mandatory local
  pre-release validation.

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
pixi run vendor-cfml-build
pixi run vendor-cfml-test
pixi run vendor-cfml-validate
pixi run vendor-cfml-refresh
pixi run vendor-cfml-refresh-branch --branch master
pixi run vendor-cfml-refresh-commit --branch master --commit <sha>
```

Guardrails for that workflow:

- `vendor-cfml-build` and `vendor-cfml-test` are the normal non-destructive
  validation path for vendored CFML.
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

## Maintenance Rules

- Keep `.github/workflows/build-debug.yml` aligned with
  `.github/workflows/build-release.yml` when the change affects both paths.
- Keep the source manifests under `cmake/` aligned with the sources staged into
  the Python package build.
- Prefer updating `pixi` task descriptions, helper tools, and this document
  together when the maintainer workflow changes.
- Do not reintroduce `pybuild.py`, `pybuild.toml`, or generated top-level
  helper scripts as alternate build entrypoints.

# Copilot Instructions

## Project Context

- Python bindings for the CrysFML2008 crystallographic library
- This project must be developed to be as error-free as possible, with
  the same rigour applied to critical software (e.g. nuclear-plant
  control systems).

## Build And Release Workflow

- Treat [.github/workflows/build-release.yml] as the canonical build
  pipeline for release-mode wheel builds in this repository.
- `build-release.yml` is used to build crysfml with the release-mode
  options from the repo-owned CMake path via `python -m build --wheel`,
  repair macOS wheels with `delocate`, run tests against the final wheel
  artifact, validate the `sdist -> wheel` rebuild path, and upload
  validated wheel and sdist artifacts to the workflow run for local
  validation and later release staging.
- The intended release flow is staged: merging `develop` into `master`
  via pull request should result in a successful `build-release.yml`
  run on `master`, after which `release-notes.yml` should create or
  update the draft GitHub release for that commit.
- Publishing that draft GitHub release should be treated as the event
  that triggers `pypi-publish.yml`. That publish workflow must consume
  the wheel and sdist artifacts already staged on the GitHub release rather
  than rebuilding different artifacts unless explicitly requested.
- Treat `release-notes.yml` as the stage that drafts or updates the
  GitHub release on merges to `master` with the suggested tag, release
  title, and release notes, while leaving remote tag creation to manual
  release publication from the GitHub UI.
- Treat `build-release.yml` as the stage that computes the same
  suggested release tag on `master`, creates that tag locally in CI so
  `versioningit` builds exact-version wheels without pushing a remote
  tag, repairs the macOS wheel artifact before validation, and uploads
  the validated wheels and sdist to the GitHub draft release.
- Treat `pybuild.py` and `pybuild.toml` as the source of truth for the
  generated shell scripts in `scripts/`, but not as the canonical release
  wheel-build path.
- Do not edit generated files in `scripts/` directly unless the task is
  explicitly about the generated output. Change `pybuild.py` or
  `pybuild.toml`, regenerate the scripts, and validate the affected
  workflow path instead.
- When changing build, packaging, or release behaviour, validate the
  complete affected slice: repository-root wheel build, wheel install,
  draft-release artifact staging when relevant, the relevant unit or
  functional tests, and any remaining legacy-script or vendored-program
  paths that the change still touches.
- Keep `.github/workflows/build-debug.yml` and
  `.github/workflows/build-release.yml` aligned when a change is meant
  to affect both debug and release pipelines.
- If release automation is split across multiple workflows, keep the
  contract between them explicit: `build-release.yml` produces validated
  exact-version wheel and sdist artifacts on `master` and stages them on the
  draft release, `release-notes.yml` maintains the draft release metadata and
  suggested tag without pushing the remote tag, and `pypi-publish.yml`
  consumes the published release assets.
- When changing package metadata, wheel naming, Python-version support,
  or release versioning, update all linked sources of truth together,
  including `pyproject.toml`, workflow assumptions, and release-facing
  documentation.
- Package versions are managed from Git tags via `versioningit`; do not
  hardcode a release version in source files or build helpers unless the
  task explicitly requires it.

## Code Style

- Use snake_case for functions and variables, PascalCase for classes,
  and UPPER_SNAKE_CASE for constants.
- Prefer flat over nested, explicit over clever.
- Write straightforward code; do not add defensive checks for unlikely
  edge cases.
- Prefer composition over deep inheritance.
- One class per file when the class is substantial; group small related
  classes.
- Avoid `**kwargs`; use explicit keyword arguments for clarity,
  autocomplete, and typo detection.

## Changes

- The project is in beta; do not keep legacy code or add deprecation
  warnings. Instead, update tests and tutorials to follow the current
  API.
- Minimal diffs: don't rewrite working code just to reformat it.
- Never remove or replace existing functionality as part of a new change
  without explicit confirmation. If a refactor would drop features,
  options, or configurations, highlight every removal and wait for
  approval.
- Fix only what's asked; flag adjacent issues as comments, don't fix
  them silently.
- Don't add new features or refactor existing code unless explicitly
  asked.
- Do not remove TODOs or comments unless the change fully resolves them.
- When renaming, grep the entire project (code, tests, tutorials, docs).
- Every change should be atomic and self-contained, small enough to be
  described by a single commit message. Make one change, suggest the
  commit message, then stop and wait for confirmation before starting
  the next change.
- When in doubt, ask for clarification before making changes.
- Suggest a concise commit message (as a code block) after each change
  (less than 72 characters, imperative mood, without prefixing with the
  type of change).

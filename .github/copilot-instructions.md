# Copilot Instructions

## Project Context

- Python bindings for the CrysFML2008 crystallographic library
- This project must be developed to be as error-free as possible, with
  the same rigour applied to critical software (e.g. nuclear-plant
  control systems).

## Build And Release Workflow

- Treat [.github/workflows/release.yml] as the canonical end-to-end build
  pipeline for release-mode wheel builds and release staging in this
  repository.
- `release.yml` is used to build pycrysfml with the release-mode options
  from `pybuild.toml` and upload all built wheels to GitHub so they can
  be downloaded for local validation.
- The intended release flow is staged: merging `develop` into `master`
  via pull request should produce a draft GitHub release and attach all
  built wheels there for review and local testing.
- Publishing that draft GitHub release should be treated as the event
  that triggers the separate PyPI publication action. Do not collapse
  wheel-building, draft-release staging, and PyPI publishing into one
  opaque workflow unless explicitly requested.
- Treat `pybuild.py` and `pybuild.toml` as the source of truth for the
  generated shell scripts in `scripts/`.
- Do not edit generated files in `scripts/` directly unless the task is
  explicitly about the generated output. Change `pybuild.py` or
  `pybuild.toml`, regenerate the scripts, and validate the affected
  workflow path instead.
- When changing build, packaging, or release behaviour, validate the
  complete affected slice: CFML checkout, CFML build, pyCFML generation,
  wheel build, wheel install, draft-release artifact staging when
  relevant, and the relevant unit or functional tests.
- Keep `.github/workflows/debug.yml` and `.github/workflows/release.yml`
  aligned when a change is meant to affect both debug and release
  pipelines.
- If release automation is split across multiple workflows, keep the
  contract between them explicit: the build workflow produces validated
  wheel artifacts and the publish workflow consumes an intentionally
  published GitHub release.
- When changing package metadata, wheel naming, Python-version support,
  or release versioning, update all linked sources of truth together,
  including `pyproject.toml`, workflow assumptions, and release-facing
  documentation.

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

# Proper Build Transition Review

Review date: 2026-05-01
Reviewed branch: `sdist` into `develop`

## Release Readiness Summary

No serious stopping issues were found for opening or merging the current branch into `develop`, assuming the reported GitHub Actions release-build workflow remains green.

The branch replaces the legacy script-generated build flow with repo-owned CMake, scikit-build-core, cibuildwheel, Pixi maintainer tasks, repaired wheel validation, and sdist rebuild validation. The remaining items below are follow-up risks or cleanup opportunities, not blockers for the current `develop` PR.

## Follow-Up Issues

### Coordinate release draft ownership between workflows

`build-release.yml` computes the suggested release tag by running the same drafterino configuration used by `release-notes.yml`. This appears to work, but it leaves two workflows carrying the same release-tag policy. Keep the duplicated configuration aligned or later replace it with a single shared tag-computation helper so `release-notes.yml` and `build-release.yml` cannot drift.

### Add release asset completeness checks before PyPI publish

`pypi-publish.yml` downloads `*.whl` and `*.tar.gz` assets from the published GitHub release and publishes whatever is present. Add a pre-publish check that the release contains the expected wheel matrix plus exactly one sdist before invoking the PyPI publishing action.

### Tighten GitHub Actions Node version usage

Several workflows still use JavaScript actions that may emit Node.js 20 deprecation warnings on current GitHub runners. `pypi-publish.yml` opts into Node.js 24 globally, but the other workflows should be reviewed and updated once the referenced actions officially support Node.js 24.

### Decide whether CI wheel tests should use a leaner environment

The `pycfml-test` Pixi task defaults to the `wheeltest` environment locally, but CI invokes it inside versioned `ci-py*` environments. This is acceptable for release validation, but a future cleanup could introduce dedicated versioned wheel-test environments if stricter installed-wheel isolation is desired.

### Reduce noisy downstream failures when build artifacts are missing

The release workflow uses follow-up test jobs after build jobs. If a build job fails, the matching artifact download in the test job can also fail and add noise. Consider whether to keep this explicit failure shape or make the test jobs skip when the relevant build job did not succeed.

### Clean whitespace in newly vendored upstream helper files

`git diff --check develop...HEAD` reports trailing whitespace in several newly added vendored upstream files under `repo/CFML/Scripts/PythonAPI` and `repo/CFML/Testing`. This does not affect the package build and is not currently a CI blocker, but it should be cleaned during a future vendored-source hygiene pass if the project wants whitespace-clean diffs.

### Revisit source-distribution contents after the release path is stable

The sdist currently succeeds and is validated, but it likely carries more vendored maintenance material than end users need. After the release path is stable, decide whether helper-only vendoring files, issue notes, or maintainer-only diagnostics should remain part of the source distribution or be excluded explicitly.

### Consider moving `dist/CFML` maintainer staging under the build tree

The vendored CFML staging target writes convenience outputs under `dist/CFML`. This is intentional today, but a future cleanup could move those outputs under the CMake build tree if the project wants to keep source-tree maintainer artifacts separate from release artifact directories.

### Keep optional compiler and platform expansion separate

The transition intentionally supports GNU Fortran first. Future `ifx`, `nagfor`, universal2 macOS, additional Linux architectures, or custom manylinux image work should remain separate follow-up changes with their own validation matrix.

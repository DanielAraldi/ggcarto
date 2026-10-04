---
name: cran-readiness
description: 'Best practices for R package development and preparing ggcarto for CRAN. Use when implementing or reviewing functions, tests, examples, roxygen2 documentation, dependencies or graphics export; preparing a submission, running R CMD check --as-cran, investigating ERROR/WARNING/NOTE messages or responding to reviewers. Also use for goodpractice audits, covr coverage, lintr findings, cyclomatic complexity, check-group selection and optional tidyverse style checks. Includes pre-submission, noSuggests and extrachecks guidance.'
argument-hint: 'development <change> | audit [goodpractice] | pre-submission | resubmission <feedback>'
---

# Development and CRAN Readiness

Keep ggcarto verifiable throughout development and produce readiness evidence
before submission. Passing local checks does not guarantee CRAN acceptance.
Respond in the user's language; keep public package documentation in English
and comments intended for CRAN in English, using plain ASCII text.

## Sources and Precedence

1. [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
   and the [official checklist](https://cran.r-project.org/web/packages/submission_checklist.html):
   binding requirements and official guidance.
2. [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html):
   the technical specification for R packages.
3. [R Packages, Hadley Wickham and Jennifer Bryan](https://github.com/hadley/r-pkgs),
   especially [Releasing to CRAN](https://r-pkgs.org/release.html): best practices.
4. [ThinkR prepare-for-cran](https://github.com/ThinkR-open/prepare-for-cran):
   a supplementary practical checklist.
5. [Davis Vaughan extrachecks](https://github.com/DavisVaughan/extrachecks):
   reports of additional issues found during human review.

Sources reviewed on 2026-10-03. Revisit official sources before each submission
or whenever a requirement is unclear. If they are unavailable, record that
limitation. The current official policy takes precedence in any conflict.
Do not turn historical reports into universal rules. `extrachecks` is a list
of cases, not an R package: do not invent `extrachecks::check()` or install it.

For automated package-quality assessment, load the
[goodpractice workflow](./references/goodpractice.md), adapted from the supplied
`goodpractice.Rmd` guide. It complements the CRAN policy and checklist rather
than replacing them. Coverage, complexity and style findings are advisory unless
the project explicitly adopts them as gates. Confirm the installed tool's API
before using version-dependent helpers described by the guide.

## 1. Choose the Scope

| Mode           | Action and boundary                                                                                    |
| -------------- | ------------------------------------------------------------------------------------------------------ |
| Development    | Implement the requested change with local tests and a review of affected items.                        |
| Audit          | Only inspect and run local checks; report findings without modifying files.                            |
| Pre-submission | Run the full checklist, fix local issues, rerun affected checks and prepare evidence and comments.     |
| Resubmission   | Map each CRAN request to a fix and supporting evidence, then rerun affected gates and the final check. |

In pre-submission mode, fix local implementation, testing, documentation and
packaging issues without further confirmation while preserving the public
contract. Ask for confirmation before changing the public API, authorship,
license or dependencies. Do not expand the task to unrelated refactoring or
functional changes. This mode's autonomy does not apply to audit mode.

If the request only invokes the skill, start in audit mode. Ask only about
information needed to proceed: initial submission or update, candidate version,
reviewer feedback or required environment. Do not infer publication from the
version number. Do not commit, push, change licensing or authorship, or publish
without an explicit request. Never invent names, emails, ORCIDs, copyright
ownership or results.

Installing tools, changing CI and uploading to win-builder, macbuilder, R-hub
or CRAN require specific authorization. Reading public documentation does not
authorize uploading source code. Never ask for passwords or tokens in chat.

## 2. Establish the Current State

1. Read `DESCRIPTION`, `.Rbuildignore`, `.gitignore`, applicable instructions
   and the test or example closest to the change. Note existing modifications.
2. Before implementing, read `docs/00-ASTRA_INSTRUCTIONS.md` if available.
   `docs/` contains local material excluded from the build; checks must not
   depend on it.
3. Confirm R and dependency versions, available tools and existing CI commands.
   Do not add development tools to Imports.
4. Treat the current code as the source of truth, not historical export or test
   counts or results in `artifacts/`.

In ggcarto, preserve the package's own scene graph, layout resolution for the
current device, scene immutability and isolated styles. Do not introduce
patchwork as the engine. Prefer public grid, ggplot2 and gtable APIs; isolate
version-sensitive adapters. Check the minimum R version in `Depends` before
using new syntax or APIs. Cartography and codecs must remain optional as
declared in `Suggests`.

## 3. Develop with Short Feedback Loops

1. Form a local hypothesis and choose the smallest test that can disprove it.
2. Make only necessary changes and add a regression test to the appropriate
   existing file in `tests/testthat/`. Reuse `helper-graphics.R` and fixtures.
3. Run the affected test immediately; fix the cause before broadening scope.
   Run this example from the root, replacing the filter with the affected area:

   ```sh
   NOT_CRAN=true Rscript --vanilla -e 'testthat::test_local(filter = "lengths", reporter = "summary", stop_on_failure = TRUE)'
   ```

4. For API or documentation changes, edit roxygen in `R/*.R`, including
   parameters, return values, side effects and executable examples. Regenerate
   rather than manually editing `NAMESPACE` or `man/*.Rd`:

   ```sh
   Rscript --vanilla -e 'roxygen2::roxygenise(".")'
   ```

5. Review the generated diff and rerun affected tests. Run the full suite for
   changes to shared behavior and before preparing a release:

   ```sh
   NOT_CRAN=true Rscript --vanilla -e 'testthat::test_local(reporter = "summary", stop_on_failure = TRUE)'
   ```

6. For rendering or export changes, check geometry, nonblank content,
   dimensions, formats and device preservation. Inspect visual differences
   before accepting snapshots; never accept baselines automatically.
7. Apply relevant items from the [checklist](./references/checklist.md).
   Run `R CMD check` regularly, especially after changing the API, metadata,
   documentation or dependencies. A small fix does not require remote checks.
8. When reviewing package quality, coverage, complexity or lint, use the
   [goodpractice workflow](./references/goodpractice.md). Select a focused check
   set for a local change and a full baseline for an explicit package-wide audit.
   Do not rerun expensive preparation once per finding or enable extra style
   conventions without agreement.

`NOT_CRAN=true` enables development checks, including visual checks; it does
not provide evidence of behavior on CRAN. Do not add skips, tolerances,
warning suppression or `globalVariables()` merely to hide defects.

## 4. Validate the Release Candidate

Read the entire [pre-submission checklist](./references/checklist.md).
Check metadata and tools before starting expensive checks. If a blocker
requires human input, such as a placeholder maintainer, report it and continue
only with independent checks.

Build with current R-release or R-patched. Check the exact tarball with current
R-devel; if unavailable, use release/patched and document the exception allowed
by the policy. Confirm which `R` executable is being used.

Local example for macOS/Linux, starting at the repository root. Use a new
temporary directory to avoid overwriting existing tarballs and check outputs:

```bash
repo_dir="$PWD"
check_dir="$(mktemp -d)"
package_name="$(Rscript --vanilla -e 'cat(read.dcf("DESCRIPTION")[1, "Package"])')"
package_version="$(Rscript --vanilla -e 'cat(read.dcf("DESCRIPTION")[1, "Version"])')"
pushd "$check_dir" &&
  env -u NOT_CRAN -u _R_CHECK_FORCE_SUGGESTS_ R CMD build "$repo_dir" &&
  env -u NOT_CRAN -u _R_CHECK_FORCE_SUGGESTS_ R CMD check --as-cran "${package_name}_${package_version}.tar.gz"
```

Record the exit code immediately, read `00check.log` and the example/test logs,
preserve the artifacts and return with `popd`. Inspect the tarball contents,
not just the working directory. Record its path and checksum alongside the
results. Subsequent changes require a new build and check.

The final gate includes the PDF manual and vignettes, when present.
`--no-manual`, `--no-build-vignettes` or checks without examples/tests are
partial checks, not silent substitutes. If LaTeX or another tool is missing,
record the pending gate and request installation or use an authorized suitable
environment.

Do not disable incoming or URL checks, change the clock or use permissive local
configuration to obtain a clean result. Classify network failures with evidence
and recheck them rather than hiding them.

## 5. Report and Conclude

For each completed item, record its status (`PASS`, `FAIL`, `PENDING` or `N/A`),
command or inspection, versions and platform, evidence, rationale and next
action. `N/A` requires justification; a missing tool, skip or unexecuted check
is not `PASS`. Distinguish CRAN requirements, community recommendations and
project rules.

Report blockers first, with concrete paths and messages, followed by a summary
of tests and gaps. In pre-submission mode, prepare `cran-comments.md` with the
environments actually tested, real ERROR/WARNING/NOTE counts, an explanation
for each note and reverse dependency results. Exclude this file from the
tarball through `.Rbuildignore`. Never fill in a template as though its checks
had passed.

Local readiness requires zero ERRORs, zero WARNINGs and no unresolved
significant NOTEs. Aim for zero NOTEs, but explain legitimate ones, such as
`New submission`. A justification is not CRAN approval; exceptions for
warnings or notes depend on the repository maintainers' assessment.

Conclude with `blocked`, `verification incomplete` or `ready for maintainer
review`, always stating exceptions. Only CRAN determines acceptance.
Never run `devtools::release()`, `devtools::submit_cran()` or upload the
tarball automatically as a consequence of passing checks.

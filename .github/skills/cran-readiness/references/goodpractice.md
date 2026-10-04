# Automated Package Quality with goodpractice

Adapted from the user-supplied `goodpractice.Rmd` at the repository root,
reviewed on 2026-10-03. This reference is self-contained if that guide is later
moved. Its examples demonstrate goodpractice's `bad1` sample package: assess
ggcarto instead. Do not render the supplied vignette or add vignette dependencies
to ggcarto merely to follow this workflow.

Follow the parent skill's scope and permissions. An audit reports findings
without editing package source. Fixes require development/pre-submission scope
or an explicit request. No submission, publication, push or remote checking
upload is part of this workflow.

## 1. Establish Tool Capabilities

1. Check whether goodpractice and the tools needed by the selected checks are
   installed. Request authorization for installations; keep audit tools out of
   ggcarto's Imports/Suggests unless the package itself actually needs them.
2. Record R, goodpractice and relevant dependency versions. Inspect exported
   functions, their arguments and installed help before constructing commands.
   The supplied guide may describe a different release than the installed tool.
3. Do not hard-code a check count, helper availability or a tool's default
   thresholds as permanent facts. Query the installed version.

```r
stopifnot(requireNamespace("goodpractice", quietly = TRUE))
utils::packageVersion("goodpractice")
exports <- getNamespaceExports("goodpractice")
args(goodpractice::gp)
available_checks <- goodpractice::all_checks()
grep("url|coverage|complex|namespace", available_checks, value = TRUE)
```

The guide describes `default_checks()`, `tidyverse_checks()`, `all_checks()`,
`all_check_groups()`, `checks_by_group()`, `describe_check_groups()`, `checks()`,
`failed_checks()`, `results()` and `export_json()`. Guard version-dependent
helpers with membership in `exports`. If a helper is unavailable, use documented
explicit check names supported by that version or record a tooling limitation;
do not fabricate a replacement API or silently drop requested checks.

## 2. Select the Smallest Useful Run

| Request                              | Selection                                                                                    |
| ------------------------------------ | -------------------------------------------------------------------------------------------- |
| Full package-quality audit           | Installed default check set; record all effective exclusions.                                |
| Metadata or namespace review         | Matching explicit check names, or groups when supported.                                     |
| Fast local feedback                  | Exclude expensive coverage/check preparation only if supported and report the reduced scope. |
| Coverage or complexity investigation | Select relevant supported checks and inspect detailed file/function evidence.                |
| Tidyverse style review               | Opt in explicitly; do not impose it through a general CRAN audit.                            |

goodpractice first gathers data through preparation steps, then evaluates checks
against those results. Coverage, lint and `R CMD check` preparation can be
expensive, while many individual checks reuse the same data. Run the chosen set
once, inspect its result object, and rerun only after a relevant change. A prior
standalone check cannot be claimed as a cached goodpractice preparation unless
the installed API actually supports that reuse.

After capability discovery, an explicit metadata selection can look like:

```r
stopifnot("description_url" %in% available_checks)
result <- goodpractice::gp(package_dir, checks = "description_url")
```

Group selection is available only when the installed version exports the helper:

```r
stopifnot("checks_by_group" %in% exports)
selected_checks <- goodpractice::checks_by_group("description", "namespace")
stopifnot(length(selected_checks) > 0L,
  all(selected_checks %in% available_checks))
result <- goodpractice::gp(package_dir, checks = selected_checks)
```

Confirm group names with `all_check_groups()` and descriptions with
`describe_check_groups()` when available. Do not assume a group name and an
individual check name are interchangeable.

## 3. Control Exclusions and Execution

Before running, inspect the existing `.lintr` configuration and record options
and environment variables that affect the analysis. In versions implementing
the supplied guide's behavior:

- `goodpractice.exclude_check_groups` selects groups to omit from a default run;
  `GP_EXCLUDE_CHECK_GROUPS` is its comma-separated environment counterpart.
  The R option takes precedence over the environment variable.
- Explicit `checks = ...` selections bypass group-exclusion settings. Therefore
  `checks = default_checks()` need not behave like omitting `checks`.
- `goodpractice.exclude_path` and `GP_EXCLUDE_PATH` exclude paths relative to
  the package root from supported source analyses. They do not imply exclusion
  from coverage or `R CMD check`.
- Existing `.lintr` settings remain authoritative for enabled linters and
  exclusions; do not overwrite them to force a clean result.

Verify these semantics in the installed help. If unsupported, use an explicit
supported selection and report exactly what ran. Never exclude a failing file
or category solely to hide a defect. Generated/vendored exclusions need a clear
reason and must not silently include maintained source.

Run full analysis in a disposable working copy when preparations may write
files or update snapshots. Retain relevant source, tests and audit configuration,
record the source revision and dirty changes, and keep outputs outside the
package tree or in an existing ignored artifacts directory. Set `package_dir`
to that copy; never accidentally analyze the tool's sample package or a stale
installed ggcarto. Start from a clean R session with the required tools on PATH.

Keep preparation sequential by default. Parallel execution is optional and
requires compatible goodpractice, future and future.apply versions. If enabled,
use explicit workers, budget nested package/BLAS threads and stay within the
parent skill's resource limits. Restore the previous future plan, options and
environment afterward. Do not enable parallelism merely because the guide
demonstrates `future::plan("multisession")`.

Do not wrap `gp()` in `suppressWarnings()` as the sample demonstration does:
preparation failures and warnings are evidence. Inspect whether its internal
`R CMD check` includes the manual, vignettes and the intended CRAN settings.
A default goodpractice run is not a substitute for the parent's exact-tarball
`R CMD check --as-cran` gate or cross-platform/noSuggests checks.

## 4. Interpret the Result Object

Inspect the existing result instead of rerunning analysis just to display more
findings. When supported by the installed version:

```r
stopifnot(all(c("checks", "failed_checks", "results") %in% exports))
executed_checks <- goodpractice::checks(result)
failed_checks <- goodpractice::failed_checks(result)
check_results <- goodpractice::results(result)
stopifnot("passed" %in% names(check_results))
summary <- c(
  passed = sum(check_results$passed %in% TRUE),
  failed = sum(check_results$passed %in% FALSE),
  not_evaluated = sum(is.na(check_results$passed))
)
summary
```

`TRUE` is a pass, `FALSE` a failure and `NA` not evaluated, never a pass.
Compare selected checks with returned checks as well: excluded checks may be
absent rather than represented by `NA`. Inspect preparation errors before
interpreting missing coverage or lint results. A successfully returned object
or empty failure list does not prove all requested checks ran.

Use `print(result, positions_limit = Inf)` only if that print method supports
the argument. Otherwise use supported accessors to inspect all locations.
For machine-readable evidence, use `export_json()` only when exported and its
signature is confirmed. Otherwise preserve the result with base R `saveRDS()`
and the result table with `write.csv()`; do not invent a JSON schema.

## 5. Triage and Fix Proportionately

Prioritize behavioral failures and CRAN requirements, then metadata or
documentation defects, meaningful coverage gaps, complexity and style advice.
For each finding, record the check name/group, location, evidence, impact and
whether it is required, advisory or not applicable with justification.

- Coverage: inspect untested branches and files, not just the overall percentage.
  Check whether absent codecs/sf or CRAN skips excluded relevant paths. Add
  behavior-focused tests using existing fixtures; do not test implementation
  details only to raise a score. No new global coverage target is implied.
- Complexity: inspect high-scoring functions and their responsibility. Extract
  a helper only when it clarifies behavior or removes meaningful duplication;
  do not broadly refactor the layout engine to satisfy a numeric threshold.
- Lint: distinguish actual correctness risks from style preferences. Preserve
  public names and repository conventions. A style finding is not automatically
  a CRAN rejection or permission to reformat every source file.
- Metadata: verify real project URLs before proposing URL/BugReports values;
  do not invent a repository, issue tracker, author or contact address.
- Documentation: fix roxygen sources and regenerate help when needed. Keep
  exported return values, examples and optional-dependency guards complete.

In an authorized fixing mode, take one coherent finding at a time, add or reuse
a focused regression check, make the smallest change, and run that check
immediately. Then rerun affected goodpractice checks; run the full selected
baseline at the end of a package-wide remediation. Preserve reviewed visual
baselines and apply the parent skill's build and test requirements.

Custom checks are optional. Create them only for agreed, repeated conventions,
with explicit preparation dependencies, a deterministic verdict and actionable
messages. Confirm the installed extension API and consult its `custom_checks`
vignette. Test each custom check on a passing and a failing fixture before
treating it as a gate; avoid new abstractions for one-off findings.

## 6. Deliver Evidence

Report tool/environment versions, analyzed source, selected checks, exclusions,
preparation failures, pass/fail/not-evaluated counts and findings by priority.
State unresolved or waived items with reasons and the next concrete action.
Record artifact paths and mark the audit partial if any requested group could
not run. For CI, fail on agreed required failures and unavailable required
preparations, not merely on console formatting; advisory findings remain visible.

Reaching zero reported failures is neither proof of full coverage nor CRAN
approval. Do not overwrite the parent's release-check evidence with a broader
claim than this run supports.

# Development and Pre-submission Checklist

Use alongside `SKILL.md`. Support each checked item with evidence; empty boxes
do not indicate approval. Labels distinguish official policy and guidance
(`CRAN`), best practices (`BP`) and project-specific conventions (`GGCARTO`).

## Metadata, Rights and Dependencies

- [ ] **CRAN:** For an initial submission, check for case-insensitive name
      conflicts in current CRAN packages, the Archive and Bioconductor. Explain
      the package's nontrivial contribution and usefulness.
- [ ] **CRAN:** `Authors@R` identifies authors and a single human maintainer with
      the `cre` role and a real, stable, reachable email address. Placeholders
      such as `ggcarto@example.org` block submission; request details, never invent them.
- [ ] **CRAN/BP:** Verify copyright and `aut`, `ctb`, `cph` roles against actual
      rights. Do not assign `cph` indiscriminately or invent ORCID/ROR identifiers.
- [ ] **CRAN:** `License` is compatible with code, data, maps, images, fonts and
      third-party material, preserving attribution and distribution rights.
      Check whether LICENSE/COPYRIGHTS files are needed for the license; do not
      add an unnecessary license template or change GPL-3 automatically.
- [ ] **CRAN/BP:** Keep `Title` concise, in title case, without redundancy such
      as "for R"; write `Description` as an informative paragraph that does not
      start with the package name or "This package". Proofread the English and
      expand relevant abbreviations.
- [ ] **CRAN:** Single-quote package/software names with correct capitalization,
      write functions as `foo()` without quotes and provide real author-year
      references with `<doi:...>`, ISBN or URL. For arXiv, follow the current
      official checklist, which recommends DOI `10.48550/arXiv.ID`, rather than
      blindly copying older formats.
- [ ] **CRAN/BP:** Ensure the version matches the candidate and exceeds the
      published version for updates. `URL` and `BugReports`, when provided, must
      exist. README and NEWS must not claim CRAN availability before publication.
- [ ] **CRAN:** Declare strong dependencies, use current versions available on
      CRAN/Bioconductor and avoid orphaned packages as strong dependencies.
      Do not rely on `Remotes`/GitHub or development versions to install the
      submission candidate. Check additional repositories for optional
      dependencies where applicable.
- [ ] **BP/GGCARTO:** Declare directly used dependencies; do not rely on accidental
      transitive imports. `ggplot2`, `grid` and `gtable` support the core; confirm
      the current list in DESCRIPTION. Tools such as roxygen2, devtools, covr,
      spelling and urlchecker must not become Imports just to run this skill.

## API, Documentation and Examples

- [ ] **CRAN/GGCARTO:** Document exports and S3 methods, signatures, parameters,
      return values (class, structure, units and visibility) and side effects.
      `@keywords internal` does not exempt an export from documentation. Generate
      S3 registrations and Rd files from roxygen and check consistency with code.
- [ ] **BP:** For genuinely internal helpers, use `@noRd` where appropriate.
      Do not export helpers merely to fix examples. Examples for an internal
      topic need valid access in the installed package; this is not general
      permission to use internal APIs from other libraries.
- [ ] **CRAN/BP:** Include small, useful, executable examples for the public API;
      justify genuine exceptions. Avoid fully commented-out examples or
      `if (FALSE)`. Use `try()` when intentionally demonstrating an error.
- [ ] **CRAN:** Examples run in a few seconds each, without credentials,
      mandatory network access, interactive input or personal files. Prefer
      small local fixtures; export examples use `tempfile()`/`tempdir()`.
- [ ] **CRAN/BP:** Guard optional features with
      `requireNamespace("sf", quietly = TRUE)` or an equivalent `@examplesIf`.
      Use `interactive()` only for genuine interaction. `\dontrun{}` requires a
      real reason the example cannot run; `\donttest{}` may be executed and must
      not hide failures.
- [ ] **BP/GGCARTO:** Validate examples in `man/`, relevant scripts in
      `inst/examples/functions/` and other affected examples. Do not treat local
      guides in `docs/` as substitutes for help distributed with the package.
- [ ] **CRAN/BP:** Check URLs, redirects, anchors, HTML/Rd and spelling.
      Prefer HTTPS and canonical URLs, such as
      `https://CRAN.R-project.org/package=ggplot2`. Relative links must resolve
      in the distributed artifact; they are not universally prohibited. A link
      to `docs/` excluded from the tarball may fail even if it works on GitHub.
- [ ] **BP:** When available, run `urlchecker::url_check()` and
      `spelling::spell_check_package()`. Review findings before fixing them; do
      not run `url_update()` or automatically rewrite metadata without inspection.

## Safe Execution and Portability

- [ ] **CRAN:** Installation, loading, examples and tests do not write to the
      home directory, installed package, workspace or hard-coded paths. Use
      temporary files and clean up only what the run created. Persistent data
      or caches require justification under policy exceptions, such as
      `tools::R_user_dir()` with small sizes and active cleanup; do not introduce
      unnecessary caches.
- [ ] **GGCARTO:** `gc_save()` writes only when explicitly called by the user,
      respects the destination and overwrite setting, and preserves the caller's
      scene and device. During checks, always provide a temporary destination;
      never rely on the default `dir = "."`.
- [ ] **CRAN/BP:** Restore options, working directory, environment variables,
      par and random state when temporarily changed, using `on.exit()` or a local
      helper. Close only devices/connections opened by the operation; do not use
      `graphics.off()` to clean up the user's devices.
- [ ] **CRAN:** Do not use `.GlobalEnv`, `<<-`, `q()` or namespace modifications
      for inappropriate global side effects. Do not install packages in
      `.onLoad()`, examples or tests, or send telemetry/session information
      without consent.
- [ ] **CRAN:** Do not launch browsers, viewers or external programs in checks
      without closing the instance created. Avoid network access; when needed,
      use HTTPS, timeouts and informative handling of unavailable resources,
      without check errors/warnings caused by missing resources. Do not disable
      TLS or bypass rate limits.
- [ ] **CRAN:** Use public R APIs, not `.Internal()` or access to base internals
      through `:::`. Do not tamper with diagnostics or security mechanisms.
      If compiled code is added, apply the portability requirements and
      specialized checks from Writing R Extensions.
- [ ] **CRAN/BP:** Limit package checks to at most two simultaneous cores/threads
      and keep their cost low; do not invent a universal total runtime limit.
      Measure tests, examples and vignettes while preserving meaningful coverage.
- [ ] **BP/GGCARTO:** Validate paths with spaces, case sensitivity, locales, UTF-8,
      fonts and headless devices. Use `file.path()` and portable fixtures. Test
      the declared minimum R version and supported ggplot2 versions.

## Graphics Tests and Optional Dependencies

- [ ] **GGCARTO:** Test calculations, layout and grob structure, not just images.
      Cover resizing, units, style isolation, immutability and device preservation.
      For cartography, check CRS, scales and north orientation.
- [ ] **BP/GGCARTO:** Keep snapshots deterministic, avoiding dependence on
      platform-exclusive fonts, network access, automatic names or incidental
      ordering. Investigate rendering differences before changing baselines
      or tolerances.
- [ ] **CRAN/BP:** Guard `sf`, codecs and other Suggests in code, examples,
      helpers and tests. Use `skip_if_not_installed()` where appropriate; also
      check transitive suggested dependencies used by calls. Optional APIs must
      fail informatively when called without their dependency, without breaking
      the core.
- [ ] **BP:** Check the bootstrap in `tests/testthat.R` when `testthat` is absent.
      Do not assume skips inside tests protect the test runner from load failures.
- [ ] **BP:** Run a noSuggests check in an isolated library/environment containing
      only strong dependencies and their transitive closure. Confirm the packages
      actually visible in `.libPaths()`; do not remove packages from the user's
      library. Run `R CMD check --as-cran` with `_R_CHECK_FORCE_SUGGESTS_=false`
      and without `NOT_CRAN=true`, recording the result separately.
- [ ] **BP:** The `_R_CHECK_FORCE_SUGGESTS_=false` flag alone does not simulate
      the absence of installed Suggests. The isolated test supplements, rather
      than replaces, the full check with optional dependencies. If isolation is
      unavailable, mark PENDING.
- [ ] **GGCARTO:** Also run the full suite with `NOT_CRAN=true` to exercise visual
      tests outside CRAN. Account for expected skips, and ensure the core remains
      tested on the CRAN execution path without relying on skipped snapshots.

## Artifact and Validation Matrix

- [ ] **CRAN/BP:** Build a source tarball with `R CMD build`; inspect its contents,
      size, installation and `R CMD check --as-cran` results for that same file.
      Do not submit a zip, binary package or development directory. Do not include
      binary executables.
- [ ] **BP/GGCARTO:** Check `.Rbuildignore` and the actual tarball to exclude
      `.github/`, `docs/`, `artifacts/`, `.Rcheck`, old tarballs, caches,
      credentials and session files. `.gitignore` does not control `R CMD build`.
      Do not exclude required tests/documentation merely to eliminate warnings.
- [ ] **CRAN:** Minimize size: as a general rule, the policy specifies up to
      5 MB for data and for documentation, and prefers tarballs of up to 10 MB
      when possible. Compress content and justify exceptions; do not treat
      preferences as rigid absolute limits. Retain required component sources
      and licenses.
- [ ] **CRAN/BP:** Record R-release/R-patched for the build and R-devel for the
      final check, or explain the latter's unavailability as allowed by policy.
      Check Windows, macOS and Linux as a project goal; the policy normally
      expects portability to at least two major platforms, not just the local one.
- [ ] **BP:** Use existing CI, win-builder, macbuilder or R-hub only with
      authorization for uploads/configuration. Consult current APIs, especially
      R-hub v2, rather than copying obsolete commands. Wait for logs; uploading
      does not mean passing.
- [ ] **CRAN/BP:** For updates with reverse dependencies, compare the published
      version and the candidate, separating preexisting failures from regressions.
      Use revdepcheck or official tools as available. If there are no reverse
      dependencies, record how this was verified; do not assume it from a 0.x version.

## Communication, Submission and Maintenance

- [ ] **CRAN/BP:** Resolve ERRORs, WARNINGs and significant NOTEs; justify each
      remaining note, including initial submission. Never mistake missing logs
      or an old status in `artifacts/` for a clean check.
- [ ] **BP:** Update NEWS and prepare a concise `cran-comments.md` with
      environments, real counts, notes and reverse dependency results. Keep it
      under version control but out of the tarball.
- [ ] **CRAN:** For API changes that break other packages, notify maintainers
      at least two weeks in advance and record the impact and communication.
      Significant disruption requires prior agreement with CRAN. Do not send
      emails without a request.
- [ ] **CRAN:** Resubmit using the form and comments explaining how each item
      of feedback was addressed. Updates to published packages require a version
      increase; increasing the version with every attempt is also preferred,
      but do not invent a universal requirement to bump the patch after rejection.
- [ ] **CRAN:** Do not submit again while the previous submission awaits a reply.
      Established packages should normally space updates 1-2 months apart;
      requested fixes and urgent issues require context, not blind waiting.
- [ ] **CRAN/BP:** The maintainer authorizes uploading the validated tarball
      through the official form and confirms the email received. Document
      maintainer, email or license changes and monitor results after publication.

## Human Review Cases

Before concluding, revisit extrachecks and ThinkR in the skill's sources:
redundant titles, insufficient descriptions, missing return documentation or
examples, authorship and copyright, license years where applicable, method
references, unjustified `\dontrun{}`, noSuggests, HTML and links to missing files.
Validate each case against the current policy and the actual package. No
community checklist covers every aspect of human review or guarantees publication.

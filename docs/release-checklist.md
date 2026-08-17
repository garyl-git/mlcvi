# Release checklist for mlcvi

Use for every tagged release (next: 0.4.0). Tick in order.

## Before tagging

- [ ] All ACTION items in PLAN.md resolved or consciously deferred.
- [ ] `docs/decisions-for-sheetal.md` answered; decisions reflected in code
      and documentation (custom-item labelling, wave rule, wave 7).
- [ ] The full training matrix is hosted (OSF) and
      `.mlcvi_train_url_default` in `R/internal.R` points at it; a fresh
      session with an empty `tools::R_user_dir("mlcvi", "cache")` downloads
      it and `mlcvi_extend(scores)` runs with defaults.
- [ ] `DESCRIPTION`: bump `Version` (drop the `.9000`), check `Authors@R`
      (add co-authors/contributors as agreed), `URL` (add the GitHub URL and
      the OSF/DOI landing page), `BugReports` (GitHub issues URL), `Date`
      optional.
- [ ] `NEWS.md`: rename "(development version)" to the version number; every
      user-visible change listed.
- [ ] `inst/CITATION`: version string in `note`; add the JSS article entry
      once accepted (keep the user guide entry).
- [ ] `README.md`: install instructions still correct; examples still run.
- [ ] `devtools::document()` — no diff in `man/` or `NAMESPACE` afterwards.
- [ ] `devtools::test()` — 0 failures, 0 warnings.
- [ ] `devtools::run_examples()` — clean.
- [ ] `devtools::build_vignettes()` — builds; read the HTML once.
- [ ] `R CMD check --as-cran` locally — 0 errors, 0 warnings, 0 notes
      (with LaTeX installed, drop `--no-manual`; otherwise install
      `tinytex::install_tinytex()` first so the PDF manual is checked).
- [ ] `devtools::check_win_devel()` and `rhub::rhub_check()` (or R-universe)
      — clean on Windows and Linux; fix any platform notes.
- [ ] `devtools::spell_check()` — no real misspellings.
- [ ] `urlchecker::url_check()` — all URLs resolve.
- [ ] Secret scan on the tree (`git grep -E "ghp_|github_pat_"` returns
      nothing).
- [ ] Working tree clean; `main` pushed.

## Tagging and publishing

- [ ] `git tag -a v0.4.0 -m "mlcvi 0.4.0"` on the release commit;
      `git push origin v0.4.0`.
- [ ] GitHub release from the tag; paste the NEWS section as release notes;
      attach `devtools::build()` tarball (`mlcvi_0.4.0.tar.gz`) so users
      without build tools can `install.packages(path, repos = NULL)`.
- [ ] Zenodo (or OSF registration) DOI for the tagged release; add the DOI
      badge to README and the DOI to `inst/CITATION`.
- [ ] Update the SocArXiv user guide if the API changed (new functions:
      `mlcvi_get_panel()`, `mlcvi_build_matrix()`).
- [ ] Bump `DESCRIPTION` to `0.4.0.9000` and add a new "(development
      version)" heading in NEWS.md on `main`.

## JSS submission specifics

- [ ] Manuscript code chunks reproduce with the tagged version; record
      `sessionInfo()` in the replication material.
- [ ] Replication script that regenerates every table/figure from the
      package's exported functions and bundled data.
- [ ] Vignette mirrors the manuscript sections.
- [ ] Reference list uses the package citation from `citation("mlcvi")`.
- [ ] Note the data completion procedure (question 5 of the decisions memo)
      in the data section of the paper.

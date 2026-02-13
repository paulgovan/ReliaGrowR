# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**ReliaGrowR** is a CRAN-published R package for Reliability Growth Analysis (RGA). It implements the Duane, Crow-AMSAA, and Piecewise NHPP reliability growth models, plus a REST API interface via plumber.

## Common Commands

All commands assume an R session in the package root directory.

```r
# Install development dependencies
devtools::install_dev_deps()

# Regenerate NAMESPACE and Rd files from roxygen2 comments
devtools::document()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-srr-rga.R")

# Full package check (mirrors CI)
devtools::check()
```

From the shell:
```bash
R CMD check --no-manual --compact-vignettes=gs+qpdf
```

## Architecture

### Core R files (`R/`)

| File | Purpose |
|------|---------|
| `rga.R` | Main `rga()` function — Crow-AMSAA and Piecewise NHPP models; S3 `plot`/`print` methods |
| `duane.R` | `duane()` function — log-log regression of cumulative MTBF vs time; S3 `plot`/`print` methods |
| `rdt.R` | `rdt()` — Reliability Demonstration Test plan calculator; S3 `plot`/`print` methods |
| `weibull_to_rga.R` | `weibull_to_rga()` — converts Weibull failure/suspension/interval data to RGA format |
| `gof.R` | `qqplot.rga()` / `ppplot.rga()` — Q-Q and P-P goodness-of-fit plots for `rga` objects |
| `grwr_api.R` | `grwr_api()` — launches plumber REST API from `inst/plumber/` |

### REST API (`inst/plumber/`)

Plumber endpoint definitions for `rga`, `duane`, and `gof` functions. Launched via `grwr_api()`.

### Key dependencies

- **segmented** — change-point detection in Piecewise NHPP model
- **plumber** — REST API
- **vdiffr** — visual regression testing for plot snapshots (Suggests)
- **ellmer** — listed in Suggests (AI/LLM integration)

## Testing

Tests use **testthat v3** and are in `tests/testthat/`. Each test file corresponds to a source file (e.g., `test-srr-rga.R` tests `rga.R`).

Tests include:
- Unit tests for model parameter estimation
- Parameter recovery checks (noise susceptibility)
- Visual regression tests via **vdiffr** (plot snapshots stored in `tests/testthat/_snaps/`)
- SRR statistical standards compliance tags (`@srrstats`)

When adding or changing plots, run `vdiffr::manage_cases()` to review/update snapshots.

## Documentation

- Docs use **roxygen2** with Markdown enabled. Run `devtools::document()` after editing roxygen2 comments.
- `@srrstats` tags document compliance with SRR statistical standards — include them when adding new exported functions.
- The pkgdown site is built and deployed via `.github/workflows/pkgdown.yaml`.
- README.md is generated from README.Rmd; edit README.Rmd, not README.md.

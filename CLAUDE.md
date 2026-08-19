# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Common Commands

``` r

# Load package for interactive development
devtools::load_all()

# Regenerate documentation (roxygen2 → .Rd files + NAMESPACE)
devtools::document()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-srr-rga.R")

# Full CRAN check
devtools::check()
```

## Architecture

ReliaGrowR implements statistical models for Reliability Growth Analysis
(RGA). Each model is encapsulated in its own file with a consistent
pattern: a constructor function returns an S3 class object, and
`print.*` / `plot.*` methods are defined in the same file.

**Core models and their files:** - `R/rga.R` — Crow-AMSAA (NHPP Power
Law), the primary model. Supports least-squares (default) and MLE
fitting. The `.fit_mle_crow()` internal function handles MLE.
[`predict_rga()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_rga.md)
returns a `rga_predict` S3 class. - `R/nhpp.R` — Piecewise NHPP with
automatic change point detection via the `segmented` package.
[`predict_nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_nhpp.md)
returns a `nhpp_predict` S3 class. - `R/duane.R` — Duane log-log
regression with confidence intervals. - `R/mcf.R` — Mean Cumulative
Function using the Nelson-Aalen estimator (repairable systems). -
`R/exposure.R` — Exposure-based NHPP model. - `R/rdt.R` — Reliability
Demonstration Test planning.

**Supporting files:** - `R/sim_failures.R` — Simulates failure data from
conditional Weibull models. - `R/weibull_to_rga.R` — Converts
Weibull-format data to RGA input format. - `R/gof.R` — Goodness-of-fit:
[`ppplot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/ppplot.rga.md)
and
[`qqplot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/qqplot.rga.md).

## Testing Conventions

Tests use the `testthat` (edition 3) framework and are named
`test-srr-*.R` to indicate SRR (Software Review for Reliability)
compliance. The SRR tags in test and source files (from
`R/srr-stats-standards.R`) are part of rOpenSci’s statistical software
review process — do not remove them.

Visual regression tests use `vdiffr`; snapshots live in
`tests/testthat/_snaps/`. Run `vdiffr::manage_cases()` to review
snapshot diffs interactively.

## Documentation

All user-facing functions are documented with roxygen2. After editing
roxygen comments, run `devtools::document()` — never edit `man/*.Rd` or
`NAMESPACE` directly.

Vignettes are in `vignettes/` as `.Rmd` files. The pkgdown site config
is in `_pkgdown.yml`.

# Changelog

## ReliaGrowR 0.6

CRAN release: 2026-04-16

### New Features

- New
  [`nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/nhpp.md)
  function for fitting parametric Non-Homogeneous Poisson Process (NHPP)
  models (Power Law and Log-Linear) to recurrent event data from
  repairable systems, including piecewise/segmented support with
  automatic change point detection.
- New [`mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/mcf.md)
  function for non-parametric Mean Cumulative Function estimation using
  the Nelson-Aalen estimator.
- New
  [`exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/exposure.md)
  function for computing system exposure (total operating time at risk)
  across repairable systems.
- New
  [`overlay_rga()`](https://paulgovan.github.io/ReliaGrowR/reference/overlay_rga.md)
  and
  [`overlay_nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/overlay_nhpp.md)
  functions for overlaying multiple model fits on a single plot.
- New hex sticker logo for the package.

### Minor improvements and bug fixes

- Updated and expanded test suite for NHPP, MCF, and exposure models.
- New vignette on Repairable Systems Analysis (RSA).
- Updated README with new features and logo.
- Other minor improvements and bug fixes.

## ReliaGrowR 0.4.0

### New Features

- New
  [`sim_failures()`](https://paulgovan.github.io/ReliaGrowR/reference/sim_failures.md)
  function for simulating failures with optional simulation window
  parameter.
- MLE fit method for
  [`rga()`](https://paulgovan.github.io/ReliaGrowR/reference/rga.md)
  function for maximum likelihood estimation.
- Enhanced
  [`predict_rga()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_rga.md)
  function for making predictions from RGA models.
- Added confidence intervals for Duane model.
- Enhanced
  [`rdt()`](https://paulgovan.github.io/ReliaGrowR/reference/rdt.md)
  function with `f` parameter for failure-allowed reliability
  demonstration test plans.

### Minor improvements and bug fixes

- Fixed legend in rga_predict plot.
- Added WeibullR to package suggests.
- New comprehensive RGF (Reliability Growth Function) vignette with
  detailed documentation.
- Updated and improved test suite throughout.
- Enhanced package documentation and examples.
- Updated citation information.
- Code styling improvements.
- Other minor improvements and bug fixes.

## ReliaGrowR 0.3

CRAN release: 2025-11-07

### Minor improvements and bug Fixes

- More plotting options for RGA and Duane models.
- More printing options for RGA and Duane models.
- New vignette on reliability data management.
- More documentation and examples.
- Other minor improvements and bug fixes.

## ReliaGrowR 0.2

CRAN release: 2025-09-21

### Breaking Changes

- `duane_plot` function has been renamed to `duane` function with a
  separate S3 method for plotting.
- `plot_rga` function has been replaced with a separate S3 method for
  plotting RGA objects.

### New Features

- New goodness of fit plotting functions for RGA models called
  `ppplot.rga` and `qqplot.rga`. for P-P plots and Q-Q plots
  respectively.
- New `rdt` function for generating reliability demonstration test
  plans.

### Minor improvements and bug fixes

- More plotting options for RGA and Duane models.
- More documentation and examples.
- Other minor improvements and bug fixes.

## ReliaGrowR 0.1.5

CRAN release: 2025-07-13

### Minor Updates and Bug Fixes

- Minor improvements, documentation updates, and bug fixes.

## ReliaGrowR 0.1.4

CRAN release: 2025-05-16

### New features

- New custom print methods for RGA and Duane models.
- More plot options for RGA and Duane models.

### Minor Updates and Bug Fixes

- Other minor improvements, documentation updates, and bug fixes.

## ReliaGrowR 0.1.3

CRAN release: 2024-11-16

### Minor Updates and Bug Fixes

- Model results now include more details for model parameters including
  standard error, confidence bounds, etc.
- Other minor improvements.

## ReliaGrowR 0.1.2

### Minor Updates and Bug Fixes

- Duane model not includes optional plot argument.
- Other minor improvements.

## ReliaGrowR 0.1.1

CRAN release: 2024-11-01

### Minor Updates and Bug Fixes

- RGA and Duane model results now include goodness of fit metrics (AIC
  and BIC).
- Duane model results now include fitted values.
- Other minor improvements

## ReliaGrowR 0.1.0

CRAN release: 2024-09-19

- Initial release.

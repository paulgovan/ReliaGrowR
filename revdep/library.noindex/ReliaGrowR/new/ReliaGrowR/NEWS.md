# ReliaGrowR 0.4.0

## New Features
* New `sim_failures()` function for simulating failures with optional simulation window parameter.
* MLE fit method for `rga()` function for maximum likelihood estimation.
* Enhanced `predict_rga()` function for making predictions from RGA models.
* Added confidence intervals for Duane model.
* Enhanced `rdt()` function with `f` parameter for failure-allowed reliability demonstration test plans.

## Minor improvements and bug fixes
* Fixed legend in rga_predict plot.
* Added WeibullR to package suggests.
* New comprehensive RGF (Reliability Growth Function) vignette with detailed documentation.
* Updated and improved test suite throughout.
* Enhanced package documentation and examples.
* Updated citation information.
* Code styling improvements.
* Other minor improvements and bug fixes.

# ReliaGrowR 0.3

## Minor improvements and bug Fixes
* More plotting options for RGA and Duane models.
* More printing options for RGA and Duane models.
* New vignette on reliability data management.
* More documentation and examples.
* Other minor improvements and bug fixes.

# ReliaGrowR 0.2

## Breaking Changes
* `duane_plot` function has been renamed to `duane` function with a separate S3 method for plotting. 
* `plot_rga` function has been replaced with a separate S3 method for plotting RGA objects.

## New Features
* New goodness of fit plotting functions for RGA models called `ppplot.rga` and `qqplot.rga`.
for P-P plots and Q-Q plots respectively.
* New `rdt` function for generating reliability demonstration test plans.

## Minor improvements and bug fixes
* More plotting options for RGA and Duane models.
* More documentation and examples.
* Other minor improvements and bug fixes.

# ReliaGrowR 0.1.5

## Minor Updates and Bug Fixes
* Minor improvements, documentation updates, and bug fixes.

# ReliaGrowR 0.1.4

## New features
* New custom print methods for RGA and Duane models.
* More plot options for RGA and Duane models.

## Minor Updates and Bug Fixes
* Other minor improvements, documentation updates, and bug fixes.

# ReliaGrowR 0.1.3

## Minor Updates and Bug Fixes
* Model results now include more details for model parameters including standard error, confidence bounds, etc.
* Other minor improvements.

# ReliaGrowR 0.1.2

## Minor Updates and Bug Fixes
* Duane model not includes optional plot argument.
* Other minor improvements.

# ReliaGrowR 0.1.1

## Minor Updates and Bug Fixes
* RGA and Duane model results now include goodness of fit metrics (AIC and BIC).
* Duane model results now include fitted values.
* Other minor improvements

# ReliaGrowR 0.1.0

* Initial release.
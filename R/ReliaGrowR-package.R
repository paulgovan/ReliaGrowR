#' ReliaGrowR: Reliability Growth Analysis
#'
#' @description
#' Modeling and plotting functions for Reliability Growth Analysis (RGA).
#' The package implements three families of models and provides a REST API
#' interface via [plumber][plumber::plumber].
#'
#' @section Reliability Growth Models:
#'
#' **Crow-AMSAA (NHPP power-law)**
#'
#' The core model fits a Non-Homogeneous Poisson Process (NHPP) with a
#' Weibull intensity function to cumulative failure data. Parameters are
#' estimated by least-squares log-log regression (default) or maximum
#' likelihood.  A growth rate > 0 indicates reliability improvement.
#'
#' **Piecewise NHPP**
#'
#' Extends Crow-AMSAA by fitting separate NHPP segments separated by
#' change points.  Change points can be detected automatically via the
#' [segmented][segmented::segmented] package or supplied by the user.
#'
#' **Duane**
#'
#' Log-log regression of cumulative MTBF versus cumulative time, providing
#' a graphical and analytical representation of reliability growth.
#'
#' @section Main Functions:
#'
#' | Function | Description |
#' |---|---|
#' | [rga()] | Fit Crow-AMSAA or Piecewise NHPP model |
#' | [predict_rga()] | Forecast cumulative failures from a fitted model |
#' | [duane()] | Fit Duane model |
#' | [rdt()] | Reliability Demonstration Test plan calculator |
#' | [weibull_to_rga()] | Convert Weibull data to RGA format |
#' | [sim_failures()] | Simulate failures from a conditional Weibull model |
#' | [qqplot.rga()] | Q-Q goodness-of-fit plot for an `rga` object |
#' | [ppplot.rga()] | P-P goodness-of-fit plot for an `rga` object |
#' | [grwr_api()] | Launch the plumber REST API |
#'
#' @section S3 Classes and Methods:
#'
#' | Class | Methods |
#' |---|---|
#' | `rga` | [print.rga()], [plot.rga()] |
#' | `rga_predict` | [print.rga_predict()], [plot.rga_predict()] |
#' | `duane` | [print.duane()], [plot.duane()] |
#' | `rdt` | [print.rdt()] |
#'
#' @section References:
#' Crow, L. H. (1975). *Reliability Analysis for Complex Repairable Systems.*
#' AMSAA Technical Report No. 138. US Army Materiel Systems Analysis Activity.
#'
#' Duane, J. T. (1964). Learning curve approach to reliability monitoring.
#' *IEEE Transactions on Aerospace*, 2(2), 563–566.
#' \doi{10.1109/TA.1964.4319640}
#'
#' Guo, H., Mettas, A., Sarakakis, G., & Niu, P. (2010). Piecewise NHPP
#' models with maximum likelihood estimation for repairable systems. In
#' *Proceedings of the 2010 Annual Reliability and Maintainability Symposium*
#' (pp. 1–6). IEEE. \doi{10.1109/RAMS.2010.5448029}
#'
#' Muggeo, V. M. R. (2024). *segmented: Regression Models with Break-Points /
#' Change-Points Estimation.* R package.
#' \url{https://cran.r-project.org/package=segmented}
#'
#' @aliases ReliaGrowR-package
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

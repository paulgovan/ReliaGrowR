#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#' @srrstatsTODO {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' @srrstatsTODO {G5.4} **Correctness tests** *to test that statistical algorithms produce expected results to some fixed test data sets (potentially through comparisons using binding frameworks such as [RStata](https://github.com/lbraglia/RStata)).*
#' @srrstatsTODO {G5.4b} *For new implementations of existing methods, correctness tests should include tests against previous implementations. Such testing may explicitly call those implementations in testing, preferably from fixed-versions of other software, or use stored outputs from those where that is not possible.*
#' @srrstatsTODO {G5.4c} *Where applicable, stored values may be drawn from published paper outputs when applicable and where code from original implementations is not available*
#' @srrstatsTODO {G5.5} *Correctness tests should be run with a fixed random seed*

#' @noRd
NULL

#' NA_standards
#'
#' @srrstatsNA {G1.4a} There are no internal (non-exported) functions.
#' @srrstatsNA {G1.6} There are no performance claims compared to other R packages.
#' @srrstatsNA {G2.0a} The primary documentation covers all expectations on lengths of inputs.
#' @srrstatsNA {G2.1a} The primary documentation covers all expectations on data types of all vector inputs.
#' @srrstatsNA {G2.4} See sub-tags for responses.
#' @srrstatsNA {G2.4a} `integer` conversion is not required.
#' @srrstatsNA {G2.4c} `as.character()` conversion is not required.
#' @srrstatsNA {G2.4d} `as.factor()` conversion is not required.
#' @srrstatsNA {G2.4e} `as...()` conversion is not required.
#' @srrstatsNA {G2.5} There are no factor inputs.
#' @srrstatsNA {G2.9} No information is lost in type conversions.
#' @srrstatsNA {G2.14b} Missing data results in an error.
#' @srrstatsNA {G3.0} There are no comparisons made between floating point numbers.
#' @srrstatsNA {G3.1} There are no covariance calculations.
#' @srrstatsNA {G3.1a} There are no covariance calculations.
#' @srrstatsNA {G4.0} There are no local file outputs at this time.
#' @srrstatsNA {G5.3} Missing values return an error.
#' @srrstatsNA {G5.4a} There are new implementations of existing methods but no new methods.
#' @srrstatsNA {G5.6b} There are no random components in the algorithms.
#' @srrstatsNA {G5.9b} There are no random components in the algorithms.
#' @srrstatsNA {G5.10} All unit tests run as part of continuous integration.
#' @srrstatsNA {G5.11} Unit tests do not require large amounts of data.
#' @srrstatsNA {G5.11a} Unit tests do not require downloads of additional data.
#' @srrstatsNA {G2.12} `data.frame` objects do not have list columns.
#' @srrstatsNA {G5.12} Unit tests do not require any special platform requirements, memory, expected runtime, or artefacts.
#'
#' @noRd
NULL

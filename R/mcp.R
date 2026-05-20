#' MCP Tool Definitions and Server for ReliaGrowR
#'
#' Exposes the core ReliaGrowR functions as Model Context Protocol (MCP) tools
#' so AI assistants (e.g., Claude) can call them directly. Requires the
#' \pkg{mcptools} and \pkg{ellmer} packages.
#'
#' Start the server from the command line:
#' \preformatted{
#'   Rscript -e "ReliaGrowR::rga_mcp_server()"
#' }
#'
#' Add it to Claude Code:
#' \preformatted{
#'   claude mcp add -s user reliagrowR -- Rscript -e "ReliaGrowR::rga_mcp_server()"
#' }
#'
#' @name mcp
#' @keywords internal
NULL

.check_mcp_deps <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required. Install with: install.packages('ellmer')",
         call. = FALSE)
  }
  if (!requireNamespace("mcptools", quietly = TRUE)) {
    stop("Package 'mcptools' is required. Install with: install.packages('mcptools')",
         call. = FALSE)
  }
}

#' @keywords internal
.make_rga_tool <- function() {
  ellmer::tool(
    function(times, failures, times_type = "failure_times", method = "LS") {
      fit <- rga(
        times      = times,
        failures   = failures,
        times_type = times_type,
        method     = method
      )
      list(
        beta        = as.numeric(fit$betas[1]),
        lambda      = as.numeric(fit$lambdas[1]),
        growth_rate = as.numeric(fit$growth_rate[1]),
        logLik      = fit$logLik,
        AIC         = fit$AIC,
        BIC         = fit$BIC,
        n_obs       = fit$n_obs,
        method      = fit$method
      )
    },
    name = "rga",
    description = paste(
      "Fit a Crow-AMSAA Reliability Growth Analysis (NHPP Power Law) model.",
      "Returns beta (shape), lambda (scale), growth rate, and fit statistics.",
      "Use times_type='failure_times' for individual failure times or",
      "'cumulative_failure_times' for cumulative times."
    ),
    arguments = list(
      times      = ellmer::type_array(
        ellmer::type_number(),
        description = "Failure times (individual or cumulative, per times_type)."
      ),
      failures   = ellmer::type_array(
        ellmer::type_number(),
        description = "Number of failures at each corresponding time."
      ),
      times_type = ellmer::type_string(
        description = "One of 'failure_times' (default) or 'cumulative_failure_times'."
      ),
      method     = ellmer::type_string(
        description = "Estimation method: 'LS' (least squares, default) or 'MLE'."
      )
    )
  )
}

#' @keywords internal
.make_nhpp_tool <- function() {
  ellmer::tool(
    function(time, event, model_type = "Power Law", method = "MLE") {
      fit <- nhpp(
        time       = time,
        event      = event,
        model_type = model_type,
        method     = method
      )
      params <- fit$params
      list(
        model_type = fit$model_type,
        method     = fit$method,
        params     = params,
        logLik     = fit$logLik,
        AIC        = fit$AIC,
        BIC        = fit$BIC
      )
    },
    name = "nhpp",
    description = paste(
      "Fit a Non-Homogeneous Poisson Process (NHPP) model for repairable systems.",
      "Supports Power Law (Crow-AMSAA) and Log-Linear models.",
      "Inputs are event times and binary event indicators (1=failure, 0=censored)."
    ),
    arguments = list(
      time       = ellmer::type_array(
        ellmer::type_number(),
        description = "Event times for each observation."
      ),
      event      = ellmer::type_array(
        ellmer::type_number(),
        description = "Event indicator: 1 = failure, 0 = censored (end of observation)."
      ),
      model_type = ellmer::type_string(
        description = "Model type: 'Power Law' (default) or 'Log-Linear'."
      ),
      method     = ellmer::type_string(
        description = "Estimation method: 'MLE' (default) or 'LS'."
      )
    )
  )
}

#' @keywords internal
.make_duane_tool <- function() {
  ellmer::tool(
    function(times, failures, conf_level = 0.95) {
      fit <- duane(times = times, failures = failures, conf_level = conf_level)
      list(
        slope      = as.numeric(stats::coef(fit$model)[2]),
        intercept  = as.numeric(stats::coef(fit$model)[1]),
        logLik     = fit$logLik,
        AIC        = fit$AIC,
        BIC        = fit$BIC,
        n_obs      = fit$n_obs,
        conf_level = fit$conf_level
      )
    },
    name = "duane",
    description = paste(
      "Fit a Duane (log-log) reliability growth model.",
      "Returns the slope (growth parameter) and intercept of the log(MTBF) vs log(Time)",
      "regression, along with goodness-of-fit statistics."
    ),
    arguments = list(
      times      = ellmer::type_array(
        ellmer::type_number(),
        description = "Failure times."
      ),
      failures   = ellmer::type_array(
        ellmer::type_number(),
        description = "Number of failures at each corresponding time."
      ),
      conf_level = ellmer::type_number(
        description = "Confidence level for bounds (default: 0.95)."
      )
    )
  )
}

#' @keywords internal
.make_mcf_tool <- function() {
  ellmer::tool(
    function(id, time, event, conf_level = 0.95) {
      fit <- mcf(id = id, time = time, event = event, conf_level = conf_level)
      list(
        n_systems = fit$n_systems,
        n_events  = fit$n_events,
        times     = fit$time,
        mcf       = fit$mcf,
        lower     = fit$lower_bounds,
        upper     = fit$upper_bounds
      )
    },
    name = "mcf",
    description = paste(
      "Compute the Mean Cumulative Function (MCF) for repairable systems",
      "using the Nelson-Aalen estimator.",
      "Each row represents one event observation with system id, time, and event flag."
    ),
    arguments = list(
      id         = ellmer::type_array(
        ellmer::type_string(),
        description = "System identifier for each observation."
      ),
      time       = ellmer::type_array(
        ellmer::type_number(),
        description = "Observation time for each record."
      ),
      event      = ellmer::type_array(
        ellmer::type_number(),
        description = "Event indicator: 1 = failure, 0 = end of observation (censored)."
      ),
      conf_level = ellmer::type_number(
        description = "Confidence level for bounds (default: 0.95)."
      )
    )
  )
}

#' @keywords internal
.make_predict_rga_tool <- function() {
  ellmer::tool(
    function(times, failures, forecast_times, method = "LS", conf_level = 0.95) {
      fit <- rga(times = times, failures = failures, method = method)
      fc  <- predict_rga(fit, times = forecast_times, conf_level = conf_level)
      list(
        forecast_times = fc$times,
        cum_failures   = as.numeric(fc$cum_failures),
        lower_bounds   = as.numeric(fc$lower_bounds),
        upper_bounds   = as.numeric(fc$upper_bounds),
        conf_level     = fc$conf_level
      )
    },
    name = "predict_rga",
    description = paste(
      "Forecast cumulative failures from a Crow-AMSAA RGA model.",
      "Fits the model to the provided data and returns predicted cumulative failures",
      "with confidence bounds at the specified forecast times."
    ),
    arguments = list(
      times          = ellmer::type_array(
        ellmer::type_number(),
        description = "Historical failure times used to fit the model."
      ),
      failures       = ellmer::type_array(
        ellmer::type_number(),
        description = "Number of failures at each historical time."
      ),
      forecast_times = ellmer::type_array(
        ellmer::type_number(),
        description = "Future cumulative times at which to forecast."
      ),
      method         = ellmer::type_string(
        description = "Estimation method: 'LS' (default) or 'MLE'."
      ),
      conf_level     = ellmer::type_number(
        description = "Confidence level (default: 0.95)."
      )
    )
  )
}

#' @keywords internal
.make_predict_duane_tool <- function() {
  ellmer::tool(
    function(times, failures, forecast_times, conf_level = 0.95) {
      fit <- duane(times = times, failures = failures, conf_level = conf_level)
      fc  <- predict_duane(fit, times = forecast_times, conf_level = conf_level)
      list(
        forecast_times = fc$times,
        mtbf           = as.numeric(fc$mtbf),
        lower_bounds   = as.numeric(fc$lower_bounds),
        upper_bounds   = as.numeric(fc$upper_bounds),
        conf_level     = fc$conf_level
      )
    },
    name = "predict_duane",
    description = paste(
      "Forecast MTBF from a Duane reliability growth model.",
      "Fits the Duane model and returns predicted MTBF with confidence bounds",
      "at the specified future cumulative times."
    ),
    arguments = list(
      times          = ellmer::type_array(
        ellmer::type_number(),
        description = "Historical failure times."
      ),
      failures       = ellmer::type_array(
        ellmer::type_number(),
        description = "Number of failures at each historical time."
      ),
      forecast_times = ellmer::type_array(
        ellmer::type_number(),
        description = "Future cumulative times at which to forecast MTBF."
      ),
      conf_level     = ellmer::type_number(
        description = "Confidence level (default: 0.95)."
      )
    )
  )
}

#' @keywords internal
.make_rdt_tool <- function() {
  ellmer::tool(
    function(target, mission_time, conf_level, beta = 1, f = 0,
             n = NULL, test_time = NULL) {
      fit <- rdt(
        target       = target,
        mission_time = mission_time,
        conf_level   = conf_level,
        beta         = beta,
        f            = f,
        n            = n,
        test_time    = test_time
      )
      as.list(fit)
    },
    name = "rdt",
    description = paste(
      "Plan a Reliability Demonstration Test (RDT).",
      "Given a target reliability, mission time, and confidence level,",
      "computes either the required test time (if sample size n is given)",
      "or the required sample size (if test_time is given).",
      "Uses the Weibull distribution with shape parameter beta."
    ),
    arguments = list(
      target       = ellmer::type_number(
        description = "Target reliability (probability of survival), between 0 and 1."
      ),
      mission_time = ellmer::type_number(
        description = "Mission time (duration) over which reliability is demonstrated."
      ),
      conf_level   = ellmer::type_number(
        description = "Confidence level for the test (e.g., 0.90)."
      ),
      beta         = ellmer::type_number(
        description = "Weibull shape parameter (default: 1 for exponential)."
      ),
      f            = ellmer::type_number(
        description = "Allowable number of failures during the test (default: 0)."
      ),
      n            = ellmer::type_number(
        description = "Sample size (number of units on test). Provide to solve for test time."
      ),
      test_time    = ellmer::type_number(
        description = "Total test time per unit. Provide to solve for required sample size."
      )
    )
  )
}

#' @keywords internal
.make_gof_tool <- function() {
  ellmer::tool(
    function(times, failures, method = "LS") {
      fit <- rga(times = times, failures = failures, method = method)
      g   <- gof(fit)
      list(
        model_type = g$model_type,
        n          = g$n,
        cvm        = g$cvm,
        ks         = g$ks
      )
    },
    name = "gof_rga",
    description = paste(
      "Compute goodness-of-fit statistics for a Crow-AMSAA RGA model.",
      "Returns the Cramer-von Mises (W^2) and Kolmogorov-Smirnov (D) statistics.",
      "Smaller values indicate a better fit to the NHPP Power Law model."
    ),
    arguments = list(
      times    = ellmer::type_array(
        ellmer::type_number(),
        description = "Failure times."
      ),
      failures = ellmer::type_array(
        ellmer::type_number(),
        description = "Number of failures at each corresponding time."
      ),
      method   = ellmer::type_string(
        description = "Estimation method: 'LS' (default) or 'MLE'."
      )
    )
  )
}

#' Start the ReliaGrowR MCP Server
#'
#' Starts a Model Context Protocol (MCP) server that exposes the core
#' ReliaGrowR analysis functions as tools for AI assistants such as Claude.
#'
#' Requires the \pkg{mcptools} and \pkg{ellmer} packages to be installed.
#'
#' @section Available tools:
#' \describe{
#'   \item{\code{rga}}{Fit Crow-AMSAA reliability growth model}
#'   \item{\code{nhpp}}{Fit NHPP model for repairable systems}
#'   \item{\code{duane}}{Fit Duane log-log reliability growth model}
#'   \item{\code{mcf}}{Compute Mean Cumulative Function}
#'   \item{\code{predict_rga}}{Forecast from RGA model}
#'   \item{\code{predict_duane}}{Forecast MTBF from Duane model}
#'   \item{\code{rdt}}{Plan a Reliability Demonstration Test}
#'   \item{\code{gof_rga}}{Goodness-of-fit statistics for RGA model}
#' }
#'
#' @section Claude Code setup:
#' \preformatted{
#' claude mcp add -s user reliagrowR -- Rscript -e "ReliaGrowR::rga_mcp_server()"
#' }
#'
#' @section Claude Desktop setup (claude_desktop_config.json):
#' \preformatted{
#' {
#'   "mcpServers": {
#'     "reliagrowR": {
#'       "command": "Rscript",
#'       "args": ["-e", "ReliaGrowR::rga_mcp_server()"]
#'     }
#'   }
#' }
#' }
#'
#' @param ... Additional arguments passed to \code{mcptools::mcp_server()},
#'   e.g., \code{type = "stdio"} (default) or \code{type = "http"}.
#'
#' @return Called for its side effect of starting the MCP server.
#' @importFrom stats coef
#' @export
rga_mcp_server <- function(...) {
  .check_mcp_deps()

  tools <- list(
    .make_rga_tool(),
    .make_nhpp_tool(),
    .make_duane_tool(),
    .make_mcf_tool(),
    .make_predict_rga_tool(),
    .make_predict_duane_tool(),
    .make_rdt_tool(),
    .make_gof_tool()
  )

  mcptools::mcp_server(tools = tools, ...)
}

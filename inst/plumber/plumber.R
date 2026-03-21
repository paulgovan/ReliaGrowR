# plumber.R

#* @apiTitle ReliaGrowR API
#* @apiDescription An API for the ReliaGrowR package
#* @apiContact list(name = "API Issues", url = "https://github.com/paulgovan/ReliaGrowR/issues")
#* @apiLicense list(name = "CC-BY-4.0", url = "https://creativecommons.org/licenses/by/4.0/")
#* @apiVersion 0.2

library(ReliaGrowR)

# Source scripts (comment-only stubs kept for organisational clarity)
source("plumb-duane.R")
source("plumb-rga.R")
source("plumb-gof.R")

# ---------------------------------------------------------------------------
# Serialisation helpers — convert S3 objects to plain lists for JSON output.
# (jsonlite has no asJSON method for custom S3 classes, and rga/rga_predict
#  embed lm objects that contain environments which cannot be serialised.)
# ---------------------------------------------------------------------------

.rga_as_list <- function(fit) {
  betas <- if (is.list(fit$betas)) {
    as.numeric(fit$betas$log_times[, "Est."])
  } else {
    as.numeric(fit$betas)
  }
  betas_se <- as.numeric(fit$betas_se)
  lambdas  <- if (is.matrix(fit$lambdas)) {
    as.numeric(fit$lambdas[, "Est."])
  } else {
    as.numeric(fit$lambdas)
  }
  breakpoints <- if (!is.null(fit$breakpoints)) as.numeric(fit$breakpoints) else NULL

  list(
    times         = as.numeric(fit$times),
    failures      = as.numeric(fit$failures),
    n_obs         = fit$n_obs,
    cum_failures  = as.numeric(fit$cum_failures),
    residuals     = as.numeric(fit$residuals),
    logLik        = fit$logLik,
    AIC           = fit$AIC,
    BIC           = fit$BIC,
    method        = fit$method,
    breakpoints   = breakpoints,
    fitted_values = as.numeric(fit$fitted_values),
    lower_bounds  = as.numeric(fit$lower_bounds),
    upper_bounds  = as.numeric(fit$upper_bounds),
    growth_rate   = as.numeric(fit$growth_rate),
    betas         = betas,
    betas_se      = betas_se,
    lambdas       = lambdas
  )
}

.rga_predict_as_list <- function(fc) {
  list(
    times        = as.numeric(fc$times),
    cum_failures = as.numeric(fc$cum_failures),
    lower_bounds = as.numeric(fc$lower_bounds),
    upper_bounds = as.numeric(fc$upper_bounds),
    conf_level   = fc$conf_level,
    model_type   = fc$model_type
  )
}

.duane_as_list <- function(d) {
  cb <- d$Confidence_Bounds
  list(
    times              = as.numeric(d$times),
    failures           = as.numeric(d$failures),
    n_obs              = d$n_obs,
    MTBF               = as.numeric(d$MTBF),
    logLik             = d$logLik,
    AIC                = d$AIC,
    BIC                = d$BIC,
    conf.level         = d$conf.level,
    Cumulative_Time    = as.numeric(d$Cumulative_Time),
    Cumulative_MTBF    = as.numeric(d$Cumulative_MTBF),
    Fitted_Values      = as.numeric(d$Fitted_Values),
    lower_bounds       = as.numeric(d$lower_bounds),
    upper_bounds       = as.numeric(d$upper_bounds),
    Confidence_Bounds  = if (!is.null(cb)) as.data.frame(cb) else NULL,
    Residuals_Log      = as.numeric(d$Residuals_Log),
    Residuals_MTBF     = as.numeric(d$Residuals_MTBF)
  )
}

# ---------------------------------------------------------------------------
# Duane
# ---------------------------------------------------------------------------

#* Run a Duane Analysis
#* @param times Cumulative failure times (comma separated)
#* @param failures The number of failures at each corresponding time in times (comma separated)
#* @param conf.level:numeric Confidence level for intervals (default: 0.95)
#* @post /duane
function(times, failures, conf.level = 0.95) {

  times <- as.numeric(unlist(strsplit(times, ",")))
  failures <- as.numeric(unlist(strsplit(failures, ",")))
  conf.level <- as.numeric(conf.level)

  result <- duane(times = times, failures = failures,
                  conf.level = conf.level)

  return(.duane_as_list(result))
}

#* Plot Method for Duane Analysis
#* @serializer png
#* @param times Cumulative failure times (comma separated)
#* @param failures The number of failures at each corresponding time in times (comma separated)
#* @param conf.level:numeric Confidence level for intervals (default: 0.95)
#* @param log:boolean Whether to use logarithmic scales (default: TRUE)
#* @param conf.int:boolean Whether to plot confidence bounds (default: TRUE)
#* @post /plot.duane
function(times, failures, conf.level = 0.95, log = TRUE, conf.int = TRUE) {

  times <- as.numeric(unlist(strsplit(times, ",")))
  failures <- as.numeric(unlist(strsplit(failures, ",")))
  conf.level <- as.numeric(conf.level)
  log <- as.logical(log)
  conf.int <- as.logical(conf.int)

  result <- duane(times = times, failures = failures,
                  conf.level = conf.level)
  plot(result, log = log, conf.int = conf.int)
}

# ---------------------------------------------------------------------------
# RGA
# ---------------------------------------------------------------------------

#* Run a Crow-AMSAA or Piecewise NHPP Analysis
#* @param times Cumulative failure times (comma separated)
#* @param failures The number of failures at each corresponding time in times (comma separated)
#* @param model_type Type of model: "Crow-AMSAA" or "Piecewise NHPP" (default: "Crow-AMSAA")
#* @param breaks Cumulative time breakpoints for Piecewise NHPP (comma separated, optional)
#* @param conf_level Confidence level for intervals (default: 0.95)
#* @param method Estimation method: "LS" (default) or "MLE"
#* @post /rga
function(times, failures, model_type = "Crow-AMSAA", breaks = NULL,
         conf_level = 0.95, method = "LS") {

  times <- as.numeric(unlist(strsplit(times, ",")))
  failures <- as.numeric(unlist(strsplit(failures, ",")))
  conf_level <- as.numeric(conf_level)
  method <- match.arg(method, c("LS", "MLE"))
  if (!is.null(breaks))
    breaks <- as.numeric(unlist(strsplit(breaks, ",")))

  result <- rga(times = times, failures = failures,
                model_type = model_type, breaks = breaks,
                conf_level = conf_level, method = method)
  return(.rga_as_list(result))
}

#* Plot Method for RGA Objects
#* @serializer png
#* @param times Cumulative failure times (comma separated)
#* @param failures The number of failures at each corresponding time in times (comma separated)
#* @param model_type Type of model: "Crow-AMSAA" or "Piecewise NHPP" (default: "Crow-AMSAA")
#* @param breaks Cumulative time breakpoints for Piecewise NHPP (comma separated, optional)
#* @param conf_level Confidence level for intervals (default: 0.95)
#* @param method Estimation method: "LS" (default) or "MLE"
#* @param conf_bounds:boolean Whether to include confidence bounds (default: TRUE)
#* @param legend:boolean Whether to show the legend (default: TRUE)
#* @param log:boolean Whether to use a log-log scale (default: FALSE)
#* @param legend_pos Position of the legend (default: "bottomright")
#* @post /plot.rga
function(times, failures, model_type = "Crow-AMSAA", breaks = NULL,
         conf_level = 0.95, method = "LS",
         conf_bounds = TRUE, legend = TRUE, log = FALSE,
         legend_pos = "bottomright") {

  times <- as.numeric(unlist(strsplit(times, ",")))
  failures <- as.numeric(unlist(strsplit(failures, ",")))
  model_type <- as.character(model_type)
  conf_level <- as.numeric(conf_level)
  method <- match.arg(method, c("LS", "MLE"))
  if (!is.null(breaks))
    breaks <- as.numeric(unlist(strsplit(breaks, ",")))
  conf_bounds <- as.logical(conf_bounds)
  legend <- as.logical(legend)
  log <- as.logical(log)
  legend_pos <- as.character(legend_pos)

  result <- rga(times = times, failures = failures,
                model_type = model_type, breaks = breaks,
                conf_level = conf_level, method = method)
  plot(result, conf_bounds = conf_bounds, legend = legend,
       log = log, legend_pos = legend_pos)
}

# ---------------------------------------------------------------------------
# predict_rga
# ---------------------------------------------------------------------------

#* Forecast Cumulative Failures from a Fitted RGA Model
#* @param times Cumulative failure times used to fit the model (comma separated)
#* @param failures Number of failures at each time (comma separated)
#* @param predict_times Cumulative times for forecasting (comma separated)
#* @param model_type Type of model: "Crow-AMSAA" or "Piecewise NHPP" (default: "Crow-AMSAA")
#* @param method Estimation method: "LS" (default) or "MLE"
#* @param conf_level Confidence level (default: 0.95)
#* @post /predict_rga
function(times, failures, predict_times, model_type = "Crow-AMSAA",
         method = "LS", conf_level = 0.95) {

  times <- as.numeric(unlist(strsplit(times, ",")))
  failures <- as.numeric(unlist(strsplit(failures, ",")))
  predict_times <- as.numeric(unlist(strsplit(predict_times, ",")))
  conf_level <- as.numeric(conf_level)
  method <- match.arg(method, c("LS", "MLE"))

  fit <- rga(times = times, failures = failures,
             model_type = model_type, method = method,
             conf_level = conf_level)
  fc <- predict_rga(fit, times = predict_times, conf_level = conf_level)
  .rga_predict_as_list(fc)
}

#* Plot Forecast from a Fitted RGA Model
#* @serializer png
#* @param times Cumulative failure times used to fit the model (comma separated)
#* @param failures Number of failures at each time (comma separated)
#* @param predict_times Cumulative times for forecasting (comma separated)
#* @param model_type Type of model: "Crow-AMSAA" or "Piecewise NHPP" (default: "Crow-AMSAA")
#* @param method Estimation method: "LS" (default) or "MLE"
#* @param conf_level Confidence level (default: 0.95)
#* @param conf_bounds:boolean Whether to include confidence bounds (default: TRUE)
#* @param legend:boolean Whether to show the legend (default: TRUE)
#* @param legend_pos Position of the legend (default: "bottomright")
#* @post /plot.predict_rga
function(times, failures, predict_times, model_type = "Crow-AMSAA",
         method = "LS", conf_level = 0.95,
         conf_bounds = TRUE, legend = TRUE, legend_pos = "bottomright") {

  times <- as.numeric(unlist(strsplit(times, ",")))
  failures <- as.numeric(unlist(strsplit(failures, ",")))
  predict_times <- as.numeric(unlist(strsplit(predict_times, ",")))
  conf_level <- as.numeric(conf_level)
  method <- match.arg(method, c("LS", "MLE"))
  conf_bounds <- as.logical(conf_bounds)
  legend <- as.logical(legend)
  legend_pos <- as.character(legend_pos)

  fit <- rga(times = times, failures = failures,
             model_type = model_type, method = method,
             conf_level = conf_level)
  fc <- predict_rga(fit, times = predict_times, conf_level = conf_level)
  plot(fc, conf_bounds = conf_bounds, legend = legend, legend_pos = legend_pos)
}

# ---------------------------------------------------------------------------
# RDT
# ---------------------------------------------------------------------------

#* Calculate Required Sample Size or Test Time for Reliability Demonstration Test
#* @param target Target reliability to demonstrate (e.g., 0.9)
#* @param mission_time Mission time for the reliability target (e.g., 1000 hours)
#* @param conf_level Confidence level for the demonstration (e.g., 0.95)
#* @param beta Shape parameter of the Weibull distribution (default: 1 for Exponential)
#* @param f Allowable failures during the test (non-negative integer, default: 0)
#* @param n Sample size (number of units to test, optional)
#* @param test_time Test time per unit (optional)
#* @post /rdt
function(target, mission_time, conf_level, beta = 1, f = 0,
         n = NULL, test_time = NULL) {

  target <- as.numeric(target)
  mission_time <- as.numeric(mission_time)
  conf_level <- as.numeric(conf_level)
  beta <- as.numeric(beta)
  f <- as.integer(f)
  if (!is.null(n)) n <- as.integer(n)
  if (!is.null(test_time)) test_time <- as.numeric(test_time)

  result <- rdt(target = target, mission_time = mission_time, conf_level = conf_level,
                beta = beta, f = f, n = n, test_time = test_time)
  unclass(result)
}

# ---------------------------------------------------------------------------
# weibull_to_rga
# ---------------------------------------------------------------------------

#* Convert Weibull Failure/Suspension/Interval Data to RGA Format
#* @param failures Exact failure times (comma separated)
#* @param suspensions Right-censored times (comma separated, optional)
#* @param interval_starts Interval lower bounds (comma separated, optional)
#* @param interval_ends Interval upper bounds (comma separated, optional)
#* @post /weibull_to_rga
function(failures, suspensions = NULL, interval_starts = NULL,
         interval_ends = NULL) {

  failures <- as.numeric(unlist(strsplit(failures, ",")))
  if (!is.null(suspensions))
    suspensions <- as.numeric(unlist(strsplit(suspensions, ",")))
  if (!is.null(interval_starts))
    interval_starts <- as.numeric(unlist(strsplit(interval_starts, ",")))
  if (!is.null(interval_ends))
    interval_ends <- as.numeric(unlist(strsplit(interval_ends, ",")))

  weibull_to_rga(failures, suspensions, interval_starts, interval_ends)
}

# ---------------------------------------------------------------------------
# sim_failures
# ---------------------------------------------------------------------------

#* Simulate Failures from a Conditional Weibull Model
#* @param n Number of failures to simulate (positive integer)
#* @param runtimes Current runtimes for each unit (comma separated)
#* @param replace:boolean Sampling with replacement (default: FALSE)
#* @param window:numeric Observation window width (optional)
#* @post /sim_failures
function(n, runtimes, replace = FALSE, window = NULL) {

  n <- as.integer(n)
  runtimes <- as.numeric(unlist(strsplit(runtimes, ",")))
  replace <- as.logical(replace)
  if (!is.null(window)) window <- as.numeric(window)

  sim_failures(n, runtimes, replace = replace, window = window)
}

# ---------------------------------------------------------------------------
# GoF plots
# ---------------------------------------------------------------------------

#* Q-Q Plot for a Fitted RGA Model
#* @serializer png
#* @param times Cumulative failure times (comma separated)
#* @param failures Number of failures at each time (comma separated)
#* @param model_type Type of model: "Crow-AMSAA" or "Piecewise NHPP" (default: "Crow-AMSAA")
#* @param method Estimation method: "LS" (default) or "MLE"
#* @param conf_level Confidence level (default: 0.95)
#* @param main Plot title (default: "Q-Q Plot")
#* @post /qqplot.rga
function(times, failures, model_type = "Crow-AMSAA",
         method = "LS", conf_level = 0.95, main = "Q-Q Plot") {

  times <- as.numeric(unlist(strsplit(times, ",")))
  failures <- as.numeric(unlist(strsplit(failures, ",")))
  conf_level <- as.numeric(conf_level)
  method <- match.arg(method, c("LS", "MLE"))

  fit <- rga(times = times, failures = failures,
             model_type = model_type, method = method,
             conf_level = conf_level)
  qqplot.rga(fit, main = main)
}

#* P-P Plot for a Fitted RGA Model
#* @serializer png
#* @param times Cumulative failure times (comma separated)
#* @param failures Number of failures at each time (comma separated)
#* @param model_type Type of model: "Crow-AMSAA" or "Piecewise NHPP" (default: "Crow-AMSAA")
#* @param method Estimation method: "LS" (default) or "MLE"
#* @param conf_level Confidence level (default: 0.95)
#* @param main Plot title (default: "P-P Plot")
#* @post /ppplot.rga
function(times, failures, model_type = "Crow-AMSAA",
         method = "LS", conf_level = 0.95, main = "P-P Plot") {

  times <- as.numeric(unlist(strsplit(times, ",")))
  failures <- as.numeric(unlist(strsplit(failures, ",")))
  conf_level <- as.numeric(conf_level)
  method <- match.arg(method, c("LS", "MLE"))

  fit <- rga(times = times, failures = failures,
             model_type = model_type, method = method,
             conf_level = conf_level)
  ppplot.rga(fit, main = main)
}

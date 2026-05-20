#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.

# MCP tool tests only run if ellmer is available
skip_if_not_installed("ellmer")

# ── Dependency check ───────────────────────────────────────────────────────────

test_that(".check_mcp_deps errors informatively when mcptools is missing", {
  # We can only test this if mcptools is actually missing; skip if installed
  skip_if(requireNamespace("mcptools", quietly = TRUE),
          "mcptools is installed; cannot test missing-package error")
  expect_error(
    ReliaGrowR:::.check_mcp_deps(),
    "mcptools"
  )
})

# ── Tool construction ──────────────────────────────────────────────────────────

test_that("each tool factory returns an ellmer ToolDef object", {
  factories <- list(
    ReliaGrowR:::.make_rga_tool,
    ReliaGrowR:::.make_nhpp_tool,
    ReliaGrowR:::.make_duane_tool,
    ReliaGrowR:::.make_mcf_tool,
    ReliaGrowR:::.make_predict_rga_tool,
    ReliaGrowR:::.make_predict_duane_tool,
    ReliaGrowR:::.make_rdt_tool,
    ReliaGrowR:::.make_gof_tool
  )
  for (f in factories) {
    tool <- f()
    expect_true(inherits(tool, "ellmer::ToolDef"),
                label = paste("Expected ellmer::ToolDef class from factory"))
  }
})

test_that("rga tool wrapper returns correct keys", {
  tool   <- ReliaGrowR:::.make_rga_tool()
  result <- tool(
    times    = c(100, 200, 300, 400, 500),
    failures = c(1, 2, 1, 3, 2)
  )
  expect_named(result, c("beta", "lambda", "growth_rate", "logLik", "AIC", "BIC",
                          "n_obs", "method"))
  expect_equal(result$n_obs, 5L)
  expect_true(is.finite(result$beta))
  expect_true(is.finite(result$lambda))
})

test_that("duane tool wrapper returns correct keys", {
  tool   <- ReliaGrowR:::.make_duane_tool()
  result <- tool(
    times    = c(100, 200, 300, 400, 500),
    failures = c(1, 2, 1, 3, 2)
  )
  expect_named(result, c("slope", "intercept", "logLik", "AIC", "BIC",
                          "n_obs", "conf_level"))
  expect_true(is.finite(result$slope))
})

test_that("gof tool wrapper returns correct keys", {
  tool   <- ReliaGrowR:::.make_gof_tool()
  result <- tool(
    times    = c(100, 200, 300, 400, 500),
    failures = c(1, 2, 1, 3, 2)
  )
  expect_named(result, c("model_type", "n", "cvm", "ks"))
  expect_equal(result$model_type, "Crow-AMSAA")
  expect_true(result$cvm > 0)
  expect_true(result$ks > 0)
})

test_that("predict_rga tool wrapper returns forecast vectors", {
  tool   <- ReliaGrowR:::.make_predict_rga_tool()
  result <- tool(
    times          = c(100, 200, 300, 400, 500),
    failures       = c(1, 2, 1, 3, 2),
    forecast_times = c(2000, 5000)
  )
  expect_named(result, c("forecast_times", "cum_failures", "lower_bounds",
                          "upper_bounds", "conf_level"))
  expect_length(result$cum_failures, 2)
  expect_true(all(result$lower_bounds < result$cum_failures))
  expect_true(all(result$cum_failures < result$upper_bounds))
})

test_that("predict_duane tool wrapper returns mtbf forecast", {
  tool   <- ReliaGrowR:::.make_predict_duane_tool()
  result <- tool(
    times          = c(100, 200, 300, 400, 500),
    failures       = c(1, 2, 1, 3, 2),
    forecast_times = c(2000, 5000)
  )
  expect_named(result, c("forecast_times", "mtbf", "lower_bounds",
                          "upper_bounds", "conf_level"))
  expect_length(result$mtbf, 2)
})

test_that("rga_mcp_server errors informatively when mcptools is missing", {
  skip_if(requireNamespace("mcptools", quietly = TRUE),
          "mcptools is installed; cannot test missing-package error")
  expect_error(rga_mcp_server(), "mcptools")
})

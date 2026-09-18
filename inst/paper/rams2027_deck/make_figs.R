# Generate presentation figures for the RAMS 2027 RGF deck.
# Reproduces the key chunks of inst/paper/RGF_paper.Rmd at slide resolution.
suppressMessages({
  library(ReliaGrowR)
  library(WeibullR)
})

figdir <- "/Users/paulgovan/Documents/ReliaGrowR/inst/paper/rams2027_deck/figs"

# Deck palette (RAMS-branded; must match build_deck_template.py):
GROWTH <- "#C32135" # RAMS red -> with-growth series (solid)
BASE   <- "#8A8F98" # grey     -> no-growth baseline (dashed)
ACCENT <- "#14243A" # navy     -> highlight/annotation

png_open <- function(name, w = 1600, h = 1200, res = 200) {
  png(file.path(figdir, name), width = w, height = h, res = res)
  par(mar = c(4.2, 4.2, 1.2, 1.2), cex = 0.95)
}

## ---- Fleet baseline Weibull (paper Fig 1) --------------------------------
set.seed(8)
n_units <- 100
test_end <- 1000
beta_mech <- 4
eta_matured <- 1456
fleet_life <- eta_matured * (-log(runif(n_units)))^(1 / beta_mech)
failures <- sort(fleet_life[fleet_life <= test_end])
suspensions <- rep(test_end, sum(fleet_life > test_end))
n_test_failures <- length(failures)
n_surviving <- length(suspensions)

obj <- wblr(failures, suspensions, col = GROWTH, is.plot.cb = FALSE)
obj <- wblr.fit(obj, method.fit = "mle")
sim_beta <- obj$fit[[1]]$beta
eta_orig <- obj$fit[[1]]$eta

png_open("fig_baseline.png")
plot(obj, main = "", xlab = "Operating Time", ylab = "Unreliability")
dev.off()

## ---- Developmental Crow-AMSAA growth model (paper Fig 2) ------------------
set.seed(3)
N_dev <- 30
g_exp <- 0.8
eta_build <- eta_matured * (seq_len(N_dev) / N_dev)^g_exp
dev_life <- eta_build * (-log(runif(N_dev)))^(1 / beta_mech)
dev_cum <- cumsum(dev_life)

dev_fit <- rga(
  data.frame(times = dev_cum, failures = rep(1, N_dev)),
  times_type = "cumulative_failure_times"
)
growth_beta <- as.numeric(dev_fit$betas)
dev_r2 <- summary(dev_fit$model)$r.squared

png_open("fig_rga.png")
plot(dev_fit, main = "", xlab = "Cumulative Test Time", ylab = "Cumulative Failures")
dev.off()

## ---- Forecast quantities --------------------------------------------------
forecast_window <- 400
test_end_cum_time <- sum(c(failures, suspensions))
expected_failures <- function(w, eta, r, beta) {
  sum(1 - exp(-(((r + w) / eta)^beta - (r / eta)^beta)))
}
solve_eta <- function(target, r, beta, window) {
  uniroot(function(eta) expected_failures(window, eta, r, beta) - target,
    interval = c(1e-3, 1e9))$root
}
count_ratio <- function(bg, rho) ((1 + rho)^bg - 1) / rho

f0 <- expected_failures(forecast_window, eta_orig, rep(test_end, n_surviving), sim_beta)
rho <- (n_surviving * forecast_window) / test_end_cum_time
n_expected <- f0 * count_ratio(growth_beta, rho)
n_forecast <- round(n_expected)
cval <- count_ratio(growth_beta, rho)

## ---- Simulate forecast failures & combine ---------------------------------
set.seed(123)
sim_eta <- solve_eta(n_expected, rep(test_end, n_surviving), sim_beta, forecast_window)
sim_result <- sim_failures(n = n_forecast, runtimes = rep(1000, n_surviving),
  window = forecast_window, beta = sim_beta, eta = sim_eta)
sim_fail_times <- sim_result$runtime[sim_result$type == "Failure"]
sim_susp_times <- sim_result$runtime[sim_result$type == "Suspension"]
combined_failures <- c(failures, sim_fail_times)
combined_suspensions <- sim_susp_times

## ---- Weibull comparison (paper Fig 4) -------------------------------------
obj_nogrowth <- wblr(failures, suspensions, col = BASE, pch = 1, lty = 2,
  label = "Without Growth", is.plot.cb = FALSE)
obj_nogrowth <- wblr.fit(obj_nogrowth, method.fit = "mle")
obj_growth <- wblr(combined_failures, combined_suspensions, col = GROWTH, pch = 16, lty = 1,
  label = "With Growth", is.plot.cb = FALSE)
obj_growth <- wblr.fit(obj_growth, method.fit = "mle")

png_open("fig_comparison.png", w = 1700, h = 1250)
plot.wblr(list(obj_nogrowth, obj_growth), main = "", is.plot.legend = TRUE)
dev.off()

b10 <- function(beta, eta, t0 = 0) t0 + eta * (-log(0.90))^(1 / beta)
nogrowth_wb_beta <- obj_nogrowth$fit[[1]]$beta
nogrowth_wb_eta <- obj_nogrowth$fit[[1]]$eta
growth_wb_beta <- obj_growth$fit[[1]]$beta
growth_wb_eta <- obj_growth$fit[[1]]$eta
nogrowth_b10 <- b10(nogrowth_wb_beta, nogrowth_wb_eta)
growth_b10 <- b10(growth_wb_beta, growth_wb_eta)

## ---- Count-ratio curve (slide 8) ------------------------------------------
png_open("fig_countratio.png", w = 1500, h = 1050)
bg_seq <- seq(0.4, 1.2, length.out = 200)
cc <- count_ratio(bg_seq, rho)
plot(bg_seq, cc, type = "l", lwd = 3, col = GROWTH,
  xlab = expression("Growth parameter " * beta[g]),
  ylab = expression("Count-ratio " * c(beta[g])), main = "")
abline(h = 1, col = BASE, lty = 2, lwd = 2)
abline(v = 1, col = BASE, lty = 3, lwd = 1.5)
points(1, 1, pch = 19, col = ACCENT, cex = 1.6)
text(1.0, 1.0, "no growth\nc(1) = 1", pos = 4, offset = 0.7, col = GROWTH, cex = 0.9)
points(growth_beta, cval, pch = 17, col = ACCENT, cex = 1.7)
text(growth_beta, cval, sprintf("case study\nc = %.2f", cval), pos = 1, offset = 0.9,
  col = GROWTH, cex = 0.9)
dev.off()

## ---- Monte Carlo (slide 12) -----------------------------------------------
sim_and_fit <- function(test_failures, target_exp, runtimes, window, beta) {
  eta_c <- solve_eta(target_exp, runtimes, beta, window)
  sim_i <- sim_failures(n = round(target_exp), runtimes = runtimes, window = window,
    beta = beta, eta = eta_c)
  sim_f <- sim_i$runtime[sim_i$type == "Failure"]
  sim_s <- sim_i$runtime[sim_i$type == "Suspension"]
  obj_i <- wblr.fit(wblr(c(test_failures, sim_f), sim_s, is.plot.cb = FALSE), method.fit = "mle")
  data.frame(beta = obj_i$fit[[1]]$beta, eta = obj_i$fit[[1]]$eta)
}

set.seed(99)
n_mc <- 500
betag_hat <- as.numeric(dev_fit$betas)
betag_se <- as.numeric(dev_fit$betas_se)
mc_results <- vector("list", n_mc)
for (i in seq_len(n_mc)) {
  tryCatch({
    beta_i <- rnorm(1, betag_hat, betag_se)
    if (beta_i <= 0) stop("bad")
    target_i <- f0 * count_ratio(beta_i, rho)
    if (!is.finite(target_i) || target_i <= 0) stop("bad")
    mc_results[[i]] <- sim_and_fit(failures, target_i, rep(1000, n_surviving), forecast_window, sim_beta)
  }, error = function(e) NULL)
}
mc_df <- do.call(rbind, Filter(Negate(is.null), mc_results))
mc_df$b10 <- b10(mc_df$beta, mc_df$eta)
mc_ci <- quantile(mc_df$b10, c(0.025, 0.975))
mc_med <- median(mc_df$b10)
mc_ci_beta <- quantile(mc_df$beta, c(0.025, 0.975))
mc_med_beta <- median(mc_df$beta)

png_open("fig_mc.png", w = 1600, h = 1050)
h <- hist(mc_df$beta, breaks = 30, plot = FALSE)
plot(h, col = "#D9DEE5", border = "white", main = "",
  xlab = expression("Growth-adjusted Weibull shape  " * beta), ylab = "Frequency",
  xlim = range(c(h$breaks, nogrowth_wb_beta)))
abline(v = nogrowth_wb_beta, col = BASE, lwd = 3, lty = 2)
abline(v = mc_med_beta, col = GROWTH, lwd = 3)
abline(v = mc_ci_beta, col = ACCENT, lwd = 2, lty = 3)
legend("topright", bty = "n", cex = 0.85,
  legend = c(sprintf("No-growth baseline (%.2f)", nogrowth_wb_beta),
             sprintf("MC median (%.2f)", mc_med_beta),
             sprintf("95%% band (%.2f-%.2f)", mc_ci_beta[1], mc_ci_beta[2])),
  col = c(BASE, GROWTH, ACCENT), lwd = c(3, 3, 2), lty = c(2, 1, 3))
dev.off()

## ---- Sensitivity: growth strength (slide 13, paper Fig 6 eta panel) -------
growth_scenarios <- c(0.4, 0.6, 0.8, 1.0)
growth_labels <- c("0.4\nstrong", "0.6\nmoderate", "0.8\nmild", "1.0\nnone")
set.seed(77)
n_mc_sens <- 200
sens_growth_list <- lapply(seq_along(growth_scenarios), function(k) {
  gb <- growth_scenarios[k]
  target_k <- f0 * count_ratio(gb, rho)
  if (target_k <= 0) return(NULL)
  rows <- lapply(seq_len(n_mc_sens), function(i) {
    tryCatch({
      r <- sim_and_fit(failures, target_k, rep(1000, n_surviving), forecast_window, sim_beta)
      cbind(scenario = growth_labels[k], r)
    }, error = function(e) NULL)
  })
  do.call(rbind, Filter(Negate(is.null), rows))
})
sens_growth_df <- do.call(rbind, Filter(Negate(is.null), sens_growth_list))
sens_growth_df$scenario <- factor(sens_growth_df$scenario, levels = growth_labels)

png_open("fig_sensitivity.png", w = 1600, h = 1100)
par(mar = c(5, 4.4, 1.2, 1.2))
boxplot(eta ~ scenario, data = sens_growth_df,
  col = c(GROWTH, "#D06A62", "#B7A7A0", BASE), outline = FALSE,
  border = "#2A3442",
  xlab = expression("Growth strength (" * beta[g] * ")"),
  ylab = expression("Fitted " * hat(eta)), main = "")
abline(h = nogrowth_wb_eta, col = ACCENT, lwd = 2, lty = 2)
text(4, nogrowth_wb_eta, "baseline", pos = 3, col = ACCENT, cex = 0.85)
dev.off()

## ---- Emit numbers for slide text ------------------------------------------
nums <- list(
  sim_beta = round(sim_beta, 2), eta_orig = round(eta_orig, 0),
  growth_beta = round(growth_beta, 3), dev_r2 = round(dev_r2, 3),
  g_rate = round(1 - growth_beta, 3),
  n_test_failures = n_test_failures, n_surviving = n_surviving,
  f0 = round(f0, 1), cval = round(cval, 2), n_forecast = n_forecast,
  sim_eta = round(sim_eta, 0),
  nogrowth_beta = round(nogrowth_wb_beta, 2), nogrowth_eta = round(nogrowth_wb_eta, 0),
  growth_wb_beta = round(growth_wb_beta, 2), growth_wb_eta = round(growth_wb_eta, 0),
  nogrowth_b10 = round(nogrowth_b10, 0), growth_b10 = round(growth_b10, 0),
  b10_pct = round(100 * (growth_b10 / nogrowth_b10 - 1), 1),
  mc_valid = nrow(mc_df), mc_med_b10 = round(mc_med, 0),
  mc_ci_lo = round(mc_ci[1], 0), mc_ci_hi = round(mc_ci[2], 0),
  mc_med_beta = round(mc_med_beta, 2),
  mc_ci_beta_lo = round(mc_ci_beta[1], 2), mc_ci_beta_hi = round(mc_ci_beta[2], 2)
)
writeLines(jsonlite::toJSON(nums, auto_unbox = TRUE, pretty = TRUE),
  file.path(figdir, "../numbers.json"))
cat("DONE\n")
print(nums)

# Generate the ReliaGrowR hex sticker
#
# Requires: hexSticker, ggplot2
# Usage: source("inst/hex_sticker.R")
#
# Output is saved to man/figures/logo.png

library(ggplot2)
library(hexSticker)

# --- Subplot: reliability growth curve with confidence band ---
set.seed(42)
t <- seq(0.5, 10, length.out = 80)
# Power law: N(t) = lambda * t^beta (beta < 1 = improving system)
y_fit <- 2.5 * t^0.65
y_lo <- y_fit * 0.75
y_hi <- y_fit * 1.30

# Simulated observed data points
t_obs <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y_obs <- 2.5 * t_obs^0.65 + rnorm(10, 0, 0.6)

df_ribbon <- data.frame(t = t, y = y_fit, lo = y_lo, hi = y_hi)
df_pts <- data.frame(t = t_obs, y = y_obs)

p <- ggplot() +
  geom_ribbon(data = df_ribbon, aes(x = t, ymin = lo, ymax = hi),
              fill = "#4FC3F7", alpha = 0.25) +
  geom_line(data = df_ribbon, aes(x = t, y = y),
            color = "#4FC3F7", linewidth = 1.0) +
  geom_point(data = df_pts, aes(x = t, y = y),
             color = "#B3E5FC", size = 1.2) +
  theme_void() +
  theme_transparent()

# --- Create hex sticker ---
sticker(p,
        package = "ReliaGrowR",
        p_size = 17,
        p_color = "#E3F2FD",
        p_y = 1.48,
        s_x = 1.0,
        s_y = 0.75,
        s_width = 1.2,
        s_height = 0.85,
        h_fill = "#0D1B3E",
        h_color = "#4FC3F7",
        h_size = 1.6,
        url = "paulgovan.github.io/ReliaGrowR",
        u_size = 3.5,
        u_color = "#64B5F6",
        filename = "man/figures/logo.png")

message("Hex sticker saved to man/figures/logo.png")

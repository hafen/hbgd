# plots for comparing groups

# #' Plot comparison of two distributions with overlaid histograms
# #'
# #' @export
# plot_hist_compare <- function(dist1, dist2, breaks = "Sturges", xlab = "", alpha = 0.9) {
#   if (!inherits(dist1, "histogram"))
#     dist1 <- hist(dist1, breaks = breaks, plot = FALSE)

#   if (!inherits(dist2, "histogram"))
#     dist2 <- hist(dist2, breaks = dist1$breaks, plot = FALSE)

#   hst_dat <- data.frame(
#     x_start = dist1$breaks[-length(dist1$breaks)],
#     x_end = dist1$breaks[-1],
#     frequency = dist1$counts,
#     cur_freq = dist2$counts,
#     cur_pct = dist2$counts / dist1$counts,
#     percent = round(dist2$counts / dist1$counts * 100),
#     alpha = alpha
#   )

#   p1 <- figure(xaxes = FALSE, height = 200, min_border_left = 70, min_border_right = 2, min_border_top = 2, min_border_bottom = 2, ylab = "Frequency") %>%
#     ly_rect(x_start, 0, x_end, frequency, data = hst_dat, fill_color = "#CCCCCC", line_color = "#AAAAAA", fill_alpha = alpha) %>%
#     ly_rect(x_start, 0, x_end, cur_freq, data = hst_dat, color = "red", alpha = alpha)

#   p2 <- figure(height = 200, min_border_left = 70, min_border_right = 2, min_border_top = 2, min_border_bottom = 2, ylab = "Proportion", xlab = xlab) %>%
#     ly_rect(x_start, 0, x_end, 1, data = hst_dat, fill_color = "#CCCCCC", line_color = "#AAAAAA", fill_alpha = alpha,
#       # hover = percent
#       ) %>%
#     ly_rect(x_start, 0, x_end, cur_pct, data = hst_dat, color = "red", alpha = alpha)

#   grid_plot(list(p1, p2), same_axes = c(TRUE, FALSE), ncol = 1)
# }

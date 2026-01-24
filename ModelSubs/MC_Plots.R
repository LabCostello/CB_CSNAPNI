# MC_Plots.R
# Visualization of Monte Carlo Uncertainty Analysis Results

if(print_tags == 1){
  print("ModelSubs/MC_Plots.R")
}

require(ggplot2)
require(patchwork)

# ============================================================================
# UNCERTAINTY DISTRIBUTION PLOTS
# ============================================================================

# Function to create uncertainty ribbon plot
plot_uncertainty_ribbon <- function(mc_data, summary_data, title, ylab) {
  
  df <- data.frame(
    Year = year_labels,
    Mean = summary_data$mean,
    Median = summary_data$median,
    Q025 = summary_data$q025,
    Q975 = summary_data$q975,
    Q05 = summary_data$q05,
    Q95 = summary_data$q95
  )
  
  p <- ggplot(df, aes(x = Year)) +
    # 95% confidence interval
    geom_ribbon(aes(ymin = Q025, ymax = Q975), 
                fill = "lightblue", alpha = 0.4) +
    # 90% confidence interval
    geom_ribbon(aes(ymin = Q05, ymax = Q95), 
                fill = "steelblue", alpha = 0.5) +
    # Mean line
    geom_line(aes(y = Mean), color = "darkblue", size = 1.2) +
    # Median line
    geom_line(aes(y = Median), color = "red", size = 0.8, linetype = "dashed") +
    labs(title = title,
         x = "Year",
         y = ylab,
         caption = "Dark ribbon: 90% CI, Light ribbon: 95% CI\nBlue solid: Mean, Red dashed: Median") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.caption = element_text(hjust = 0, size = 8))
  
  return(p)
}

# Create plots for each major output

p1 <- plot_uncertainty_ribbon(
  mc_results$NANI_total,
  mc_summary$NANI_total,
  "Total NANI Uncertainty",
  "Total NANI (kg N)"
)

p2 <- plot_uncertainty_ribbon(
  mc_results$NANI_fert,
  mc_summary$NANI_fert,
  "NANI from Fertilizer Uncertainty",
  "Fertilizer N Input (kg N)"
)

p3 <- plot_uncertainty_ribbon(
  mc_results$NANI_fix,
  mc_summary$NANI_fix,
  "NANI from Fixation Uncertainty",
  "N Fixation (kg N)"
)

p4 <- plot_uncertainty_ribbon(
  mc_results$TN_total,
  mc_summary$TN_total,
  "Total Nitrogen Uncertainty",
  "Total N (kg N)"
)

# Combine plots
combined_plot <- (p1 + p2) / (p3 + p4)

# Save combined plot
ggsave("OutputFiles_CBW/MonteCarlo/uncertainty_ribbons.png",
       combined_plot,
       width = 14, height = 10, dpi = 300)

cat("  Saved: uncertainty_ribbons.png\n")

# ============================================================================
# HISTOGRAM PLOTS FOR LATEST YEAR
# ============================================================================

# Create histogram for final year NANI
last_year_idx <- nyrs

df_hist <- data.frame(
  NANI = mc_results$NANI_total[, last_year_idx]
)

p_hist <- ggplot(df_hist, aes(x = NANI)) +
  geom_histogram(aes(y = ..density..), bins = 50, 
                 fill = "steelblue", alpha = 0.7, color = "black") +
  geom_density(color = "red", size = 1.2) +
  geom_vline(xintercept = mc_summary$NANI_total$mean[last_year_idx],
             color = "darkblue", linetype = "solid", size = 1) +
  geom_vline(xintercept = mc_summary$NANI_total$q025[last_year_idx],
             color = "blue", linetype = "dashed", size = 0.8) +
  geom_vline(xintercept = mc_summary$NANI_total$q975[last_year_idx],
             color = "blue", linetype = "dashed", size = 0.8) +
  labs(title = sprintf("Total NANI Distribution (%d)", year_labels[last_year_idx]),
       subtitle = sprintf("Mean: %.2e ± %.2e kg N (95%% CI: [%.2e, %.2e])",
                          mc_summary$NANI_total$mean[last_year_idx],
                          mc_summary$NANI_total$sd[last_year_idx],
                          mc_summary$NANI_total$q025[last_year_idx],
                          mc_summary$NANI_total$q975[last_year_idx]),
       x = "Total NANI (kg N)",
       y = "Density",
       caption = "Blue solid: Mean, Blue dashed: 95% CI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

ggsave("OutputFiles_CBW/MonteCarlo/NANI_distribution_latest_year.png",
       p_hist,
       width = 10, height = 7, dpi = 300)

cat("  Saved: NANI_distribution_latest_year.png\n")

# ============================================================================
# COEFFICIENT OF VARIATION PLOT
# ============================================================================

# Plot CV over time to see how uncertainty changes
df_cv <- data.frame(
  Year = year_labels,
  NANI_CV = mc_summary$NANI_total$cv * 100,
  Fert_CV = mc_summary$NANI_fert$cv * 100,
  Fix_CV = mc_summary$NANI_fix$cv * 100,
  TN_CV = mc_summary$TN_total$cv * 100
)

df_cv_long <- tidyr::pivot_longer(df_cv, cols = -Year, 
                                  names_to = "Variable", 
                                  values_to = "CV_percent")

p_cv <- ggplot(df_cv_long, aes(x = Year, y = CV_percent, color = Variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Coefficient of Variation Over Time",
       x = "Year",
       y = "Coefficient of Variation (%)",
       color = "Variable") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

ggsave("OutputFiles_CBW/MonteCarlo/coefficient_of_variation.png",
       p_cv,
       width = 10, height = 6, dpi = 300)

cat("  Saved: coefficient_of_variation.png\n")

# ============================================================================
# TORNADO PLOT (SENSITIVITY ANALYSIS)
# ============================================================================
# This shows which parameters contribute most to output uncertainty

# For the latest year, calculate correlation between each input parameter
# and the total NANI output

# Note: This requires storing sampled parameter values, which we can add later
# For now, we'll create a placeholder

cat("  Note: Tornado/sensitivity plots require parameter value storage\n")
cat("        This can be added in a future enhancement\n")

# ============================================================================
# BOX PLOTS BY YEAR
# ============================================================================

# Create box plots showing distribution for each year
df_box <- data.frame(
  Year = rep(year_labels, each = n_iterations),
  NANI = as.vector(mc_results$NANI_total)
)

df_box$Year <- factor(df_box$Year)

p_box <- ggplot(df_box, aes(x = Year, y = NANI)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(title = "Total NANI Distribution by Year",
       x = "Year",
       y = "Total NANI (kg N)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("OutputFiles_CBW/MonteCarlo/NANI_boxplots_by_year.png",
       p_box,
       width = 10, height = 6, dpi = 300)

cat("  Saved: NANI_boxplots_by_year.png\n")

cat("\nAll Monte Carlo visualization plots generated successfully!\n")
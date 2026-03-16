library(dplyr)
library(ggplot2)
library(readr)

# 1. Load Data
tuning_results <- read_csv("results/alpha_curve.csv")
best_alpha_row <- tuning_results[which.max(tuning_results$loglik), ]

# 2. CALCULATE THE ZOOM
# We find the 'peak' and look only at the top 2000 units
# This removes the 'empty sky' and makes the 500-unit gaps visible
peak_y <- max(tuning_results$loglik)
y_limit_lower <- peak_y - 2500  # Zooms in on the interesting part
y_limit_upper <- peak_y + 250   # Gives a little breathing room at the top

# 3. Create the Plot
p <- ggplot(tuning_results, aes(x = alpha, y = loglik)) +
  # Shading only the zoomed area
  geom_area(fill = "#3498db", alpha = 0.1) +
  geom_line(color = "#2980b9", size = 1.2) + 
  geom_point(color = "#2c3e50", size = 3) +
  
  # Highlight the Winner
  geom_point(data = best_alpha_row, aes(x = alpha, y = loglik), 
             color = "#e74c3c", size = 5) +
  geom_vline(xintercept = best_alpha_row$alpha, linetype = "dashed", color = "#e74c3c") +
  
  # THE ZOOM FIX: coord_cartesian keeps the labels clean
  coord_cartesian(ylim = c(y_limit_lower, y_limit_upper)) +
  
  # Fixed 500-unit gaps
  scale_y_continuous(breaks = seq(floor(y_limit_lower/500)*500, 
                                  ceiling(y_limit_upper/500)*500, 
                                  by = 500)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#f0f0f0"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray30")
  ) +
  labs(
    title = "Alpha Parameter Optimization",
    subtitle = paste0("Focusing on the convergence plateau | Best Alpha = ", best_alpha_row$alpha),
    x = "Alpha values",
    y = "Log-Likelihood (Model Fit)"
  )

ggsave("figures/alpha_tuning_curve.png", p, width = 10, height = 6, dpi = 300)




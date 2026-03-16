library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)

# 1. Load and Calculate Statistics
boot <- read_csv("results/bootstrap_coefficients.csv") %>%
  filter(error_flag == FALSE)

# Identify the 17 physical predictors we care about
physical_list <- c(
  "particle_polygon_area", "particle_polygon_length", "convex_hull_area",
  "convex_hull_length", "maximum_dimension_length", "maximum_dimension_angle",
  "paris_factor", "area_delta_factor", "angularity", "min_rect_min_length",
  "min_rect_min_length_angle", "min_rect_max_length", "min_rect_max_length_angle",
  "enclose_length_delta", "particle_area", "particle_weight", "particle_density"
)

importance_summary <- boot %>%
  # Focus only on the physical predictors
  select(any_of(physical_list)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    mean_coef = mean(value),
    lower_ci = quantile(value, 0.025),
    upper_ci = quantile(value, 0.975),
    .groups = "drop"
  ) %>%
  # Determine if the factor is statistically significant
  mutate(
    is_significant = ifelse(lower_ci > 0 | upper_ci < 0, "Significant", "Insignificant"),
    clean_name = str_to_title(str_replace_all(variable, "_", " "))
  ) %>%
  arrange(desc(abs(mean_coef)))

# 2. Create the Aesthetic Barplot
p <- ggplot(importance_summary, 
            aes(x = reorder(clean_name, abs(mean_coef)), 
                y = mean_coef, 
                fill = is_significant)) +
  # Main Bars
  geom_col(color = "black", width = 0.7, alpha = 0.8, size = 0.3) +
  # 95% Confidence Interval "Whiskers"
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0.2, color = "black", size = 0.5) +
  # Zero reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.7) +
  # Flip for readability
  coord_flip() +
  # Professional Colors
  scale_fill_manual(values = c("Significant" = "#2c7bb6", "Insignificant" = "#d9d9d9")) +
  # Clean Academic Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(color = "black", face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 15)),
    legend.position = "bottom"
  ) +
  labs(
    title = "Physical Drivers of Shaking Table Separation",
    subtitle = "Mean Coefficients with 95% Confidence Intervals (1,000 Bootstrap Runs)",
    x = "Physical Particle Feature",
    y = "Impact Strength (Standardized Coefficient)",
    fill = "Statistical Result:"
  )

# 3. Save
if(!dir.exists("figures")) dir.create("figures")
ggsave("figures/predictor_importance_pro.png", p, width = 11, height = 9, dpi = 300)

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)

# 1. Load Data
boot <- read_csv("results/bootstrap_coefficients.csv") %>%
  filter(error_flag == FALSE)

# 2. THE MASTER LIST (All 17 you requested)
physical_list <- c(
  "particle_polygon_area", "particle_polygon_length", "convex_hull_area",
  "convex_hull_length", "maximum_dimension_length", "maximum_dimension_angle",
  "paris_factor", "area_delta_factor", "angularity", "min_rect_min_length",
  "min_rect_min_length_angle", "min_rect_max_length", "min_rect_max_length_angle",
  "enclose_length_delta", "particle_area", "particle_weight", "particle_density"
)

# 3. Clean and Prepare Data
# We clean names but keep EVERYTHING in the list
boot_long <- boot %>%
  select(-iteration, -error_flag, -any_of("error_message")) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(clean_name = str_to_title(str_replace_all(variable, "[_:]", " ")))

# --- IMAGE 1: THE 17 PHYSICAL PREDICTORS (FORCED) ---
message("Generating Image 1: 17 Physical Predictors...")

# We match against your list specifically
phys_data <- boot_long %>% 
  filter(variable %in% physical_list)

p1 <- ggplot(phys_data, aes(x = value)) +
  geom_histogram(bins = 30, fill = "#2c7bb6", color = "white", linewidth = 0.2) +
  # 6 rows x 3 columns fits 17-18 plots perfectly on a page
  facet_wrap(~clean_name, scales = "free", ncol = 3) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#f8f9fa"),
    strip.text = element_text(face = "bold", size = 9),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Physical Particle Drivers: Bootstrap Distributions",
    subtitle = "1,000 Iterations | All 17 requested predictors included",
    x = "Coefficient Value",
    y = "Frequency (Count)"
  )

# --- IMAGE 2: THE INTERCEPTS (THRESHOLD BOUNDARIES) ---
message("Generating Image 2: 3 Model Intercepts...")

int_data <- boot_long %>% 
  filter(str_detect(variable, "Intercept"))

p2 <- ggplot(int_data, aes(x = value)) +
  geom_histogram(bins = 30, fill = "#d35400", color = "white", linewidth = 0.2) +
  facet_wrap(~clean_name, scales = "free", ncol = 1) + # Stacked vertically
  theme_bw(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#fff5eb"),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
  ) +
  labs(
    title = "Model Intercepts (Class Thresholds)",
    subtitle = "Distributions of category boundaries",
    x = "Intercept Value", 
    y = "Frequency (Count)"
  )

# --- SAVE AS TWO SEPARATE FILES ---
if(!dir.exists("figures")) dir.create("figures")

# Save Physical Predictors (Tall format)
ggsave("figures/01_physical_predictors.png", p1, width = 14, height = 18, dpi = 300)

# Save Intercepts (Square format)
ggsave("figures/02_model_intercepts.png", p2, width = 10, height = 10, dpi = 300)



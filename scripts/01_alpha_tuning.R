# ================================
# 1. LIBRARIES
# ================================

library(dplyr)
library(readr)
library(janitor)
library(ordinalNet)

set.seed(42)

# ================================
# 2. DATA LOADING FUNCTION
# ================================

process_stream <- function(comp_path, shape_path) {
  
  comp <- read_csv(comp_path, show_col_types = FALSE) %>% clean_names()
  shape <- read_csv(shape_path, show_col_types = FALSE) %>% clean_names()
  
  comp_clean <- comp %>%
    group_by(particle_id) %>%
    summarise(
      particle_area   = first(particle_area),
      particle_weight = first(particle_weight),
      .groups = "drop"
    ) %>%
    mutate(
      particle_density = ifelse(particle_area == 0,
                                NA,
                                particle_weight / particle_area)
    )
  
  shape %>%
    left_join(comp_clean, by = "particle_id")
}

# ================================
# 3. LOAD TRAINING STREAMS
# ================================

train_data <- bind_rows(
  process_stream("data/S15_Particle_Composition.csv",
                 "data/S15_Particle_Shape_Factors.csv") %>%
    mutate(d_class = factor("very_high",
                            levels = c("low","medium","high","very_high"),
                            ordered = TRUE)),
  
  process_stream("data/S16_Particle_Composition.csv",
                 "data/S16_Particle_Shape_Factors.csv") %>%
    mutate(d_class = factor("medium",
                            levels = c("low","medium","high","very_high"),
                            ordered = TRUE)),
  
  process_stream("data/S17_Particle_Composition.csv",
                 "data/S17_Particle_Shape_Factors.csv") %>%
    mutate(d_class = factor("low",
                            levels = c("low","medium","high","very_high"),
                            ordered = TRUE)),
  
  process_stream("data/S19_Particle_Composition.csv",
                 "data/S19_Particle_Shape_Factors.csv") %>%
    mutate(d_class = factor("high",
                            levels = c("low","medium","high","very_high"),
                            ordered = TRUE))
)

# ================================
# 4. PREPARE MATRICES
# ================================

numeric_cols <- train_data %>%
  select(where(is.numeric)) %>%
  select(-particle_id) %>%
  colnames()

x_train <- scale(as.matrix(train_data[numeric_cols]))
y_train <- train_data$d_class

# ================================
# 5. ALPHA TUNING
# ================================

message("Starting Alpha Tuning...")

alpha_grid <- seq(0, 1, by = 0.1)
cv_errors <- numeric(length(alpha_grid))

for (i in seq_along(alpha_grid)) {
  
  cv_fit <- ordinalNetCV(
    x = x_train,
    y = y_train,
    alpha = alpha_grid[i],
    nFolds = 5,
    family = "cumulative",
    link = "logit"
  )
  
  cv_errors[i] <- min(cv_fit$misclassification)
}

best_alpha <- alpha_grid[which.min(cv_errors)]

message(paste("Best Alpha:", best_alpha))

# Save results
write.csv(
  data.frame(alpha = alpha_grid, cv_error = cv_errors),
  "results/alpha_curve.csv",
  row.names = FALSE
)

saveRDS(best_alpha, "results/best_alpha.rds")
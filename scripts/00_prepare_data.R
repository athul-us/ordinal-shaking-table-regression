library(dplyr)
library(tidyr)

source("scripts/utils_data_processing.R")

message("===================================")
message("DATA PREPARATION STARTED")
message("===================================")

train_data <- load_training_data()

message("Initial Rows: ", nrow(train_data))

# --- NEW CLEANING STEP ---
message("Cleaning data and removing NAs...")
train_data_clean <- train_data %>% 
  drop_na() %>%
  # Remove any non-predictive ID or Index columns
  select(-matches("id|index|particle_id|row_num"))

message("Rows after NA removal: ", nrow(train_data_clean))

# Separate features and target
x_matrix <- train_data_clean %>% select(where(is.numeric)) %>% as.matrix()
y_train  <- as.numeric(train_data_clean$d_class)

# --- HANDLE ZERO-VARIANCE ---
# If a column has no variation, scale() creates NaNs. We must remove them.
column_variances <- apply(x_matrix, 2, var, na.rm = TRUE)
keep_cols <- column_variances > 0

if(sum(!keep_cols) > 0) {
  message("Removing ", sum(!keep_cols), " constant columns (zero variance)...")
  x_matrix <- x_matrix[, keep_cols]
}

message("Final Feature Count: ", ncol(x_matrix))

# --- SCALE ---
x_train <- scale(x_matrix)

# Final Sanity Check
if(any(is.na(x_train)) | any(is.infinite(x_train))) {
  stop("CRITICAL ERROR: x_train contains NA or Inf after scaling!")
}

scale_center <- attr(x_train, "scaled:center")
scale_scale  <- attr(x_train, "scaled:scale")

message("Saving processed data...")
saveRDS(x_train, "results/x_train.rds")
saveRDS(y_train, "results/y_train.rds")
saveRDS(scale_center, "results/scale_center.rds")
saveRDS(scale_scale, "results/scale_scale.rds")

message("DATA PREPARATION COMPLETED")

library(ordinalNet)

message("===================================")
message("FINAL MODEL FITTING STARTED")
message("===================================")

# Load data
x_train <- readRDS("results/x_train.rds")
y_train <- readRDS("results/y_train.rds")

# Ensure correct type
y_train <- factor(y_train, ordered = TRUE)

best_alpha <- readRDS("results/best_alpha.rds")

message(paste("Best alpha:", best_alpha))

# Fit final model
final_model <- ordinalNet(
  x = x_train,
  y = y_train,
  alpha = best_alpha,
  family = "cumulative",
  link = "logit"
)

# Save model
saveRDS(final_model, "results/final_model.rds")

message("===================================")
message("FINAL MODEL FITTING COMPLETED")
message("===================================")

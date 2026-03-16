library(ordinalNet)
library(doParallel)
library(foreach)

message("===================================")
message("BOOTSTRAP STARTED - 1000 ITERATIONS")
message("===================================")

set.seed(42)

# 1. Load Data
x_train <- readRDS("results/x_train.rds")
y_train <- readRDS("results/y_train.rds")

# --- CRITICAL FIX: Ensure y is an ordered factor ---
y_train <- factor(y_train, ordered = TRUE)

best_alpha <- readRDS("results/best_alpha.rds")

message(paste("Best alpha being used:", best_alpha))
message(paste("Data size:", nrow(x_train), "rows x", ncol(x_train), "cols"))

# 2. Setup Cluster
# Ensure your PBS file requests 16 CPUs for these 15 workers
n_workers <- 15
cl <- makeCluster(n_workers)
registerDoParallel(cl)
clusterSetRNGStream(cl, 42)
clusterEvalQ(cl, library(ordinalNet))

# Export variables to workers
clusterExport(
  cl,
  varlist = c("x_train", "y_train", "best_alpha"),
  envir = environment()
)

n_boot <- 1000
message(paste("Starting", n_boot, "iterations..."))

# 3. Parallel Bootstrap Loop
bootstrap_results <- foreach(
  i = 1:n_boot,
  .combine = rbind,
  .packages = "ordinalNet",
  .errorhandling = "pass"
) %dopar% {
  
  # Log progress every 50 iterations to the output file
  if (i %% 50 == 0) {
    message(paste("Completed iteration:", i))
  }
  
  result <- tryCatch({
    
    # Resample with replacement
    idx <- sample(seq_len(nrow(x_train)), replace = TRUE)
    
    # Fit model
    fit <- ordinalNet(
      x = x_train[idx, ],
      y = y_train[idx],
      alpha = best_alpha,
      family = "cumulative",
      link = "logit"
    )
    
    # Extract coefficients with names
    co <- as.data.frame(t(as.matrix(coef(fit))))
    co$iteration <- i
    co$error_flag <- FALSE
    
    co
    
  }, error = function(e) {
    # If a specific iteration fails, return a row with an error flag
    # This prevents the whole 1000-run job from crashing
    data.frame(
      iteration = i,
      error_flag = TRUE,
      error_message = as.character(e$message)
    )
  })
  
  result
}

stopCluster(cl)

# 4. Save Results
message("Saving results to results/bootstrap_coefficients.csv...")
write.csv(
  bootstrap_results,
  "results/bootstrap_coefficients.csv",
  row.names = FALSE
)

message("===================================")
message("BOOTSTRAP COMPLETED SUCCESSFULLY")
message("===================================")


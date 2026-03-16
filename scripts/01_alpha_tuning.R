library(ordinalNet)
library(doParallel)
library(foreach)

message("===================================")
message("ALPHA TUNING STARTED - FINAL")
message("===================================")

set.seed(42)

# Load prepared data
x_train <- readRDS("results/x_train.rds")
y_train <- readRDS("results/y_train.rds")

# Ensure response is ordered factor
if(!is.factor(y_train)){
  y_train <- factor(y_train, ordered = TRUE)
}

message(paste("Rows:", nrow(x_train)))
message(paste("Columns:", ncol(x_train)))

# Safety checks
if(any(is.na(x_train))) stop("ERROR: NA values found in predictors.")
if(any(is.na(y_train))) stop("ERROR: NA values found in response.")

# Detect constant columns
zero_var <- which(apply(x_train,2,var)==0)
if(length(zero_var) > 0){
  stop(paste("ERROR: Constant predictors detected:", paste(zero_var, collapse=", ")))
}

# Alpha grid
alpha_grid <- seq(0,1,by=0.1)
message(paste("Total alpha values:", length(alpha_grid)))

# Start cluster
n_workers <- 11
cl <- makeCluster(n_workers)
registerDoParallel(cl)

clusterEvalQ(cl, library(ordinalNet))

clusterExport(cl,
              varlist=c("x_train","y_train"),
              envir=environment())

results <- foreach(a = alpha_grid,
                   .combine = rbind,
                   .packages = "ordinalNet") %dopar% {

  message(paste("Running alpha:", a))

  cv_fit <- ordinalNetCV(
    x = x_train,
    y = y_train,
    alpha = a,
    nFolds = 5,
    family = "cumulative",
    link = "logit"
  )

  data.frame(
    alpha = a,
    cv_error = min(cv_fit$misclassification),
    loglik = max(cv_fit$loglik)
  )
}

stopCluster(cl)

# Identify best alpha using log-likelihood
best_alpha <- results$alpha[
  which.max(results$loglik)
]

message("-----------------------------------")
message(paste("BEST ALPHA:", best_alpha))
message("-----------------------------------")

# Save results
write.csv(results,
          "results/alpha_curve.csv",
          row.names = FALSE)

saveRDS(best_alpha,
        "results/best_alpha.rds")

message("===================================")
message("ALPHA TUNING COMPLETED")
message("===================================")


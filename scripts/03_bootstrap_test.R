library(ordinalNet)
library(doParallel)

set.seed(42)

x_train <- readRDS("results/x_train.rds")
y_train <- readRDS("results/y_train.rds")

# --- CRITICAL FIX: Add this line ---
y_train <- factor(y_train, ordered = TRUE)

best_alpha <- readRDS("results/best_alpha.rds")

# Matches the ncpus in your PBS (use n_workers-1)
n_workers <- 4 
cl <- makeCluster(n_workers)
registerDoParallel(cl)
clusterSetRNGStream(cl, 42)
clusterEvalQ(cl, library(ordinalNet))

n_boot <- 5

bootstrap_results <- foreach(
  i = 1:n_boot,
  .combine = rbind,
  .packages = "ordinalNet",
  .errorhandling = "pass"
) %dopar% {
  
  idx <- sample(seq_len(nrow(x_train)), replace = TRUE)
  
  # --- ADDED tryCatch for stability ---
  fit <- tryCatch({
    ordinalNet(
      x = x_train[idx, ],
      y = y_train[idx],
      alpha = best_alpha,
      family = "cumulative",
      link = "logit"
    )
  }, error = function(e) return(NULL))
  
  if(is.null(fit)) return(NULL)

  co <- as.data.frame(t(as.matrix(coef(fit))))
  co$iteration <- i
  co
}

stopCluster(cl)

write.csv(
  bootstrap_results,
  "results/bootstrap_test.csv",
  row.names = FALSE
)


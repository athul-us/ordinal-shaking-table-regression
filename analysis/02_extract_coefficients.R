library(ordinalNet)

model <- readRDS("results/final_model.rds")

coefs <- as.data.frame(as.matrix(coef(model)))

print(coefs)

write.csv(
  coefs,
  "results/final_model_coefficients.csv"
)


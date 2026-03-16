library(dplyr)
library(readr)

boot <- read_csv("results/bootstrap_coefficients.csv")

boot <- boot %>% filter(error_flag == FALSE)

summary_table <- boot %>%
  select(-iteration, -error_flag) %>%
  summarise(across(
    everything(),
    list(
      mean = ~mean(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      lower = ~quantile(.x, 0.025, na.rm = TRUE),
      upper = ~quantile(.x, 0.975, na.rm = TRUE)
    )
  ))

print(summary_table)

write.csv(
  summary_table,
  "results/bootstrap_summary.csv"
)

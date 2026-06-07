# tests/testthat/helper-data.R
# Minimal reproducible test dataset shared across all test files.

set.seed(42)
n <- 100

test_data <- data.frame(
  ID     = paste0("ID", seq_len(n)),
  gender = sample(c("Male", "Female"), n, replace = TRUE),
  height = c(
    rnorm(50, mean = 176, sd = 6),   # Males  (approx)
    rnorm(50, mean = 163, sd = 6)    # Females (approx)
  ),
  weight = c(
    rnorm(50, mean = 78, sd = 10),
    rnorm(50, mean = 62, sd = 9)
  ),
  phys   = sample(c("None", "Moderate", "Intense", NA),
                  n, replace = TRUE,
                  prob = c(0.25, 0.5, 0.2, 0.05)),
  stringsAsFactors = FALSE
)

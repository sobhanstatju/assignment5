library(readr)
library(dplyr)
library(usethis)

project2026_data <- readr::read_csv(
  "data-raw/project2026.csv",
  show_col_types = FALSE
) |>
  dplyr::select(ID, gender, height, weight, phys) |>
  dplyr::mutate(phys = ifelse(is.na(phys), "None", phys))

usethis::use_data(project2026_data, overwrite = TRUE)

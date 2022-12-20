# Run this to update the DESCRIPTION
imports <- c(
  "magrittr",
  "rlang",
  "cli",
  "dplyr",
  "gsubfn",
  "purrr",
  "tidyr",
  "tibble",
  "furrr",
  "progressr",
  "future",
  "data.table"
)
purrr::walk(imports, usethis::use_package, "Imports")
usethis::use_tidy_description()
rm(imports)

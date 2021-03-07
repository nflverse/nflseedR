# Run this to update the DESCRIPTION
imports <- c(
  "magrittr",
  "rlang",
  "crayon",
  "cli",
  "dplyr",
  "glue",
  "gsubfn",
  "purrr",
  "tidyr",
  "tibble",
  "furrr",
  "progressr",
  "future",
  "readr"
)
purrr::walk(imports, usethis::use_package, "Imports")
usethis::use_tidy_description()
rm(imports)

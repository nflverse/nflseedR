pkg_check <- rcmdcheck::rcmdcheck()

notes <- pkg_check$notes |>
  stringr::str_squish() |>
  stringr::str_extract("(?<=Undefined global functions or variables:).+(?=Consider)") |>
  purrr::pluck(2) |>
  stringr::str_squish() |>
  stringr::str_split(" ", simplify = NA) |>
  unique() |>
  sort()

if(length(notes) == 0){
  cli::cli_alert_success("No tidy eval NOTEs, yay!")
  return(invisible(pkg_check))
}

out <- paste0('"', notes, '"', collapse = ",\n")

paste0(
  'utils::globalVariables(
  package = "', pkg_check$package, '",
  names = c(\n', out, '\n)\n)'
) |>
  cli::cli_code()

#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import data.table
#' @import gsubfn
#' @importFrom cli symbol
#' @importFrom dplyr select mutate rename left_join inner_join n arrange group_by
#' @importFrom dplyr slice bind_rows row_number distinct
#' @importFrom dplyr ungroup filter case_when summarize summarise pull right_join everything
#' @importFrom furrr future_map furrr_options
#' @importFrom future plan
#' @importFrom lifecycle deprecated
#' @importFrom progressr progressor
#' @importFrom purrr pluck
#' @importFrom rlang inform %||%
#' @importFrom stats rnorm setNames
#' @importFrom tibble is_tibble tibble
#' @importFrom tidyr pivot_longer
## usethis namespace: end
NULL

#' @export
nflreadr::load_schedules

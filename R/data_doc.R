#' NFL team names and the conferences and divisions they belong to
#'
#' @docType data
#' @format A data frame with 36 rows and 4 variables containing NFL team level
#' information, including franchises in multiple cities:
#' \describe{
#'   \item{team}{Team abbreviation}
#'   \item{conf}{Conference abbreviation}
#'   \item{division}{Division name}
#'   \item{sdiv}{Division abbreviation}
#' }
#' @examples
#' \donttest{
#' divisions
#' }
#' This data frame is created using the `teams_colors_logos` data frame of the
#' `nflfastR` package. Please see `data-raw/divisions.R` for the code to create
#' this data.
"divisions"

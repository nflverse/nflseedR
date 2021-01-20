#' NFL team names and the conferences and divisions they belong to
#'
#' @docType data
#' @format A data frame with 36 rows and 4 variables containing NFL team level
#' information, including franchises in multiple cities:
#' \describe{
#'   \item{team}{Team abbreviation}
#'   \item{conf}{Complete Team name}
#'   \item{division}{Team id used in the roster function}
#'   \item{sdiv}{Nickname}
#' }
#' This data frame is created using the `teams_colors_logos` data frame of the
#' `nflfastR` package. Please see `data-raw/divisions.R` for the code to create
#' this data.
"divisions"

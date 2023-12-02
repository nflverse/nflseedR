#' Format Numerical Values to Special Percentage Strings
#'
#' @description This function formats numeric vectors with values between 0 and
#' 1 into percentage strings with special specifications. Those specifications
#' are:
#' * 0 and 1 are converted to "0%" and "100%" respectively (takes machine
#' precision into account)
#' * all other values < 0.01 are converted to "<1%"
#' * all other values between 0.01 and 0.995 are rounded to percentages without
#' decimals
#' * values between 0.995 and 0.999 are rounded to percentages with 1 decimal
#' * values between 0.999 and 1 are converted to ">99.9%" unless closer to 1
#' than machine precision.
#'
#' @param x A vector of numerical values
#'
#' @return A character vector
#' @export
#'
#' @examples
#' x <- c(0, 0.004, 0.009, 0.011, 0.9, 0.98, 0.994,
#'        .995, .9989, .999, .9991, .99999999)
#' fmt <- fmt_pct_special(x)
#' data.frame(x = x, fmt = fmt)
fmt_pct_special <- function(x){
  if(!is.vector(x = x, mode = "numeric")){
    cli::cli_abort("Argument {.arg x} has to be a numeric vector")
  }
  if(any(!x[!is.na(x)] %inrange% list(0L,1L))){
    cli::cli_abort("One or more values in {.arg x} are outside the range between 0 and 1")
  }
  rlang::check_installed("scales", "to format numerical strings.")
  # allocate prefix and accuracy vectors
  prefix <- vector("character", length = length(x))
  accuracy <- vector("numeric", length = length(x))
  # we want to show <1% for every value <0.01, unless very close to 0.
  # To avoid cases of <0%, we need to change those values to something
  # close to 0.01.
  x[x < 0.01 & x > sqrt(.Machine$double.eps)] <- 0.009
  # Analog rounding close to 0 above
  x[x > 0.999 & 1 - x > sqrt(.Machine$double.eps)] <- 0.9991
  # Don't round percentages >99.5 to 100% automatically
  accuracy <- ifelse(x >= 0.995, 0.1, 1)
  # create prefixes for <1% and >99.9%
  prefix[x < 0.01 & x > sqrt(.Machine$double.eps)] <- "<"
  prefix[x > 0.999] <- ">"
  # remove prefix and change accuracy if x is VERY close to 100%
  prefix[1 - x <= sqrt(.Machine$double.eps)] <- ""
  accuracy[1 - x <= sqrt(.Machine$double.eps)] <- 1
  # catch NAs as scales will fail otherwise
  prefix[is.na(prefix)] <- ""
  accuracy[is.na(accuracy)] <- 1
  # now format x using scales
  scales::number(
    x,
    accuracy = accuracy,
    scale = 100,
    prefix = prefix,
    suffix = "%"
  )
}

gt_fmt_pct_special <- function(gt, columns, ...){
  gt::text_transform(
    gt,
    locations = gt::cells_body(columns = {{ columns }}),
    fn = function(x){
      # avoid "NAs introduced by coercion" warning
      x[x == "NA"] <- NA_character_
      x <- as.double(x)
      fmt_pct_special(x)
    }
  )
}

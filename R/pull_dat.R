#' Pull historic data & convert to data table
#'
#' Pull the historic price data of a security and set the resulting data to a
#' `data.table` for future processing
#' @param sym Character vector of length 1 with a security's ticker
#' @param start_date Starting date for which prices should be pulled
#' @param ... any additional arguments to be passed into `riingo::riingo_prices`
#' @export
#' @import data.table

pull_dat <- function(sym, start_date = '1970-01-01', ...) {
  d = riingo::riingo_prices(sym, start_date = start_date, ...)
  setDT(d)
  return(d)
}

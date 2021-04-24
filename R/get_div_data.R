#' Get basic dividend data
#'
#' Make a call using `riingo` to get dividend data on a monthly time scale
#'
#' @param sym character vector of length 1 with a single security's symbol
#' @export
#' @import data.table
get_div_data <- function(sym) {
  d = riingo::riingo_prices(sym, resample_frequency = 'monthly')
  setDT(d)
  d = d[divCash > 0][order(-date)][1:4]
  return(list(sum(d[,divCash]), get_first(d[divCash > 0, divCash])*sum(d[,divCash > 0])))
}

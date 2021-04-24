#' Add years_ahead column
#'
#' Simple helper function to add a years_ahead column using a combined growth metric
#' @param divs a `data.table` with columns for yield_mean and comb_growth
#' @export
#' @import data.table
add_years_ahead = function(divs) {
  divs[,years_ahead := (log(1+yield_mean) / log(1 + comb_growth))]
}

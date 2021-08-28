#' Simple function for calculating the gini
#'
#' Calculates the gini from the lorenz curve
#'
#'
#' @export
#' @import data.table

calc_gini <- function(lorenz,
                      normalize = FALSE,
                      col = NULL) {
  if(normalize) {
    return(NULL)
  }
  d <- copy(lorenz)
  d[,y_avg := (y_cumprop + shift(y_cumprop))/2]
  d[,x_delta := x_cumprop - shift(x_cumprop)]
  return(1 - 2*sum(d[-1,y_avg * x_delta]))
}

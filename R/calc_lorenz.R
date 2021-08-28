#' Simple function for calculating the lorenz curve
#'
#'
#' @export
#' @import data.table

calc_lorenz <- function(d, col, weights = NULL) {
  if(!('data.table' %in% class(d))) setDT(d)
  if(is.null(weights)) {
    dt <- d[,..col]
    dt[,weight := 1]
    weights = 'weight'
  } else {
    cols <- c(col, weights)
    dt <- d[,..cols]
  }
  dt[,to_order := get(col)/get(weights)]
  setkey(dt, to_order)
  dt[,y_cumprop := cumsum(get(col))/sum(get(col))]
  dt[,x_cumprop := cumsum(get(weights))/sum(get(weights))]
  dt <- rbindlist(list(data.table(x_cumprop = 0, y_cumprop = 0),
                       dt[,.(x_cumprop, y_cumprop)]))
  return(dt)
}


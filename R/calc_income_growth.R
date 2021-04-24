#' Calculate annual income growth
#'
#' @param dat a list of `data.table` objects in the format returned by
#'   `get_income_stream`
#' @param period period of growth
calc_income_growth = function(dat, period = 10) {
  start_yield = sapply(dat, FUN = function(x) x[order(date)][1,income]/1000*4)
  end_yield = sapply(dat, FUN = function(x) x[order(-date)][1,income]/1000*4)
  start_yield
  end_yield
  mean(start_yield)
  mean(end_yield)
  individual = sapply(names(start_yield), FUN = function(s) calc_ann_growth(end_yield[s], start_yield[s]))
  names(individual) = names(dat)
  total = calc_ann_growth(mean(end_yield), mean(start_yield))
  return(list(individual = individual,
              total = total))
}

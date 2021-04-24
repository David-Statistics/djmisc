#' Calculate the annualized growth
#'
#' @param end ending amount
#' @param start starting amount
#' @param period time between start and end
#' @export
calc_ann_growth = function(end, start, period = 10) {
  exp((log(end / start)/period))
}

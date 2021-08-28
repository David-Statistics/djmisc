#' Get Historic Price Data & Summaries
#'
#' Pull the historic price information for a set of symbols and show many useful
#' summaries dealing with dividend growth, yield, RSI, and price.
#'
#' @param syms Character vector with symbols to pull data on
#' @param show_progress Show a progress bar if TRUE else no console output
#' @param sleep_time tiingo doesn't appreciate requests coming in extremely
#'   quickly...
#' @param ... Additional arguments to be passed into `riingo::riingo_prices`
#' @export
#' @import data.table
#' @import progress

gather_data <- function(syms, show_progress = TRUE, sleep_time = .25, ...) {
  if(length(syms) > 1 & show_progress) {
    pb = progress_bar$new(format = ':ticker [:bar] :percent in :elapsed eta: :eta',
                          total = length(syms))
  }
  res = lapply(syms, FUN = function(sym) {
    Sys.sleep(sleep_time)
    pb$tick(tokens = list(ticker = sym))
    d = tryCatch(pull_dat(sym, ...),
                 error = function(cond) {
                   return(NULL)
                 })
    if(is.null(d)) return(NULL)
    d = populate_div_yield(d)
    curr_p = d[order(-date)][1,close]
    if(is.null(curr_p)) return(NULL)
    d[,y := floor(lubridate::time_length(lubridate::interval(Sys.Date(), as.Date(date)),"years"))]
    tots = d[divCash > 0 | splitFactor != 1]
    valid_divs = unique(c(tots[divCash > 0][order(-date)][1,divCash],
                          tots[,.N,divCash][N > 1, divCash]))
    tots = tots[splitFactor != 1 | divCash %in% valid_divs]
    tots[,adjRatio := cumprod(splitFactor)]
    tots[, adjDiv := divCash * (adjRatio / max(adjRatio))]
    y_divs = tots[,.(sum(adjDiv) / .N), y]
    year5 = NA
    year10 = NA
    year15 = NA
    momentum = NA
    combined = NA
    if(-5 %in% y_divs[,y]) year5 = (y_divs[y == -1, V1] / y_divs[y == -5, V1])^.2 - 1
    if(-10 %in% y_divs[,y]) year10 = (y_divs[y == -1, V1] / y_divs[y == -10, V1])^.1 - 1
    if(-15 %in% y_divs[,y]) year15 = (y_divs[y == -1, V1] / y_divs[y == -15, V1])^(1/15) - 1
    if(!is.na(year10)) momentum = year5 / year10
    if(!is.na(year5)) {
      grs <- c(year5, year10, year15)
      grs <- grs[!is.na(grs) & !is.infinite(grs)]
      combined = mean(grs, na.rm = TRUE)
    }
    rsi = get_last(TTR::RSI(get_last(d[,close], 30)))
    yield = get_last(d[divCash > 0,divCash]) / curr_p
    yield_perc = mean(d[date > (Sys.Date() - lubridate::years(10)),yield] < yield)
    yield_mean = yield / mean(d[date > (Sys.Date() - lubridate::years(10)),yield]) - 1
    yield = get_last(y_divs[,V1]) / curr_p
    return(list('Ticker' = sym,
                'growth5' = year5,
                'growth10' = year10,
                'growth15' = year15,
                'growth_momentum' = momentum,
                'comb_growth' = combined,
                'rsi' = rsi,
                'yield_perc' = yield_perc,
                'yield_mean' = yield_mean,
                'yield' = yield,
                'tot' = combined + yield))
  })
  return(rbindlist(res))
}

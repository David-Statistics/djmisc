#' Get historical income information from a security
#'
#' Pull the income information (including dividends) from a security for a given
#' period of time and a given starting investment amount. The returned
#' information will give information about the income stream from reinvestment
#' separately.
#' @param sym a character vector with ticker symbols
#' @param orig_date starting date of the investment
#' @param orig_amount starting investment amount
#' @return a list where each element is a `data.table` containing income information
#' @export
#' @import data.table
get_income_stream = function(sym, orig_date = today() - years(10),
                             orig_amount = 1000) {
  if(length(sym) > 1) {
    if(length(orig_amount) == 1) {
      ret = lapply(sym, FUN = function(s) get_income_stream(s, orig_date, orig_amount))
    } else if(length(orig_amount) == length(sym)) {
      ret = lapply(seq_along(sym), FUN = function(i) get_income_stream(sym[i], orig_date, orig_amount[i]))
    } else {
      stop('orig_amount needs to be either length 1 or length(sym)')
    }
    names(ret) = sym
    return(ret)
  }
  stock_dat = pull_dat(sym)

  shares = c(0,0)
  shares[1] = orig_amount / stock_dat[date <= orig_date][order(-date)][1,close]

  div_dat = stock_dat[date > orig_date & (divCash > 0 | splitFactor != 1)]
  res = list(NULL)
  if(sym == 'MKC') {
    if(as.POSIXct(ymd('2020-12-30')) %in% div_dat[,date]) {
      div_dat[date == as.POSIXct(ymd('2020-12-30')), divCash := .35]
    }
  }
  if(sym == 'FRT') {
    if(as.POSIXct(ymd('2012-12-28')) %in% div_dat[,date]) {
      div_dat = div_dat[date != as.POSIXct(ymd('2012-12-28'))]
    }
  }
  for(i in seq_len(nrow(div_dat))) {
    if(div_dat[i,splitFactor] != 1) {
      shares = shares * div_dat[i, splitFactor]
      next
    }
    ret = list(div_dat[i,date], div_dat[i,divCash], shares[1], shares[2])
    income = sum(div_dat[i,divCash] * shares)
    if(div_dat[i,divCash] > (.05 * div_dat[i,close])) next
    shares[2] = shares[2] + income / div_dat[i,close]
    res[[i]] = ret
  }
  res = rbindlist(res)
  setnames(res, c('date', 'div', 'orig', 'reinvest'))
  res[,c("orig_income", "reinvest_income") :=
        .(orig * div, reinvest * div)]
  res[,income := orig_income + reinvest_income]
  return(res)
}

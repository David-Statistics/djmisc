#' General function for getting performance information from transactions
#'
#' Summarize all of my transactions to easily see both retrospective and prospective performance metrics. Depends on a specfic set of column names so I'm not going to spend a bunch of time documenting this just yet.
#' @export
#' @import data.table

anaylze_portfolio <- function(transactions, rounding = 4, updates = NULL) {
  stocks = dcast(transactions, Ticker ~ ., fun.aggregate = sum,
                 value.var = c('Volume', 'Total_Cost'))
  stocks = stocks[Volume > 0]
  stocks[,cost_basis := round(Total_Cost / Volume, 3)]
  stocks[,curr_price := round(sapply(stocks[,Ticker], recent_quote), 2)]
  stocks[,curr_value := round(curr_price * Volume, 3)]
  stocks[,return := 100*(round(curr_value / Total_Cost, rounding) - 1)]
  stocks[,c('hist_div', 'forward_div') := get_div_data(Ticker), Ticker]
  if(!is.null(updates)) {
    for(ticker in names(updates)) {
      stocks[Ticker == ticker, forward_div := updates[[ticker]]]
    }
  }
  stocks[,c('hist_income', 'forward_income') := .(round(Volume * hist_div, 2),
                                                  round(Volume * forward_div,2))]
  stocks[,c('CB_share',
            'portfolio_share',
            'income_share') :=
           .(round(Total_Cost / sum(Total_Cost), rounding),
             round(curr_value / sum(curr_value), rounding),
             round(forward_income / sum(forward_income), rounding))]
  stocks[,c('yield', 'YOC') :=
           .(round(forward_income / curr_value, rounding),
             round(forward_income / Total_Cost, rounding))]
  print(stocks[,.('cost basis' = sum(Total_Cost),
                  'current value' = sum(curr_value),
                  'return' = 100*(sum(curr_value) / sum(Total_Cost) - 1),
                  'forward income' = sum(forward_income),
                  'yield' = sum(forward_income) / sum(curr_value),
                  'yield on cost' = sum(forward_income) / sum(Total_Cost))])
  invisible(stocks)
}

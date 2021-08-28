#' Stack multiple years of income
#'
#' @param data a `data.table` with income and dates
#' @param date_field a character with the name of the column with the payment
#'   date
#' @param m a date of the last day of income to be included (defaults to last
#'   day of the current month)
#' @param y the last year to analyze (defaults to current year)
#' @param n_years the number of years to plot
#'
#' @export
#' @import data.table
#' @import ggplot2

compare_div_income <- function(data,
                               date_field = 'PayDate',
                               m = NULL,
                               y = NULL,
                               n_years = 1) {
  if(is.null(m)) m <- lubridate::ceiling_date(Sys.Date(), 'months')
  if(is.null(y)) y <- lubridate::year(Sys.Date())
  if(date_field != 'PayDate') {
    setnames(data, old = date_field, new = 'PayDate')
  }
  data[,PayDate := as.IDate(PayDate)]
  data <- data[PayDate < m & data.table::year(PayDate) >= (y-n_years)]
  data[,Month := lubridate::ceiling_date(PayDate, unit = 'month') - 1]
  data[,Income := as.numeric(gsub('\\$', '', Income))]
  data = data[,.('Income' = sum(Income)), Month]
  data[,Year := as.character(year(Month))]
  data[,Month := month(Month)]
  data[,cumIncome := cumsum(Income),Year]
  data = rbindlist(list(data, as.data.table(expand.grid(Month = 0, Income = 0, Year = seq(y-n_years, y, 1), cumIncome = 0))))
  fills <- c('#28473d', paste0('#28473d', seq(85, 99 - ((n_years-1) * 15), length.out = n_years)))
  names(fills) <- as.character(seq(y-n_years, y, 1))
  data <- rbindlist(list(data, data.table(Month = 1, Income = 0, Year = 2020, cumIncome = 0)))
  plt <- ggplot(data, aes(x = Month, y = cumIncome)) +
    geom_area(aes(fill = Year), position = 'identity') +
    scale_fill_manual(values = fills) +
    scale_x_continuous(breaks = 1:12,
                       labels = month.abb) +
    labs(x = 'Month (End)', y = 'Cumulative Annual Income')
  return(plt)
}

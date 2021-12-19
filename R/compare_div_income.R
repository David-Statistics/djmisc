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
  if(is.null(m)) m <- lubridate::month(Sys.Date())
  if(is.null(y)) y <- lubridate::year(Sys.Date())
  if(date_field != 'PayDate') {
    setnames(data, old = date_field, new = 'PayDate')
  }
  data[,PayDate := as.IDate(PayDate)]
  data[,Month := data.table::month(PayDate)]
  data[,Year := data.table::year(PayDate)]
  data <- data[(Month <= m & Year == y) |
                 (Year >= (y-n_years) & Year < y)]
  data[,Income := as.numeric(gsub('\\$', '', Income))]
  data = data[,.('Income' = sum(Income)), keyby = .(Year,Month)]
  data[,cumIncome := cumsum(Income),Year]
  data = rbindlist(list(data, as.data.table(expand.grid(Year = seq(y-n_years, y, 1), Month = 0, Income = 0, cumIncome = 0))))
  fills <- c('#28473d', paste0('#28473d', seq(85, 99 - ((n_years-1) * 15), length.out = n_years)))
  names(fills) <- as.character(seq(y-n_years, y, 1))
  if(2020 >= (y - n_years) & 2020 <= y) {
    data <- rbindlist(list(data, data.table(Year = 2020, Month = 1, Income = 0, cumIncome = 0)))
  }
  data[,Year := as.character(Year)]
  plt <- ggplot(data, aes(x = Month, y = cumIncome)) +
    geom_area(aes(fill = Year), position = 'identity') +
    scale_fill_manual(values = fills) +
    scale_x_continuous(breaks = 1:12,
                       labels = month.abb) +
    labs(x = 'Month (End)', y = 'Cumulative Annual Income')
  return(plt)
}

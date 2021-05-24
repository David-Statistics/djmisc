#' Income Forecast over time
#'
#' Create a forward income forcast plot over time. Optionally add an exponential
#' function connecting the first and last time points to calculate CAGR.
#'
#' @param data a `data.table` with a field for date and forward income
#' @param date_field a character with the date field
#' @param forecast_field a character with the income forecast field
#' @param y year of interest (NULL defaults to current year)
#' @param add_cagr if TRUE adds a smoothed income over time line and prints the
#'   CAGR from the first time period to the last in the upper left.
#' @param n_years How many years to include in the plot
#'
#' @export
#' @import ggplot2
#' @import data.table
#' @import gridExtra
plot_income_forecast <- function(data,
                                 date_field = 'Date',
                                 forecast_field = 'Forward_div',
                                 y = NULL,
                                 add_cagr = TRUE,
                                 n_years = 1) {
  if(is.null(y)) y <- lubridate::year(lubridate::today())
  old_names <- c(date_field, forecast_field)
  setnames(data, old = old_names, new = c('date', 'div'))
  data[,date := as.IDate(date)]
  data = data[year(date) %in% c(y, y-n_years)]
  data[,`MoM Net Change` := round(div - shift(div, type = 'lag'), 2)]
  data[,`MoM % Change` := paste0(round(100*`MoM Net Change` / shift(div, type = 'lag'),2), '%')]
  data[,`MoM Net Change` := paste0('$', `MoM Net Change`)]
  data[,'Forward Annual Income' := paste0('$', div)]
  cagr <- (data[order(-date)][1,div]/data[1,div])^(1/(as.numeric(data[,max(date)] - data[,min(date)]) / 365.25)) - 1
  smoothed <- data[1,div] * (1+cagr) ^ ((seq_along(seq(min(data[,date]), max(data[,date]), 1))-1)/365.25)
  smoothed <- data.table(date = seq(min(data[,date]), max(data[,date]),1), div = smoothed)
  income_plt <- ggplot(data, aes(x = date, y = div)) +
    geom_area(fill = '#28473d') +
    labs(x = '', y = 'Forward Annual Income')
  if(add_cagr) {
    income_plt <- income_plt +
      geom_line(data = smoothed, size = 1.25, col = '#C6773E') +
      geom_text(x = min(data[,date]),
                y = max(data[,div]),
                label = glue::glue('CAGR: {round(cagr*100, 2)}%'),
                vjust = 'inward',
                hjust = 'inward')
  }
  to_plot <- data[,.(date, `Forward Annual Income`, `MoM Net Change`, `MoM % Change`)]
  setDF(to_plot)
  to_plot <- tableGrob(to_plot, rows = NULL)
  p <- do.call(grid.arrange, list(income_plt, to_plot))
  setnames(data, old = c('date', 'div'), new = old_names)
  return(p)
}

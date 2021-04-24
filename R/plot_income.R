#' Generate an income plot for a set of securities
#'
#' Generate a plot that shows the income broken out by security and reinvestment for a given set of securities
#'
#' @param dat a list of `data.tables` in the format that is returned by `get_income_stream`
#' @param yield show yield on cost instead of absolute amounts
#' @param cost original cost of the investments
#' @param annual annualize numbers
#' @param main title of the plot
#' @return a `ggplot2` object with the plot for the securities
#' @export
#' @import data.table
#' @import ggplot2

plot_income = function(dat, yield = FALSE, cost = 1000, annual = FALSE,
                       main = ggplot2::element_blank()) {
  if('date' %in% names(dat)) {
    periods = dat[,.N,year(date)][order(-N)][1,N]
    to_plot = melt(dat, id.vars = c('date'),
                   measure.vars = c('orig_income', 'reinvest_income'),
                   variable.name = 'source', variable.factor = FALSE)
    to_plot[,source := factor(source, levels = c('reinvest_income', 'orig_income'))]
    if(yield) to_plot[,value := value / cost]
    if(annual) to_plot[,value := periods*value]
    ending = unlist(dat[order(-date)][1,.(orig_income, reinvest_income)])
    caption = paste0('Annual forward income from original investment: $', round(ending[1]*periods, 2),
                     '\nAnnual forward income from reinvestment: $', round(ending[2]*periods, 2))
    colors = c(reinvest_income = '#00631b70',
               orig_income = '#00154070')
    p = ggplot(to_plot, aes(x = date)) +
      geom_area(aes(y = value, fill = source)) +
      theme_gray() +
      scale_fill_manual(values = colors) +
      labs(y = ifelse(yield, 'yield', 'dollars'),
           caption = caption,
           title = main) +
      theme(legend.position="bottom")
    return(p)
  }
  dat = lapply(dat, FUN = function(x) {
    periods = x[,.N,year(date)][order(-N)][1,N]
    ret = melt(x, id.vars = c('date'),
               measure.vars = c('orig_income', 'reinvest_income'),
               variable.name = 'source', variable.factor = FALSE)
    ret[,source := factor(source, levels = c('reinvest_income', 'orig_income'))]
    ret[,periods := periods]
    return(ret[])
  })
  for(ticker in names(dat)) {
    dat[[ticker]][,Ticker := ticker]
  }
  to_plot = rbindlist(dat)
  to_plot[,date := as.IDate(date)]
  to_plot[,Month := ceiling_date(date, 'months')-1]
  frame = expand.grid(Month = ceiling_date(seq(min(to_plot[,Month]), max(to_plot[,Month]), by ='months'), 'months')-1,
                      Ticker = unique(to_plot[,Ticker]),
                      source = unique(to_plot[,source]))
  setDT(frame)
  to_plot = merge(frame, to_plot[,.(Month, Ticker, source, value, periods)],
                  by = c('Month', 'Ticker', 'source'), all.x = TRUE)
  setkey(to_plot, Month, Ticker)
  to_plot = to_plot[, zoo::na.locf(.SD, na.rm = FALSE), .(Ticker,source)]
  to_plot[is.na(value), value := 0]
  if(yield) to_plot[,value := value / cost]
  if(annual) to_plot[,value := periods*value]
  to_plot[,Ticker := factor(Ticker)]
  colors = RColorBrewer::brewer.pal(length(unique(to_plot[,Ticker])),
                                    ifelse(length(unique(to_plot[,Ticker])) > 8, 'Set3', 'Dark2'))
  names(colors) = unique(to_plot[,Ticker])
  alpha = c(orig_income = .95, reinvest_income = .5)
  to_plot = to_plot[,max(value),.(Ticker, source, Month)]
  setnames(to_plot, 'V1', 'value')
  p = ggplot(to_plot, aes(x = Month)) +
    geom_area(aes(y = value, alpha = source, fill = Ticker), position = position_stack()) +
    theme_gray() +
    scale_fill_manual(values = colors) +
    scale_alpha_manual(values = alpha) +
    labs(y = ifelse(yield, 'yield', 'dollars'),
         title = main,
         x = '') +
    theme_bw() +
    theme(legend.position="bottom")
  return(p)
}

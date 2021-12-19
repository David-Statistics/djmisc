#'
#'
#'
#' @export
#' @import data.table
#' @import ggplot2

generate_rmd <- function(income_forecast,
                         income_data,
                         path,
                         m = NULL,
                         y = NULL,
                         plt_height = 8) {
  if(!dir.exists(path)) dir.create(path)
  if(!dir.exists(paste0(path, '/plots'))) dir.create(paste0(path, '/plots'))
  if(!dir.exists(paste0(path, '/data'))) dir.create(paste0(path, '/data'))
  forecast_plot <- plot_income_forecast(income_forecast, y = y)
  ggsave(paste0(path, '/plots/income_forecast.png'), forecast_plot,
         dpi = 'retina', height = plt_height, width =  4/3 * plt_height)
  comp_plot <- compare_div_income(google_dat, m = m, y = y)
  ggsave(paste0(path, '/plots/income_comparison.png'), comp_plot,
         dpi = 'retina', height = plt_height, width = 4/3 * plt_height)
  if(is.null(m)) {
    m = lubridate::month(Sys.Date())
  } else if(class(m) == 'Date') {
    m = lubridate::month(m)
  }
  if(is.null(y)) y = lubridate::year(Sys.Date())

  d <- income_data[month(PayDate) == m & year(PayDate) == y,
                   .(Ticker,
                     `Div/share`,
                     Volume,
                     Income = round(Income,2))][order(Ticker)]
  d <- merge(d, google_dat[month(PayDate) == m & year(PayDate) == y - 1,
                           .(Ticker, Income)],
             by = 'Ticker', all = TRUE)
  setnames(d, c('Income.x', 'Income.y'), c('2021 Income', '2020 Income'))
  d[is.na(`2021 Income`), `2021 Income` := 0]
  d[is.na(`2020 Income`), `2020 Income` := 0]
  dt <- DT::datatable(d[order(-`2021 Income`)],
                      options = list(pageLength = 25, dom = 'tip'), rownames = FALSE)
  dt <- DT::formatCurrency(dt, c('Div/share', '2021 Income', '2020 Income'))

  saveRDS(dt,
          paste0(path, '/data/income_dt.rds'))


  sink(paste0(path, '/monthly_report.rmd'))
  cat(paste0(
'---
title: "May Month End Summary"
author: "David Clancy"
date: "', lubridate::today(), '"
output: html_document
---

## Long Term Updates

![](', paste0(path, '/plots/income_forecast.png'), ')

![](', paste0(path, '/plots/income_comparison.png'), ")

## Monthly Income

```{r, echo=FALSE, results='asis'}
readRDS('./data/income_dt.rds')
```
"))

  sink()
  rmarkdown::render(paste0(path, '/monthly_report.rmd'))
}

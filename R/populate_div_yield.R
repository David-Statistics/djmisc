#' Populate dividend yield using closest dividend
#'
#' This function will add a `yield` column to a data.table which will be
#' populated with the dividend yield. This yield uses the daily closing price
#' and the dividend closest in time to that date (so likely an overestimate for
#' about a month prior to any historic dividend raise).
#' @param d a `data.table` that includes the columns that come from `pull_dat`
#'   (divCash, date, splitFactor, close)
#' @return a `data.table` with the same information as `d` but with a column for
#'   yield added (and sorted by date if not already)
#' @export
#' @import data.table

populate_div_yield <- function(d) {
  setkey(d, date)
  d[,rowi := .I]
  divs <- d[divCash > 0, .(date, 'prev' = divCash, 'previ' = rowi)]
  d = divs[d, roll = T]
  divs <- d[divCash > 0, .(date, 'nex' = divCash, 'nexti' = rowi)]
  d = divs[d, roll = -Inf]
  d[,most_recent := ifelse((nexti - rowi) > (rowi - previ), prev, nex)]
  d[is.na(prev), most_recent := nex]
  d[is.na(nex), most_recent := prev]
  d[is.na(nexti), nexti := -99999]
  d[is.na(previ), previ := -99999]
  d[,time_diff := abs(nexti - rowi)]
  d[abs(rowi - previ) < time_diff, time_diff := abs(rowi - previ)]
  d[time_diff > 150, most_recent := 0]
  d[,c('nex', 'nexti', 'prev', 'previ', 'rowi', 'time_diff') := NULL]
  d[,adjRatio := cumprod(splitFactor)]
  d[,adjDiv := most_recent * (adjRatio / max(adjRatio))]
  d[,yield := adjDiv / close]
  return(d[])
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_div_income.R
\name{compare_div_income}
\alias{compare_div_income}
\title{Stack multiple years of income}
\usage{
compare_div_income(
  data,
  date_field = "PayDate",
  m = NULL,
  y = NULL,
  n_years = 1
)
}
\arguments{
\item{data}{a \code{data.table} with income and dates}

\item{date_field}{a character with the name of the column with the payment
date}

\item{m}{a date of the last day of income to be included (defaults to last
day of the current month)}

\item{y}{the last year to analyze (defaults to current year)}

\item{n_years}{the number of years to plot}
}
\description{
Stack multiple years of income
}

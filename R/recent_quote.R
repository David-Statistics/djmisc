#' Simple function for getting the last price of a security through `riingo`
#'
#' @param sym character vector of securities to get the quotes of
#' @export

recent_quote <- function(sym) {
  riingo::riingo_iex_quote(sym)[['last']]
}

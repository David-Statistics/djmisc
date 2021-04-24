#' Last `n` elements
#'
#' Simple helper function to get the last `n` elements of a vector
#'
#' @param x vector to get the last `n` elements of
#' @param n number of elements to return
#' @export
get_last <- function(x, n = 1) {
  if(length(x) == 0) {
    warning('vector is empty')
    return(NULL)
  }
  if(length(x) <= n) {
    warning('n >= length(x). Returning full vector')
    return(x)
  }
  return(x[(length(x) - n + 1):length(x)])
}

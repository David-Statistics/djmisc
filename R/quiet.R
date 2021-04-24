#' Some functions insist on using `cat` to print messages to the console. This
#' will make them quiet by sinking to a temp file
#'
#' @param x expression to make quiet
#' @export

quiet = function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

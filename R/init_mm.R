#' Initialize python environ for match_mutate package
#'
#' @return NULL
#' @export
#'
#' @examples
init_mm <- function(){
  library(reticulate)
  reticulate::py_install("scipy", method = auto)
}

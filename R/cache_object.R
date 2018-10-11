# cache_object ------------------------------------------------------------

#' Cache object
#'
#' @param expr An expression
#' @param filename A filename to use
#' @param renew Whether to force renewal of the object
#' @param ... Other arguments passed to [readr::write_rds()]
#'
#' @return An object
#' @export
#'
#' @examples
#' i = cache_object(MASS::mvrnorm(1e6, rep(0, 3), matrix(c(1, .4, .4, .4, 1, .4, .4, .4, 1), ncol = 3)), "test_object.rds")
#' i = cache_object(stop("This is not evaluated"), "test_object.rds")
#' i = cache_object(warning("But this is"), "test_object.rds", renew = T)
cache_object = function(expr, filename, renew = F, ...) {
  #force renewal?
  if (renew) {
    base::message("Forcing renewal of cached object")
    if (base::file.exists(filename)) {
      base::file.remove(filename)
    }
  }

  #try to read from disk
  #if not, eval expr
  if (base::file.exists(filename)) {
    base::message("Cache found, reading object from disk")
    x = readr::read_rds(filename)
  } else {
    if (!renew) {
      base::message("Cache not found, evaluating expression")
    } else {
      base::message("Evaluating expression")
    }
    x = base::eval(base::quote(expr))
    readr::write_rds(x, filename, ...)
  }

  x
}

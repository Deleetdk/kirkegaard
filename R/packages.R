#' Load packages
#'
#' @param ... Unquoted package names separated by comma
#'
#' @return On success invisibly returns TRUE, otherwise an error.
#' @export
#'
#' @examples
#' load_packages(ggplot2)
#' testthat::expect_error(load_packages(ggplot3))
load_packages = function(...) {
  # browser()
  #capture
  args = rlang::ensyms(...)

  #chrs
  chrs = purrr::map_chr(args, as.character)

  #load each
  purrr::map_lgl(chrs, function(x) {
    suppressWarnings({
      kirkegaard::try_else(
        expr = library(x, character.only = T, logical.return = T),
        else. = F,
        silent = T
      )
    })
  }) -> tries

  #any fails?
  if (any(!tries)) {
    failed = chrs[!tries]
    stop(glue::glue("Some packages could not load: {stringr::str_c(failed, collapse = ', ')}"),
         call. = F)
  }

  return(invisible(T))
}



#' Assert that packages are installed without attaching them
#'
#' @param ... Unquoted package names separated by comma
#'
#' @return On success invisibly returns TRUE, otherwise an error.
#' @export
#'
#' @examples
#' assert_installed(ggplot2)
#' testthat::expect_error(assert_installed(ggplot2, ggplot3))
assert_installed = function(...) {
  #capture
  args = rlang::ensyms(...)

  #chrs
  chrs = purrr::map_chr(args, as.character)

  #get the list
  installed_list = installed.packages()[, 1]

  #are all installed?
  not_installed = setdiff(chrs, installed_list)

  #any?
  if (length(not_installed) > 0) {
    #fail with the list
    stop(glue::glue("Some packages not installed: {stringr::str_c(not_installed, collapse = ', ')}"), call. = F)
  }

  return(T)
}


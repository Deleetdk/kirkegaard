#' Clean string
#'
#' A simple wrapper str_replace_all() with sensible defaults.
#' @param string (chr scalar) A string to clean.
#' @param underscores (log scalar) Whether to clean underscores. Default=T.
#' @param spacing_dots (log scalar) Whether to clean spacing underscores. Default=T.
#' @param end_dots (log scalar) Whether to clean dots at the end of the string. Default=T.
#' @param all_dots (log scalar) Whether to clean all dots. Default=F.
#' @param multi_dots (log scalar) Whether to reduce multiple dots in a row to a single dot. Default=T.
#' @export
#' @examples
#' str_clean(colnames(iris))
str_clean = function(string, underscores = T, spacing_dots = T, end_dots = T, all_dots = F, multi_dots = T) {
  library(stringr)

  if (spacing_dots) string = str_replace_all(string, "(\\w)\\.(\\w)", "\\1 \\2")
  if (underscores) string = str_replace_all(string, "_", " ")
  if (all_dots) string = str_replace_all(string, "\\.", " ")
  if (multi_dots) string = str_replace_all(string, "\\.+", ".")
  if (end_dots) string = str_replace_all(string, "\\.$", "")

  return(string)
}


#' Multiple replacement
#'
#' A simple wrapper for stringr's str_replace() and str_replace_all().
#' @param string (a character scalar) A string.
#' @param patterns (a character vector) A character vector of things to clean. Regex.
#' @param replacement (a character scalar) What to replace matches with.
#' @param all (boolean) Whether to clean all instances or just the first. Default=T.
#' @keywords string, character. replace, vectorized
#' @export
#' @examples
#' str_replace_multi()
str_replace_multi = function(string, patterns, replacement, all = T) {
  library(stringr)

  for (pattern in patterns) {
    if (all) string = str_replace_all(string, pattern, replacement)
    if (!all) string = str_replace(string, pattern, replacement)
  }

  return(string)
}


#' Insert newlines into text every nth character.
#'
#' Returns a character string with newlines every nth character. See also add_newlines().
#' @param x A character string.
#' @param interval How often the newlines are added.
#' @export
#' @examples
#' set.seed(2)
#' new_lines_adder(paste0(sample(c(letters, " "), size = 100, replace = T), collapse = ""), interval = 30)
new_lines_adder = function(x, interval) {
  library(stringr)

  #add spaces after /
  x = str_replace_all(x, "/", "/ ")

  #split at spaces
  x.split = strsplit(x, " ")[[1]]

  # get length of snippets, add one for space
  lens <- nchar(x.split) + 1

  # now the trick: split the text into lines with
  # length of at most interval + 1 (including the spaces)
  lines <- cumsum(lens) %/% (interval + 1)

  # construct the lines
  x.lines <- tapply(x.split, lines, function(line)
    paste0(paste(line, collapse=" "), "\n"), simplify = TRUE)

  # put everything into a single string
  result <- paste(x.lines, collapse="")

  #remove spaces we added after /
  result = str_replace_all(result, "/ ", "/")

  #remove ending newline
  result = str_sub(result, start = 1, end = -2)

  return(result)
}


#' Insert newlines into text every nth character.
#'
#' Returns a character string with newlines every nth character. Works for character vectors too.
#' @param x (chr vector) The strings to split with newlines.
#' @param line_length (num scalar) The desired max length of each line. Defaults to 95 (suitable for ggplot2).
#' @export
#' @examples
#' set.seed(2)
#' add_newlines(paste0(sample(c(letters, " "), size = 100, replace = T), collapse = ""))
add_newlines = function(x, line_length = 95) {
  # make sure, x is a character array
  x = as.character(x)
  #determine number of groups
  groups = length(x)
  # apply splitter to each
  t = sapply(x, FUN = new_lines_adder, interval = round(line_length/groups), USE.NAMES = FALSE)
  return(t)
}


#' Easy character concatenation
#'
#' A wrapper for stringr's \code{\link{str_c}} and the primitive "+" function.
#' @param x (character vector) A character vector.
#' @param y (character vector) A character vector.
#' @return A character vector. Note that it will have a length longer than one if one of the inputs has that.
#' @export
#' @examples
#' #digits still work
#' 1+1
#' #characters work too
#' "str" + "ing"
#' #mixes are converted to characters
#' "123" + 456
#' #longer than length 1
#' 1:2 + "a"
"+" = function(x, y) {
  library("stringr")

  if(is.character(x) || is.character(y)) {
    return(str_c(x, y))
  } else {
    .Primitive("+")(x, y)
  }
}


#' Improved str_detect
#'
#' A wrapper for stringr's \code{\link{str_detect}} so that one can get the matches values, not just logical values. This restores the functionality of base-r's equivalent function, \code{\link{gsub}}.
#' @param string (character vector) A character vector.
#' @param pattern (character vector) Pattern to look for (REGEX).
#' @param value (log scalar) Whether to return the matched values instead of logical values. Default = F.
#' @return A character vector. Note that it will have a length longer than one if one of the inputs has that.
#' @export
#' @examples
#' str_detect2(letters[1:10], pattern = "[acbde]")
#' str_detect2(letters[1:10], pattern = "[acbde]", value = T)
str_detect2 = function(string, pattern, value = F) {
  library(stringr)

  #get results
  v = str_detect(string = string, pattern = pattern)

  #return values or logicals?
  if (value) return(string[v])
  v
}

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

  if (spacing_dots) string = stringr::str_replace_all(string, "(\\w)\\.(\\w)", "\\1 \\2")
  if (underscores) string = stringr::str_replace_all(string, "_", " ")
  if (all_dots) string = stringr::str_replace_all(string, "\\.", " ")
  if (multi_dots) string = stringr::str_replace_all(string, "\\.+", ".")
  if (end_dots) string = stringr::str_replace_all(string, "\\.$", "")

  return(string)
}


#' Multiple replacement
#'
#' A simple wrapper for stringr's str_replace() and str_replace_all().
#' @param string (a character scalar) A string.
#' @param patterns (a character vector) A character vector of things to clean. Regex.
#' @param replacement (a character scalar) What to replace matches with.
#' @param all (boolean) Whether to clean all instances or just the first. Default=T.
#' @export
#' @examples
#' str_replace_multi()
str_replace_multi = function(string, patterns, replacement, all = T) {

  for (pattern in patterns) {
    if (all) string = stringr::str_replace_all(string, pattern, replacement)
    if (!all) string = stringr::str_replace(string, pattern, replacement)
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

  #add spaces after /
  x = stringr::str_replace_all(x, "/", "/ ")

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
  result = stringr::str_replace_all(result, "/ ", "/")

  #remove ending newline
  result = stringr::str_sub(result, start = 1, end = -2)

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

  if(is.character(x) || is.character(y)) {
    return(stringr::str_c(x, y))
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
  #get results
  v = stringr::str_detect(string = string, pattern = pattern)

  #return values or logicals?
  if (value) return(string[v])
  v
}


#' Detect and replace values with regex
#'
#' A wrapper for stringr's \code{\link{str_detect}} so that one can detect strings with a particular pattern and replacement them outright. Vectorized over patterns and replacements.
#' @param string (chr vector) A character vector.
#' @param pattern (chr vector) Pattern to look for (REGEX).
#' @param replacement (chr vector) A vector of values to replace with.
#' @return A character vector.
#' @export
#' @examples
#' str = 1:99 + c(".", ",", ":") #example str vector
#' str_detect_replace(str, pattern = c("\\.", ",", ":"), replacement = c("dot", "comma", "colon")) #replace by pattern
str_detect_replace = function(string, pattern, replacement) {
  #check input
  if (!lengths_match(pattern, replacement)) stop("Lengths of pattern and replacement must match!")

  #loop over pattern-replacements
  for (i in seq_along(pattern)) {
    string[stringr::str_detect(string, pattern = pattern[i])] = replacement[i] #detect and replace
  }

  string
}


#' Make strings unique
#'
#' Detects duplicate strings and makes them unique by adding a number at the end.
#' @param string (chr vector) A character vector.
#' @param suffix (chr vector) A character to use to make unique suffixes. Must contain \%d.
#' @return A character vector.
#' @details This function loops over the groups of identical strings and adds the suffix if the group has more than 1 member. This suffix addition is done using \code{sprintf}.
#' @export
#' @examples
#' Example vector with some duplicates
#' (x = sample(LETTERS[1:10], size = 20, replace = T))
#' #uniquify
#' x %>% str_uniquify
#' #custom suffix using a second %d.
#' x %>% str_uniquify(" [%d/%d]")
str_uniquify = function(string, suffix = " (%d)") {
  #make a df
  d = data.frame(names = string,
                 n = seq_along(string))

  #loop over groups
  if (any(duplicated(d$names))) {
    d = plyr::ddply(d, .variables = "names", .fun = function(x) {
      if (nrow(x) == 1) return(x)
      x$names = sprintf(x$names + suffix, 1:nrow(x), nrow(x))
      x
    })
  }

  #sort by n to get original order back
  d %<>% df_sort("n")

  #return
  d$names
}


#' Filter string by pattern
#'
#' A wrapper for stringr's \code{\link{str_detect}} that returns the matched values, or the non-matched values.
#' @param string (chr) A string.
#' @param pattern (chr) Pattern to look for (regex).
#' @param reverse (lgl) Whether to return the non-matched values instead (default no).
#' @return A character vector.
#' @export
#' @examples
#' str_filter(letters, pattern = "[aeiou]")
#' str_filter(letters, pattern = "[aeiou]", reverse = T)
str_filter = function(string, pattern, reverse = F) {
  #get results
  v = stringr::str_detect(string = string, pattern = pattern)

  #reverse
  if (reverse) return(string[!v])
  string[v]
}

#legalize names
#' Make names legal
#'
#' Make variable names legal to use in R formulas etc.
#' @param x (chr) A vector of names.
#'
#' @return a vector
#' @export
#'
#' @examples
#' c("2017x", "male (%)", "Male/female ratio") %>% str_legalize()
str_legalize = function(x) {
  x %>%
    #replace % to pct
    stringr::str_replace_all("\\%", "pct") %>%
    #replace illegal chars to underscores
    stringr::str_replace_all("[^A-Za-z0-9]", "_") %>%
    #remove double underscores
    stringr::str_replace_all("_+", "_") %>%
    #remove trailing underscores
    stringr::str_replace_all("(^_)|(_$)", "") %>%
    #insert leading _ if numeral is first
    stringr::str_replace("^(\\d)", "_\\1")
}

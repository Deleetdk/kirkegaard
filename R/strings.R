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
#' Make strings unique by adding a suffix to indicate which number copy it is.
#' @param string (chr vector) A character vector.
#' @param suffix (chr vector) A character to use to make unique suffixes. Must contain `%d`.
#' @param pad Whether to pad numbers by adding 0's in front to keep the same length in all suffixes.
#' @examples
#' str_uniquify(letters[rep(1, 3)])
#' str_uniquify(letters[rep(1, 3)], suffix = ', %d')
str_uniquify = function(string, suffix = " (%d)", pad = F) {
  #make a df
  d = data.frame(names = string,
                 n = seq_along(string))

  #loop over groups
  if (any(duplicated(d$names))) {
    d = plyr::ddply(d, .variables = "names", .fun = function(x) {
      if (nrow(x) == 1) return(x)

      #pad numbers if wanted
      d1 = 1:nrow(x)
      if (pad) {
        d1 = str_pad(d1, width = max(str_length(d1)), side = "left", pad = "0")
        # d1 = sprintf("%06d", d1)
      }

      #add numbers
      # x$names = sprintf(x$names + suffix, 1:nrow(x), nrow(x))
      x$names = x$names + suffix %>%
        str_replace("%d", as.character(d1)) %>%
        str_replace("%d", as.character(nrow(x)))

      x
    })
  }

  #sort by n to get original order back
  d %<>% arrange(n)

  #return
  d$names %>% as.character()
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
#'
#' @param x (chr) A vector of names.
#' @param ascii Allow ascii only or all letters?
#'
#' @return a vector
#' @export
#'
#' @examples
#' c("2017x", "male (%)", "Male/female ratio") %>% str_legalize()
str_legalize = function(x, ascii = T) {

  #abbreviation symbols to text
  x %<>%
    #replace % to pct
    stringr::str_replace_all("\\%", "pct") %>%
    #replace & to _and_
    stringr::str_replace_all("\\&", "_and_") %>%
    #replace + to plus
    stringr::str_replace_all("\\+", "plus") %>%
    #replace - to _
    stringr::str_replace_all("\\-", "_") %>%
    #replace / to _
    stringr::str_replace_all("\\/", "_") %>%
    #and also \ to _
    stringr::str_replace_all("\\\\", "_")

  #other symbols
  #replace non-ascii to _
  if (ascii) {
    x %<>%
      stringr::str_replace_all("[^A-Za-z0-9]", "_")
  } else { #non-latin to _
    x %<>%
      stringr::str_replace_all("[^\\p{L}]", "_")
  }

  x %>%
    #remove double underscores
    stringr::str_replace_all("_+", "_") %>%
    #remove trailing underscores
    stringr::str_replace_all("(^_)|(_$)", "") %>%
    #insert leading x if numeral is first
    stringr::str_replace("^(\\d)", "x\\1") %>%
    #empty to x
    kirkegaard::mapvalues(from = "", to = "x", warn_missing = F) %>%
    #no duplicates
    str_uniquify(suffix = "_%d")
}


#' Capitalize initial letter
#'
#' Capitalize the initial letter in a string. Vectorized.
#' @param x (chr) A character vector.
#'
#' @return A character vector same length as the input.
#' @export
#' @details
#' Just a simple wrapper around stringr functions. Mainly useful for when writing tables to clipboard/supplementary files for writing studies.
#' @examples
#' #make iris colnames lowercase, then capitalize initial letter.
#' colnames(iris) %>% str_to_lower() %>% str_to_upper_initial()
str_to_upper_initial = function(x) {
  #convert to character
  #useful bc people might pass e.g. factors
  x = as.character(x)

  #functional wrapper for purrr
  purrr::map_chr(x, ~stringr::str_sub(., 1, 1) %>% stringr::str_to_upper() + stringr::str_sub(., 2))
}


# renamed base functions --------------------------------------------------
#hard to recall

#' Extract filename from a file path.
#'
#' @param x (chr) A path to a filename.
#'
#' @return A character.
#' @export
#' @details
#' Just a thin wrapper for [base::basename()].
#'
#' @examples
#' dir(full.names = T) %>% str_filename()
str_filename = function(x) {
  base::basename(x)
}


#' Extract directory path from a file path.
#'
#' @param x (chr) A path to a filename.
#'
#' @return A character.
#' @export
#' @details
#' Just a thin wrapper for [base::dirname()].
#'
#' @examples
#' dir(getwd(), full.names = T) %>% str_dirname()
str_dirname = function(x) {
  base::dirname(x)
}


#' Round a number to desired number of shown digits
#'
#' @param digits How many digits to print
#' @param less_than A value to add less than sign for
#' @param more_than A value to add greater than sign for
#' @param pad Whether to pad whitespace to force equal length strings
#' @param x A vector of values
#'
#' @return A character vector
#' @export
#'
#' @examples
#' str_round(1.123, 2)
#' str_round(1.100, 2)
#' str_round(1.1000000, 2)
#' str_round(1.1000000, 3)
#' str_round(0.0000001, 3)
#' str_round(seq(0, 7, by = 1), 2, less_than = 2, more_than = 5)
str_round = function(x, digits = 2, less_than = NULL, more_than = NULL, pad = F) {
  #as chr
  y = format(round(x, digits = digits), nsmall = digits)

  if (!is.null(less_than)) {
    # browser()
    y[x < less_than] = stringr::str_glue("<{str_round(less_than, digits = digits)}")
    y = as.character(y)
  }

  if (!is.null(more_than)) {
    # browser()
    y[x > more_than] = stringr::str_glue(">{str_round(more_than, digits = digits)}")
    y = as.character(y)
  }

  #padding
  if (!pad) y = y %>% str_trim()

  y
}


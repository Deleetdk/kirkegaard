## Misc other functions

##Combines lower and upper part of two matrices into one. Additional option for the diagonal.
#Credit to: http://stackoverflow.com/questions/13115720/how-do-i-combine-the-upper-tri-of-one-matrix-with-the-lower-tri-of-another-in-r
#' Combine upper and lower part of a matrix.
#'
#' Returns a matrix composed of an upper and a upper part.
#' @param .upper.tri a matrix or data.frame for the upper trianagle.
#' @param .lower.tri a matrix or data.frame for the lower trianagle.
#' @param .diag a value to insert in the diagonal. Defaults to NA.
#' @keywords combine, matrix, data.frame, upper, lower
#' @export
#' @examples
#' combine_upperlower()
combine_upperlower = function(.upper.tri, .lower.tri, .diag = NA) {
  #detect wrong input
  if (!is.matrix(.upper.tri) & !is.data.frame(.upper.tri)) {
    stop("First parameter was not a matrix or data.frame.")
  }
  if (!is.matrix(.lower.tri) & !is.data.frame(.lower.tri)) {
    stop("Second parameter was not a matrix or data.frame.")
  }

  #inpute types
  both.df = is.data.frame(.lower.tri) & is.data.frame(.upper.tri) #are both df?
  #convert
  .lower.tri = as.matrix(.lower.tri)
  .upper.tri = as.matrix(.upper.tri)
  #make new
  new = .lower.tri #copy of lower
  diag(new) = .diag #diagonal to whatever
  new[upper.tri(new)] <- .upper.tri[upper.tri(.upper.tri)]
  if (both.df) {return(as.data.frame(new))} #convert back to df
  return(new) #return as matrix
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


#' Cut into bins and get proportions
#'
#' Cuts a vector into a specified number of equal sized bins and calculations the proportion of datapoints in each bin. Returns a data.frame.
#' @param x A numeric vector.
#' @param breaks_ The number of bins to use.
#' @export
#' @examples
#' get_prop_table(iris$Sepal.Length)
get_prop_table = function(x, breaks_ = 20){
  library(magrittr)
  library(plyr)
  x_prop_table = cut(x, 20) %>% table(.) %>% prop.table %>% data.frame
  colnames(x_prop_table) = c("interval", "density")
  intervals = x_prop_table$interval %>% as.character
  fetch_numbers = str_extract_all(intervals, "\\d\\.\\d*")
  x_prop_table$means = laply(fetch_numbers, function(x) {
    x %>% as.numeric %>% mean
  })
  return(x_prop_table)
}


#' Is object a simple vector?
#'
#' A simple wrapper for is.vector and is.list. The normal is.vector function returns true for lists which is undesirable. Returns a boolean.
#' @param x (any object) An object to test.
#' @keywords vector, list
#' @export
#' @examples
#' l = list(1:10)
#' v = 1:10
#' is.vector(v)
#' is.vector(l)
#' is_simple_vector(v)
#' is_simple_vector(l)
is_simple_vector = function(x) {
  is.vector(x) & !is.list(x)
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
  if (end_dots) string = str_replace_all(string, "\\.$", "")
  if (all_dots) string = str_replace_all(string, "\\.", " ")
  if (multi_dots) string = str_replace_all(string, "\\.+", ".")

  return(string)
}


#' Get dimensions of object.
#'
#' Returns the dimensions of an object. Also works on atomic (1-d) objects for which base-r dim() returns NULL.
#' @param x (an object) An object.
#' @keywords dim, dimensions
#' @export
#' @examples
#' v = 1:10
#' get_dims(v)
#' m = matrix(1:9, nrow=3)
#' get_dims(m)
get_dims = function(x) {
  if (is.null(dim(x))) {
    return(length(x))
    } else {
    return(dim(x))
  }
}


#' Copy names from one object to another.
#'
#' Attempts to copy names that fit the dimensions of vectors, lists, matrices and data.frames.
#' @param x (an object) An object whose dimnames should be copied.
#' @param y (an object) An object whose dimensions that should be renamed.
#' @keywords names, rownames, colnames, copy
#' @export
#' @examples
#' m = matrix(1:9, nrow=3)
#' n = m
#' rownames(m) = letters[1:3]
#' colnames(m) = LETTERS[1:3]
#' copy_names(m, n)
#' n
copy_names = function(x, y, partialmatching = T) {
  library(stringr)
  #find object dimensions
  x_dims = get_dims(x)
  y_dims = get_dims(y)
  same_n_dimensions = length(x_dims) == length(y_dims)

  #what is the object in y parameter?
  y_obj_name = deparse(substitute(y))

  #perfect matching
  if (!partialmatching) {
    #set names if matching dims
    if (all(x_dims == y_dims)) {
      attr(y, "dimnames") = attr(x, "dimnames")
    } else {
      stop(str_c("Dimensions did not match! ", x_dims, " vs. ", y_dims))
    }
  }

  #if using partial matching and dimensions match in number
  if (same_n_dimensions && partialmatching) {
    #loop over each dimension
    for (dim in 1:length(dimnames(x))) {
      #do lengths match?
      if (x_dims[dim] == y_dims[dim]) {
        dimnames(y)[[dim]] = dimnames(x)[[dim]]
      }
    }
  }

  #assign in the outer envir
  assign(y_obj_name, value = y, envir = parent.frame())
}


#' Fill in values in a vector
#'
#' Fill in values in a vector until it reaches a specific length.
#' @param x (an object) An object whose dimnames should be copied.
#' @param length (numeric scalar) The desired length.
#' @param value (numeric/character/logical scalar) The value to fill in.
#' @keywords vector, fill
#' @export
#' @examples
#' fill_in(1:5, 10)
fill_in = function(x, length, value = NA) {
  v_length = length(x)
  if (v_length >= length) return(x)
  x[(v_length+1):length] = value
  return(x)
}


#' Split vector every k elements
#'
#' Split a vector every k elements. Returns a list.
#' @param x (vector) A vector to split.
#' @param k (whole number scalar) Split every k elements.
#' @param uneven (logical scalar) Whether to accept a split that would be uneven. If yes, the last group will be smaller than the others. Defaults to TRUE.
#' @keywords vector, split
#' @export
#' @examples
#' split_every_k(1:12, 4)
#' split_every_k(1:11, 4) #last group isnt as large as the others
split_every_k = function(x, k, uneven = T) {
  library("stringr")
  library("assertthat")
  library("magrittr")

  #input checks
  assert_that(is.vector(x))
  assert_that(is_whole_number(k))
  assert_that(is.logical(uneven))

  #check length
  if (!uneven) {
    if (length(x) %% k != 0) {
      stop(str_c("The length of n was not integer disible by n! ", length(x), "%%", k, "=", length(x) %% k))
    }
  }

  #split
  x_length = length(x)
  k_in_x = (x_length / k) %>% ceiling
  v_groups = rep(1:k_in_x, each = k)
  v_groups = v_groups[1:x_length]
  return(split(x, v_groups))
}





#' Reshape named vectors to a data.frame.
#'
#' Construct a data.frame from a list of named vectors by filling in the shorter vectors with NAs.
#' @param list (a list of vectors) The list of vectors.
#' @param name_suffix (character scalar) The suffix to use on the names.
#' @param valie_suffix (character scalar) The suffix to use on the values.
#' @export
#' @examples
#' l = list(A = c(a = 1, b = 2, c = 3), B = c(a = 3, b = 2, c = 1))
#' named_vectors_to_df(l)
named_vectors_to_df = function(list, name_suffix = "_name", value_suffix = "_value") {
  library("magrittr")
  library("stringr")

  #checks

  #how many vectors
  v_vectors = length(list)

  #longest vector
  v_max = max(sapply(list, length))

  #fill out
  list = lapply(list, fill_in, length = v_max)

  #make data.frame
  df = matrix(ncol = 2*v_vectors, nrow = v_max) %>% as.data.frame
  v_names = str_c(rep(names(list), each = 2), c(name_suffix, value_suffix))
  colnames(df) = v_names

  #fill out values
  l_names = lapply(list, names)
  df[seq(1, 2*v_vectors, 2)] = l_names
  df[seq(2, 2*v_vectors, 2)] = list

  return(df)
}


#' Silence warnings or messages from expression via parameter.
#'
#' If warnings and messages need to be toggleable, but there are many expressions that can give them and it would be cumbersome to add an if sentence for every expression.
#' @param expr (expression) Some expression to run.
#' @param warnings (logical) Show warnings? Default=F.
#' @param messages (logical) Show messages? Default=F.
#' @keywords warning, message, suppress
#' @export silence suppressor
#' @aliases suppressor
#' @examples
#' silence(warning())
#' silence(log(-1))
#' silence(warning("test"))
#' silence(warning("test"), warnings = T)
#' silence(message("test"))
#' silence(message("test"), messages = T)
silence = function(expr, warnings = F, messages = F) {
  if (!warnings & !messages) {
    suppressWarnings(suppressMessages(expr))
  } else if (!warnings & messages) {
    suppressWarnings(expr)
  } else if (warnings & !messages) {
    suppressMessages(expr)
  } else {
    eval(expr)
  }
}

suppressor = silence #old name


#' Are all elements of a vector the same?
#'
#' Tests whether all elements of a vector are the same. Uses the max/min method mentioned at .
#' @param x (expression) Some expression to run.
#' @keywords vector, same, identical, equal
#' @export
#' @examples
#' all_the_same(rep(1, 100))
#' all_the_same(rnorm(100))
all_the_same = function(x) {
  #for numeric data, a faster method
  if (is.numeric(x)) {
    return(max(x) == min(x))
  }
  #for non-numeric data, a slower method
  return(length(unique(x)) == 1)
}


#' Merge vectors by alternating elements.
#'
#' Inputs a list of equal length vectors, outputs a vector which is the merged vector by picking elements from each vector in alternating fashion.
#' @param x (a list of vectors) The list of vectors.
#' @keywords vector, merge, alternate, intertwine
#' @export
#' @examples
#' alternate(list(1:3, letters[1:3]))
alternate = function(x) {
  #checks
  if (!is.list(x)) stop("x must be a list!")
  v_lengths = sapply(x, length)
  if (!all_the_same(v_lengths)) stop("lengths of all vectors are not the same!")

  #merge alternatingly
  x_length = length(x)

  #how long does result need to be
  y_length = x_length * length(x[[1]])

  #make template
  y = rep(NA, y_length)

  #insert data with loop
  for (vector_i in seq_along(x)) {
    v_indices = seq(vector_i, y_length, by = x_length)
    y[v_indices] = x[[vector_i]]
  }

  return(y)
}



#' Check numericalness by column.
#'
#' A simple wrapper for \code{vapply}.
#' @param x (something coercible to a data.frame) An object to test.
#' @return Returns a logical vector the same length as the number of columns in x.
#' @export
#' @examples
#' is_numeric_by_col(iris)
is_numeric_by_col = function(df) {
  df = as.data.frame(df)
  vapply(df, FUN = is.numeric, FUN.VALUE = logical(1))
}


#' Is object thoroughly numeric?
#'
#' A more advanced version of \code{\link{is.numeric}}. It wraps the base-r function, but allows for recursive checking inside lists and hence data.frames as well.
#' @param x (any object) An object to test.
#' @param recursive (logical scalar) Whether to use recursive checking. Default=TRUE.
#' @return Returns a logical scalar indicating whether the object is thoroughly numeric.
#' @export
#' @examples
#' is_numeric(iris)
#' is_numeric(iris[-5])
is_numeric = function(x, recursive = TRUE) {
  #vector
  if (is_simple_vector(x)) return(is.numeric(x))

  #array or matrix
  if (is.array(x) || is.matrix(x)) return(is.numeric(x))

  #factor
  if (is.factor(x)) return(FALSE)

  #recursive test?
  if (recursive) {
    #test all elements
    return(all(sapply(x, is_numeric)))
  }

  #otherwise assume FALSE
  FALSE
}


#' Are objects equal?
#'
#' A wrapper for \code{\link{all.equal}} that returns a logical scalar.
#' @param x (any object) The first object.
#' @param y (any object) The second object.
#' @param ... (other named parameters) Further parameters to pass to \code{all.equal}.
#' @return Returns a logical scalar indicating whether the objects are equal.
#' @export
#' @examples
#' are_equal(iris[1:4], iris[-5])
are_equal = function(x, y, ...) {
  test = all.equal(x, y, ...)
  if (is.logical(test)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



#' Format number of digits
#'
#' A wrapper for \code{\link{format}} and \code{\link{round}} that makes sure that a certain number of digits are shown after rounding. Useful for outputting numbers for tables. Vectorized.
#' @param x (numeric vector) The number(s) to format.
#' @param digits (whole number scalar) The number of digits to show.
#' @return Returns a character vector of the same length as x.
#' @export
#' @examples
#' format_digits(c(.1), 2)
#' format_digits(c(.1), 5)
#' format_digits(c(.12345), 2)
#' format_digits(c(.15555), 2)
format_digits = function(x, digits = 2) {
  sapply(x, function(y) {
    format(round(y, digits = digits), nsmall = digits)
  })
}


#function from http://stackoverflow.com/questions/4730551/making-a-string-concatenation-operator-in-r

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


#' Does object have names?
#'
#' A wrapper for names to test for existence of names.
#' @param x (any object) The object to test.
#' @return Logical scalar.
#' @export
#' @examples
#' has_names(iris)
#' has_names(1:4)
has_names = function(x) {
  !is.null(names(x))
}


#' Write session information to a file.
#'
#' A wrapper for writeLines() and capture.output().
#' @param filename (character scalar) The filename of the file to write to.
#' @param print (logical scalar) Whether to also print the output. Default=FALSE.
#' @return Logical scalar.
#' @export
#' @examples
#' write_sessioninfo("session_info.txt", print = TRUE)
write_sessioninfo = function(filename, print = FALSE) {
  writeLines(capture.output(sessionInfo()), con = filename)
}

#' Create list-array
#'
#' A convenience function to create a list-array. A list with dimensions like an array.
#' @param ... (any number of whole numbers or vectors) If given a whole number scalar, will use that to define the length of that dimension. If given a vector, will use the length of that to determine the length of the dimension and use the values as names.
#' @return An n-dimensional list-array.
#' @export
#' @examples
#' make_list_array(1:3, letters[1:3], LETTERS[1:3])
make_list_array = function(...) {

  input_list = list(...)
  names_list = input_list

  v_lengths = sapply(seq_along(input_list), FUN = function(x) {

    #is scalar
    if (length(input_list[[x]]) == 1) {
      names_list[[x]] <<- 1:input_list[[x]]
      return(input_list[[x]])
    } else {
      return(length(input_list[[x]]))
    }
  })

  #how many total cells?
  total_cells = Reduce(f = `*`, init = 1, x = v_lengths)

  #create list-array
  l = as.list(rep(NA, total_cells))
  dim(l) = v_lengths

  #set names
  l = do.call(what = "dimnames<-", args = list(l, names_list))


  l
}


#' Calculate total number of cells in an object
#'
#' Calculate the total number of cells in an object. This is done by finding the product of the lengths of each dimension.
#' @param x (any suitable object) The object.
#' @return A whole number.
#' @export
#' @examples
#' total_cells(iris)
total_cells = function(x) {
  #find the lengths of each dimension
  v_lengths = get_dims(x)

  #get the product and return
  return(product(v_lengths))
}


#' Calculate the product
#'
#' Calculate the product of a given set of numbers.
#' @param ... (any number of numbers) The numbers. Can be multiple arguments, or one argument that is a vector.
#' @return A whole number.
#' @export
#' @examples
#' product(1:3)
#' product(1, 2, 3)
product = function(...) {
  #convert to list
  input = list(...)

  #if given a vector
  if (length(input) == 1) {
    return(Reduce(f = "*", init = 1, x = input[[1]]))
  }

  #if not
  return(Reduce(f = "*", init = 1, x = as.vector(input)))
}

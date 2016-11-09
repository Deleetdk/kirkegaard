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


#' Cut into bins and get proportions
#'
#' Cuts a vector into a specified number of equal sized bins and calculations the proportion of datapoints in each bin. Returns a data.frame.
#' @param x A numeric vector.
#' @param breaks_ The number of bins to use.
#' @export
#' @examples
#' get_prop_table(iris$Sepal.Length)
get_prop_table = function(x, breaks_ = 20){

  x_prop_table = cut(x, 20) %>% table(.) %>% prop.table %>% data.frame
  colnames(x_prop_table) = c("interval", "density")
  intervals = x_prop_table$interval %>% as.character
  fetch_numbers = stringr::str_extract_all(intervals, "\\d\\.\\d*")
  x_prop_table$means = plyr::laply(fetch_numbers, function(x) {
    x %>% as.numeric %>% mean
  })
  return(x_prop_table)
}





#' Get dimensions of object.
#'
#' Returns the dimensions of an object. Also works on atomic (1-d) objects for which base-r dim() returns NULL.
#' @param x (an object) An object.
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
#' @export
#' @examples
#' m = matrix(1:9, nrow=3)
#' n = m
#' rownames(m) = letters[1:3]
#' colnames(m) = LETTERS[1:3]
#' copy_names(m, n)
#' n
copy_names = function(x, y, partialmatching = T) {

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
      stop(stringr::str_c("Dimensions did not match! ", x_dims, " vs. ", y_dims))
    }
  }

  #if using partial matching and dimensions match in number
  if (same_n_dimensions && partialmatching) {
    #loop over each dimension
    for (dim in seq_along(dimnames(x))) {
      #do lengths match?
      if (are_equal(x_dims[dim], y_dims[dim])) {
        trial = try({dimnames(y)[[dim]] = dimnames(x)[[dim]]}, silent = T)
        if (is_error(trial)) dimnames(y)[[dim]] = seq_along(dimnames(y)[[dim]])
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

  #input checks
  assertthat::assert_that(is.vector(x))
  assertthat::assert_that(is_whole_number(k))
  assertthat::assert_that(is.logical(uneven))

  #check length
  if (!uneven) {
    if (length(x) %% k != 0) {
      stop(stringr::str_c("The length of n was not integer disible by n! ", length(x), "%%", k, "=", length(x) %% k))
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

  #checks
  #how many vectors
  v_vectors = length(list)

  #longest vector
  v_max = max(sapply(list, length))

  #fill out
  list = lapply(list, fill_in, length = v_max)

  #make data.frame
  df = matrix(ncol = 2*v_vectors, nrow = v_max) %>% as.data.frame
  v_names = stringr::str_c(rep(names(list), each = 2), c(name_suffix, value_suffix))
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
#' @export
#' @examples
#' silence(warning())
#' silence(log(-1))
#' silence(warning("test"))
#' silence(warning("test"), warnings = T)
#' silence(message("test"))
#' silence(message("test"), messages = T)
silence = function(expr, warnings = F, messages = F, startupmessages = F) {
  if (!warnings & !messages & !startupmessages) {
    suppressPackageStartupMessages(suppressWarnings(suppressMessages(expr)))
  } else if (warnings & !messages & !startupmessages) {
    suppressPackageStartupMessages(suppressMessages(expr))
  } else if (!warnings & messages & !startupmessages) {
    suppressPackageStartupMessages(suppressWarnings(expr))
  } else if (!warnings & !messages & startupmessages) {
    suppressWarnings(suppressMessages(expr))
  } else if (warnings & messages & !startupmessages) {
    suppressPackageStartupMessages(expr)
  } else if (warnings & !messages & startupmessages) {
    suppressMessages(expr)
  } else if (!warnings & messages & startupmessages) {
    suppressWarnings(expr)
  } else if (warnings & messages & startupmessages) {
    expr
  }

}


#' Merge vectors by alternating elements.
#'
#' Inputs a list of equal length vectors, outputs a vector which is the merged vector by picking elements from each vector in alternating fashion.
#' @param x (a list of vectors) The list of vectors.
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


#' Write session information to a file.
#'
#' A wrapper for writeLines() and capture.output().
#' @param filename (chr scalar) The filename of the file to write to. Default is "sessions_info.txt".
#' @param print (logical scalar) Whether to also print the output. Default=FALSE.
#' @export
#' @examples
#' write_sessioninfo("session_info.txt", print = TRUE)
write_sessioninfo = function(filename = "sessions_info.txt", print = FALSE) {
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
  return(prod(v_lengths))
}



#' Last value
#'
#' Find the last value of a vector. By default, will take the last non-NA value.
#' @param x (a vector) A vector.
#' @param na.rm (log scalar) Whether to ignore missing values (default yes).
#' @return The last value (scalar).
#' @export
#' @examples
#' last_value(1:3)
#' last_value(c(1:3, NA))
#' last_value(rep(NA, 3))
last_value = function(x, na.rm = T) {
  #all NA?
  if (all(is.na(x))) return(NA)

  #dont ignore NA?
  if (!na.rm) return(rev(x)[1])

  #otherwise
  rx = rev(x)
  for (i in 1:length(x)) {
    if (!is.na(rx[i])) {
      return(rx[i])
    }
  }

  NA
}


#' Better table
#'
#' Returns a data frame table with both counts and percents.
#' @param x (a vector) A vector.
#' @param prop (lgl) Whether to return proportions instead of percents. Default no.
#' @param include_NA (lgl) Whether to count NA values. Default yes.
#' @return A data frame (tbl).
#' @export
#' @examples
#' sample(letters[1:10], size = 100, replace = T) %>% table2
table2 = function(x, prop = F, include_NA = T) {
  tbl = table(x, exclude = NULL)
  d = tibble::data_frame("Group" = names(tbl),
                 "Count" = as.numeric(tbl)
  )
  #percent/prob
  if (!prop) d$Percent = d$Count / sum(d$Count) * 100 else d$Proportion = d$Count / sum(d$Count)

  d
}

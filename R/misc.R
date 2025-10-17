## Misc other functions

#' `NA` to `FALSE`
#'
#' Helpful small function when dealing with non-tidyverse subsetting.
#'
#' @param x A vector
#'
#' @return A vector of the same length as x
#' @export
#'
#' @examples
#' x = c(T, F, NA, T)
#' NA_to_F(x)
NA_to_F = function(x) {
  assertthat::assert_that(is.logical(x))
  x[is.na(x)] = F
  x
}

#' `NA` to specified value
#'
#' Replace `NA` values in a vector with a specified value.
#'
#' @param x A vector
#' @param replacement The value to replace `NA` with
#'
#' @returns A vector of the same length as x
#' @export
#'
#' @examples
#' c(1, 2, 3, 4) %>% NA_to_X(3)
NA_to_X = function(x, replacement) {
  x[is.na(x)] = replacement
  x
}


#' Conert infinite values to `NA`
#'
#' @param x A vector
#'
#' @return A vector of the same length as x
#' @export
#'
#' @examples
#' c(1, 2, Inf, 3) %>% inf_to_NA()
inf_to_NA = function(x) {
  x[is.infinite(x)] = NA
  x
}


#' Is x nullish?
#'
#' Sometimes useful to have flexible input to disable a feature that accepts NULL, NA, or F as meaning "no".
#'
#' @param x Length 0 or 1 input
#' @param null Whether NULL means "no"
#' @param false Whether False means "no"
#' @param na Whether NA means "no"
#' @param zero Whether 0 means "no"
#'
#' @return Logical scalar
#' @export
#'
#' @examples
#' is_nullish(NULL)
#' is_nullish(NA)
#' is_nullish(F)
#' is_nullish(0)
#' is_nullish(NA, na = F)
is_nullish = function(x, false = T, na = T, zero = T, null = T) {
  #too long
  if (length(x) > 1) stop("Length > 1. Wrong input.", call. = F)

  #first try for NULL
  if (null && is.null(x)) return(T)
  if (!null && is.null(x)) return(F)

  #then the others
  x_false = isFALSE(x)
  x_na = is.na(x)
  x_zero = identical(x, 0) || identical(x, 0L)

  #either or
  if (false & na & zero) return(x_false || x_na || x_zero)
  if (!false & na & zero) return(x_na || x_zero)
  if (false & !na & zero) return(x_false || x_zero)
  if (false & na & !zero) return(x_false || x_na)
  if (!false & !na & zero) return(x_zero)
  if (!false & na & !zero) return(x_na)
  if (false & !na & !zero) return(x_false)

  F
}


#' Restore NAs in vector
#'
#' Restore NAs in a vector from known positions. Useful for dealing with NAs in functions and predictions.
#'
#' @param x Non-NA values
#' @param na_pos Positions of NA, either logical of total length, or integer indexes
#' @param len Length of vector if known (optional)
#'
#' @return A vector with NAs restored in place
#' @export
#'
#' @examples
#' restore_NAs(c(1, 3, 5), c())
#' restore_NAs(c(1, 3, 5), 4:10)
#' restore_NAs(c(1, 3, 5), 1:5)
#' restore_NAs(c(1, 3, 5), c(2, 4, 6), 6)
#' restore_NAs(c(1, 3, 5), c(2, 4, 6))
restore_NAs = function(x, na_pos, len=NULL) {
  #no NAs? return as is
  if (length(na_pos) == 0) return(x)

  #if length known
  #then easy method
  if (!is.null(len)) {
    y = rep(NA, len)
    if (is.logical(na_pos)) na_pos = which(na_pos)
    y[(setdiff(seq_along(y), na_pos))] = x
    return(y)
  }

  #if not, more annoying
  #if NA is logical, then easy
  if (is.logical(na_pos)) {
    len = length(na_pos)
    na_pos = which(na_pos)
    return(restore_NAs(x, na_pos, len))
  }

  #else
  len = length(x) + length(na_pos)
  restore_NAs(x, na_pos, len)
}



##Combines lower and upper part of two matrices into one. Additional option for the diagonal.
#Credit to: http://stackoverflow.com/questions/13115720/how-do-i-combine-the-upper-tri-of-one-matrix-with-the-lower-tri-of-another-in-r
#' Combine upper and lower part of a matrix.
#'
#' Returns a matrix composed of an upper and a upper part.
#' @param .upper.tri a matrix or data.frame for the upper trianagle.
#' @param .lower.tri a matrix or data.frame for the lower trianagle.
#' @param .diag a value to insert in the diagonal. Defaults to NA.
#' @export
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
  #NULL per standard
  if (is.null(x)) return(dim(x))

  #some questionable input
  if (is.function(x)) return(dim(x))

  #if vector, then it has 1 dimesion with length as its length
  if (is.vector(x)) {
    return(length(x))
    } else {
    return(dim(x))
  }
}


#' How many dimensions does an object have?
#'
#' Useful wrapper for [get_dims()].
#' @param x (an object) An object.
#' @export
#' @examples
#' #flexible
#' ndims(1:3) #vector
#' ndims(iris) #data frame
#' ndims(array(1:27, dim = rep(3, 3))) #array
#' ndims(mean) #function
ndims = function(x) {
  length(get_dims(x))
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
silence = function(expr, warnings = FALSE, messages = FALSE,
                   startupmessages = FALSE, output = FALSE) {

  if (!output) {
    # Suppress output - wrap the whole thing in capture.output
    capture.output({
      result <- if (!warnings & !messages & !startupmessages) {
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
    })
    return(result)
  }

  # Don't suppress output - original logic
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
  v_lengths = purrr::map_int(x, length)
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


#' Write session information to a file.
#'
#' A wrapper for writeLines() and capture.output().
#' @param filename (chr scalar) The filename of the file to write to. Default is "sessions_info.txt".
#' @param print (logical scalar) Whether to also print the output.
#' @export
#' @examples
#' write_sessioninfo("session_info.txt", print = TRUE)
write_sessioninfo = function(filename = "sessions_info.txt", print = T) {
  y = capture.output(sessionInfo())
  if (print) cat(paste(y, collapse = "\n"))
  writeLines(y, con = filename)
  invisible(y)
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

  v_lengths = purrr::map(seq_along(input_list), function(x) {

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
#'
#' Return 0 on `NULL`.
#' @param x (any suitable object) The object.
#' @return An integer.
#' @export
#' @examples
#' total_cells(iris)
total_cells = function(x) {
  if (is.null(x)) return(0)

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
#' @param prop (lgl) Whether to return proportions instead of percents.
#' @param include_NA (lgl) Whether to count NA values.
#' @param sort (lgl) Whether to sort by descending order. If null, sorts by group factor levels.
#' @return A data frame (tbl).
#' @export
#' @examples
#' #table and sort descending
#' sample(letters[1:10], size = 100, replace = T) %>% table2()
#' #ascending
#' sample(letters[1:10], size = 100, replace = T) %>% table2(sort_descending = F)
#' #using factor levels
#' sample(letters[1:10], size = 100, replace = T) %>% table2(sort_descending = NULL)
table2 = function(x, prop = F, include_NA = T, sort_descending = T) {

  #NA param
  if (include_NA) {
    NA_param = "always"
  } else {
    NA_param = "no"
  }

  #get regular table
  tbl = table(x, useNA = NA_param)

  #as data frame
  d = tibble::tibble("Group" = names(tbl),
                         "Count" = as.numeric(tbl)
  )

  #percent/prob
  if (!prop) d$Percent = d$Count / sum(d$Count) * 100 else d$Proportion = d$Count / sum(d$Count)

  #sort?
  if (!is.null(sort_descending)) {
    if (sort_descending) {
      d %<>% dplyr::arrange(-Count)
    } else {
      d %<>% dplyr::arrange(Count)
    }
  } else {
    #reset factor levels if existed
    if (is.factor(x)) d$Group = factor(d$Group, levels = levels(x))
  }

  d
}





#' Find duplicated elements
#'
#' Find groups of duplicates elements in a vector.
#' @param x (vector) A vector.
#' @return a list
#' @export
#' @examples
#' find_duplicates(c(1, 1, 2, 2, 3, 4, 5, 5))
find_duplicates = function(x) {
  #find duplicated elements
  dup_ele = unique(x[duplicated(x)])

  #group indexes by the duped value
  group_ids = lapply(dup_ele, function(ele) which(x == ele))

  #add names
  names(group_ids) = dup_ele

  group_ids
}



#' Updates the package from Github
#' @export
#' @examples
#' update_package()
update_package = function(...) {
  devtools::install_github("deleetdk/kirkegaard")
}



#' Seq along rows
#'
#' Equivalent to seq_along, but for rows.
#' @param df (vector) A vector.
#' @return An integer vector.
#' @export
#' @examples
#' #example on iris
#' seq_along_rows(iris)
#' #does not fail on 0-row data frames
#' seq_along_rows(iris[-c(1:150), ])
seq_along_rows = function(df) {
  #input check
  if (is.null(df)) stop("`df` cannot be NULL", call. = F)
  stop_if(ndims(df) != 2, msg = sprintf("`df` must have exactly 2 dimensions, but it had %d", ndims(df)))

  #no rows
  if (nrow(df) == 0) return(integer())

  1:nrow(df)
}


#' Remove duplicate elements
#'
#' Remove duplicated elements from an object. Simple wrapper for [base::duplicated()] to use in functional contexts.
#' @details
#' Calls either `x[!duplicated(x)]` or `x[!duplicated(x), ]` depending on input dimensions. Will work for objects with a defined method for [base::duplicated()].
#' @param x An object.
#' @return Object of the same type as input.
#' @export
#' @examples
#' #simple vector
#' simple_vector = sample(1:5, size = 20, replace = T) %T>% print
#' simple_vector %>% unduplicate
#' #example on iris (data frame)
#' iris_with_dups = iris[c(1, 1, 2, 2, 3, 4, 1, 5, 2, 4), ] %T>% print
#' iris_with_dups %>% unduplicate
unduplicate = function(x) {
  #is multidimensional?
  #this catches matrices and data frames, and d>2 possibly arrays
  if (length(dim(x)) > 1) {
    return(x[!duplicated(x), ])
  }

  #rely on generic function
  x[!duplicated(x)]
}




#' Unique encoding
#'
#' @param x A vector of values
#'
#' @return A list
#' @export
#'
#' @examples
#' c(1, 2, 2, 3, 3, 3) %>% uniq_encoding()
#' c(3, 1, 3, 2, 3, 2) %>% uniq_encoding()
#' c(NA, 1, 2, 2, 3, 3, 3, NA) %>% uniq_encoding()
#' c(1, 2, 2, 3, 3, 3) %>% as.character() %>% uniq_encoding()
#' c(1, 2, 2, 3, 3, 3) %>% factor() %>% uniq_encoding()
uniq_encoding = function(x) {
  #convert to chr
  if (is.factor(x)) {
    warning("Factor automatically converted to character", call. = F)
    x = as.character(x)
  }

  #unique values
  #we use this above levels of factor to support NA
  xu = unique(x)

  #class
  xclass = class(x)[1] #ignore any secondary classes

  #compute
  list(
    class = class(x),
    levels = xu,
    positions = purrr::map(xu, function(z) {
      #for non-NA
      if (!is.na(z)) return(which(z == x))

      #NAs
      which(is.na(x))
    })
  )
}

#reverse
#' Reverse unique encoding
#'
#' @param x Output from [kirkegaard::uniq_encoding()]
#'
#' @return A vector
#' @export
#'
#' @examples
#' c(1, 2, 2, 3, 3, 3) %>% uniq_encoding() %>% rev_uniq_encoding()
#' c(1, 2, 2, 3, 3, 3) %>% uniq_encoding() %>% rev_uniq_encoding()
rev_uniq_encoding = function(x) {
  #if empty, return empty
  if (is.null(x$levels)) return(c())
  #allocate
  y = character(length = max(map_int(x$positions, max)))

  #loop and fill
  for (i in seq_along(x$levels)) {
    y[x$positions[[i]]] = x$levels[i]
  }

  #class
  class(y) = x$class

  y
}





#tired of using plyr's

#' Map values
#'
#' Copy of plyr::mapvalues
#'
#' @param x A vector
#' @param from A vector to change from
#' @param to A vector to change to, can be same length or length 1 (recycled)
#' @param warn_missing Warn if from values not present
#'
#' @return A vector
#' @export
#'
#' @examples
#' mapvalues(0:9, from = 3:5, to = c(-1, -1, -1))
#' mapvalues(0:9, from = 3:5, to = -1)
mapvalues = function (x, from, to, warn_missing = T) {
  #input lengths
  if (length(from) != length(to)) {
    #recycle?
    if (length(to) == 1) {
      to = rep(to, length(from))
    } else {
      stop("`from` and `to` vectors are not the same length, nor is `to` length 1.")
    }
  }

  #atomic check
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }

  #just change factor levels if factor
  if (is.factor(x)) {
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }

  #map values if not
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ",
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }

  x[!mapidxNA] <- to[mapidx[!mapidxNA]]

  x
}



#' Count decimals
#'
#' @param x Vector of values
#'
#' @return Vector of values
#' @export
#'
#' @examples
#' c(1, 1.2, 1.23) %>% count_decimals()
#' c() %>% count_decimals()
count_decimals = function(x) {
  #length zero input
  if (length(x) == 0) return(numeric())

  #count decimals
  x_nchr = x %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
  x_int = floor(x) %>% abs() %>% nchar()
  x_nchr = x_nchr - 1 - x_int
  x_nchr[x_nchr < 0] = 0

  x_nchr
}




is_inst <- function(pkg) {
  nzchar(system.file(package = pkg))
}


#' Reverse `table()`
#'
#' Function from https://stackoverflow.com/questions/30481496/is-there-a-general-inverse-of-the-table-function.
#' @param x A table
#'
#' @return A data frame
#' @export
#'
#' @examples
#' mpg %>% nrow() #324
#' inv_table(table(mpg$cyl, mpg$class, mpg$manufacturer)) #also 324
#' inv_table(Titanic)
#'
inv_table = function(x) {
  #to data frame
  x = as.data.frame(x)

  #reverse
  y <- x[rep(rownames(x),x$Freq),1:(ncol(x)-1)]

  #fix names
  rownames(y) <- c(1:nrow(y))

  y
}



#' Calculate overlap metrics
#'
#' @param x A vector
#' @param y A vector
#'
#' @return A named vector of metrics of overlap
#' @export
#'
#' @examples
#' overlap_metrics(
#' c(1:3),
#' c(3:6)
#' )
overlap_metrics = function(x, y) {
  #compute
  xy_intersect = intersect(x, y)
  xy_union = union(x, y)
  x_in_y = x[x %in% y]
  y_in_x = y[y %in% x]

  #summarize
  c(
    x_count = length(x),
    y_count = length(y),
    xy_union_count = length(xy_union),
    xy_intersection_count = length(xy_intersect),
    prop_overlap_of_union = length(xy_intersect) / length(xy_union),
    x_in_y_count = length(x_in_y),
    x_in_y_prop = length(x_in_y) / length(x),
    y_in_x_count = length(y_in_x),
    y_in_x_prop = length(y_in_x) / length(y)
  )
}


#named vector to data frame
#' Convert a named data to a data frame
#'
#' @param x A vector
#' @param name_col The name of the variable to use for the names
#' @param value_col The name of the variable to use for the values
#'
#' @return A data frame
#' @export
#'
#' @examples
#' named_vector_to_df(c(a = 1, b = 2))
#' named_vector_to_df(c(a = 1, b = 2), "x", "y")
named_vector_to_df = function(x, name_col = "name", value_col = "value") {
  tibble(
    name = names(x),
    value = x
  ) %>% set_names(c(name_col, value_col))
}


#' Encode combinations
#'
#' Given a data frame of binary values, encode the combinations of TRUE values. Useful for combining multiple yes/no options in different variables into a single column, e.g. self-identified race/ethnicity.
#'
#' @param x A data frame of binary values, which will be converted to logicals.
#'
#' @return A chracter vector of encoded combinations
#' @export
#'
#' @examples
#' tibble(A = c(T, F, F, T, F), B = c(F, T, F, F, F), C = c(F, F, T, F, F), D = c(F, F, F, T, F)) %>% encode_combinations()
encode_combinations = function(x, collapse = ", ") {
  #rowwise
  x %>%
    #ensure they are logicals
    map_df(as.logical) %>%
    #then go rowwise
    plyr::alply(.margins = 1, function(row) {

      #if only one option
      if (sum(row) == 1) return(names(row)[unlist(row)])

      #if multiple, combine with commas
      return(str_c(names(row)[unlist(row)], collapse = ", "))
    }) %>%
    unlist() %>% unname()
}


#' Convert date to decimal year
#'
#' @param x A date vector
#'
#' @returns A decimal year vector
#' @export
#'
#' @examples
#' as_decimal_year(as.Date("2020-01-01"))
#' today() |> as_decimal_year() |>  decimal_year_to_date()
as_decimal_year = function(x) {
  tibble(
    date = x
  ) %>%
    mutate(
      year = year(date),
      day_of_year = yday(date),
      year_length = if_else(leap_year(date), 366, 365),
      decimal_year = year + (day_of_year - 1) / year_length
    ) %>% pull(decimal_year)
}


#' Convert decimal year to date
#'
#' @param decimal_year A decimal year vector
#'
#' @returns A date vector
#' @export
#'
#' @examples
#' decimal_year_to_date(2020.5)
#' today() |> as_decimal_year() |>  decimal_year_to_date()
decimal_year_to_date <- function(decimal_year) {
  tibble(decimal_year = decimal_year) %>%
    mutate(
      year = floor(decimal_year),
      fractional_year = decimal_year - year,
      year_length = if_else(leap_year(as.Date(paste0(year, "-01-01"))), 366, 365),
      day_of_year = round(fractional_year * year_length) + 1,
      date = as.Date(paste0(year, "-01-01")) + (day_of_year - 1)
    ) %>% pull(date)
}

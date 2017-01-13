### functions for data structure transformation

#' Vector to data frame
#'
#' Transform a vector to a data.frame. If it has names, these will be added as rownames unless name_col is specified to a chr.
#' @param x (vector) A vector.
#' @param value_col (chr) Name of the column with values. If NA, will use the name of x.
#' @param name_col (chr/lgl) Name of the column with names. If given, will not use rownames and use an explicit column.
#' @return a data.frame
#' @export
#' @examples
#' v_to_df(1:4)
#' v_to_df(c(a = 1, b = 2, c = 3, d = 4))
#' v_to_df(c(a = 1, b = 2, c = 3, d = 4), name_col = "name")
v_to_df = function(x, value_col = "value", name_col = F) {
  if (is.na(value_col)) value_col = kirkegaard::object_to_string(x)

  #has names?
  if (has_names(x)) {
    #explicit names?
    if (is_logical(name_col) && !name_col) {
      y = data.frame(x)
      names(y) = value_col
    } else {
      y = data.frame(names(x), x)
      names(y) = c(name_col, value_col)
      rownames(y) = NULL
    }
  } else {
    y = data.frame(x)
    names(y) = value_col
  }

  y
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

#' Split vector every k elements
#'
#' Split a vector every k elements. Returns a list.
#' @param x (vector) A vector to split.
#' @param k (whole number scalar) Split every k elements.
#' @param uneven (logical scalar) Whether to accept a split that would be uneven. If yes, the last group will be smaller than the others. Defaults to TRUE.
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



#' Merge vectors.
#'
#' Merge data from two vectors, either by names or by location. The second is merged into the first.
#' @param v1 (vector) The first vector.
#' @param v2 (vector) The second vector.
#' @param byname (logical scalar) Whether to merge by name. Default = FALSE.
#' @param overwrite_NA (logical scalar) When using byname, whether to overwrite NAs. Default = FALSE.
#' @export
#' @examples
#' #without names
#' t1 = c(NA, 2, 3, NA)
#' t2 = c(1, NA, NA, 4)
#' merge_vectors(t1, t2)
#' #with names
#' t1_n = c(NA, 2, 3, NA);names(t1_n) = letters[1:4]
#' t3_n = c(1, NA, NA, 4, 5);names(t3_n) = letters[1:5]
#' merge_vectors(t1_n, t3_n, byname = T)
merge_vectors = function(v1, v2, byname = FALSE, overwrite_NA = FALSE) {
  #checks
  if (!is.vector(v1)) stop("v1 was not a vector!")
  if (!is.vector(v2)) stop("v2 was not a vector!")

  #vectors of NA
  v1_NA = is.na(v1)
  v2_NA = is.na(v2)

  #use v1 as base
  v3 = v1

  #byname
  if (byname) {
    #check names
    if (!(has_names(v1) && has_names(v2))) stop("when using byname, vectors must have names!")

    #overwrite?
    if (overwrite_NA) {
      v3[names(v2)] = v2
      return(v3)
    }

    #not overwrite
    v2 = na.omit(v2) #remove NAs
    v3[names(v2)] = v2
    return(v3)
  }

  #not byname
  if (!(length(v1) == length(v2))) stop("when not using byrow, lengths must be identical!")

  #not overwrite
  #because overwriting serves no purpose when not using names, it's just replacing the entire vector
  v3[!v2_NA] = v2[!v2_NA]
  v3
}


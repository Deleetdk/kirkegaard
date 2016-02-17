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


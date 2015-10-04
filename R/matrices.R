#' Determine the dimensions of a symmetric matrix from the length of a vector of the upper or lower half.
#'
#' Returns a single number which is the length of the sides of the original matrix.
#' @param x The length of the vector of one half of the matrix.
#' @param diag Whether the diagonal values are included or not. Defaults to F.
#' @keywords matrix, size, symmetric, lower, upper
#' @export
#' @examples
#' MAT_find_size()
MAT_find_size = function(x, diag=F) {
  #this is based on the equations
  #k = length of

  #if diagonal values are present
  if (diag==T) {
    s = -1/2 + sqrt(1/4 + 2*x)
  }

  #if diagonal values are not present
  if (diag==F) {
    s = 1/2 + sqrt(1/4 + 2*x)
  }
  #verify
  if (s != floor(s)) stop("Could not determine matrix size. Incorrect input.")

  return(s)
}



#' Reconstructs a symmetric matrix based on a vector of values of one of the halves.
#'
#' Returns a symmetric matrix.
#' @param x A vector of values.
#' @param diag Whether the diagonal values are included or not. Defaults to F.
#' @param byrow Whether the values were extracted by row or column. Defaults to F which matches the output from e.g. dist().
#' @keywords matrix, symmetric, lower, upper
#' @export
#' @examples
#' MAT_vector2full()
MAT_vector2full = function(x, diag=F, byrow=F) {
  #code made from http://r.789695.n4.nabble.com/how-to-convert-the-lower-triangle-of-a-matrix-to-a-symmetric-matrix-td823271.html
  X = base::diag(0, MAT_find_size(length(x), diag))
  if (byrow) {
    X[upper.tri(X, diag=diag)] = x
  } else {
    X[lower.tri(X, diag=diag)] = x
  }
  X = X + t(X) - diag(diag(X))
  return(X)
}

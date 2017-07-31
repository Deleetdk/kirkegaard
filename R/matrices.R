#' Determine the dimensions of a symmetric matrix from the length of a vector of the upper or lower half.
#'
#' Returns a single number which is the length of the sides of the original matrix.
#' @param x The length of the vector of one half of the matrix.
#' @param diag Whether the diagonal values are included or not. Defaults to F.
#' @export
#' @examples
#' MAT_find_size(3)
MAT_find_size = function(x, diag = F) {
  #this is based on the equations
  #k = length of

  #if diagonal values are present
  if (diag) {
    s = -1/2 + sqrt(1/4 + 2*x)
  }

  #if diagonal values are not present
  if (!diag) {
    s = 1/2 + sqrt(1/4 + 2*x)
  }
  #verify
  if (s != floor(s)) stop("Could not determine matrix size. Incorrect input.")

  return(s)
}



#' Reconstructs a symmetric matrix based on a vector of values of one of the halves.
#'
#' Returns a symmetric matrix.
#' @param x (vector) A vector of values.
#' @param diag (logical scalar) Whether the diagonal values are included or not. Defaults to FALSE.
#' @param byrow (logical scalar) Whether the values were extracted by row or column. Defaults to FALSE which matches the output from e.g. dist().
#' @param diag_value (scalar) Which value to fill in the diagonal if necessary.
#' @export
#' @examples
#' MAT_vector2full(1:3)
MAT_vector2full = function(x, diag = FALSE, byrow = FALSE, diag_value = 0) {

  #make matrix with cell numbers
  m_size = MAT_find_size(length(x), diag)
  m = matrix(1:(m_size^2), nrow = m_size, byrow = byrow)
  #which cells from where?
  m_lower = MAT_half(m, diag = diag, lower = !byrow)

  #make full
  full = matrix(diag_value, nrow = nrow(m), ncol = ncol(m))
  full[m_lower] = x
  full = t(full)
  full[m_lower] = x
  full
}



#' Get half of a matrix.
#'
#' Returns a vector with the values in one half of the matrix. Which half and whether diagonal values are included can be specified.
#' @param x A matrix or matrix-coercable object.
#' @param lower Whether to extract the lower half. Defaults to T. If F, then the upper half is extracted.
#' @param diag Whether the diagonal values should be included or not. Defaults to F.
#' @export
#' @examples
#' cor(iris[-5]) #can't summarize this data due to diagonal
#' MAT_half(cor(iris[-5])) #this data we can
MAT_half = function(x, lower = T, diag = F) {
  #coerce to matrix
  x = as.matrix(x)

  #get half
  if (lower) {
    x = x[lower.tri(x, diag = diag)]
  } else {
    x = x[upper.tri(x, diag = diag)]
  }

  #return
  return(x)
}


#' Divide a matrix or df row-wise by a vector
#'
#' The default use of / divides column-wise, but sometimes row-wise is needed. This function restores the dimnames.
#' @param data (mat or df) Data to divide.
#' @param divisor (num/int vectr) Divisor to use.
#' @export
#' @return Returns a data.frame/matrix with the orignal dimnames.
#' @examples
#' m = matrix(1:10, ncol=2)
#' m
#' m/c(1, 2) #divide column-wise
#' MAT_divide_rowwise(m, c(1, 2)) #row-wise
#' #works on data.frames too
#' MAT_divide_rowwise(iris[-5], c(1, 999, 1, 999)) %>% head
MAT_divide_rowwise = function(data, divisor) {
  #check input
  is_(data, class = c("data.frame", "matrix"), error_on_false = T)
  is_(divisor, class = c("numeric", "integer"), error_on_false = T)

  #divide
  #sensible method
  # d2 = data %>%
  #   t %>%
  #   divide_by(divisor) %>%
  #   t %>%
  #   as.data.frame

  #fastest but unclear
  #http://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r
  d2 = (as.matrix(data) %*% diag(1/divisor))

  #set names
  copy_names(data, d2)

  #if data.frame
  if (is.data.frame(data)) d2 = as.data.frame(d2)

  #return
  d2
}


#' Half to full matrix
#'
#' From a half matrix, recreate a full symmetric matrix. Uses either lower or upper triangle.
#' @param mat (matrix) A symmetric matrix
#' @param lower (lgl) Whether to use the lower or higher triangle.
#' @param diag (lgl) Whether the diagonal values should be included or not.
#' @export
#' @return A matrix
#' @examples
#' m = matrix(1:9, ncol=3)
#' rownames(m) = letters[1:3]
#' colnames(m) = LETTERS[1:3]
#' m %>% MAT_half2full
#' m %>% MAT_half2full(lower=F)
#' m %>% MAT_half2full(diag=T)
#' m %>% MAT_half2full(lower=F, diag=T)
MAT_half2full = function(mat, lower = T, diag = F) {
  mat

  #check symmetry
  if (!ncol(mat) == nrow(mat)) stop("Matrix must be square.")

  #get half, then transform to full symmetric
  new_mat = mat %>% MAT_half(lower = lower, diag = diag) %>% MAT_vector2full(byrow = !lower, diag = diag)

  #copy names
  copy_names(mat, new_mat)

  #preserve integers
  if (is.integer(mat)) mode(new_mat) = "integer"

  #out
  new_mat
}

### Psychometrics

# score_items -------------------------------------------------------------
#function to score multiple choice data

#' Score multiple choice items
#'
#' Score a data.frame with multiple choice items using an answer key. Each column has a answers for one question.
#' @param df (data.frame) A data.frame with responses.
#' @param key (vector) A vector with the correct responses.
#' @keywords date.frame, score, multiple choice
#' @export
#' @examples
#' score_items()
score_items = function(df, key) {
  library(magrittr)
  #checks
  if (ncol(df) != length(key)) stop("Key length does not match data.frame! There must be the same number of columns as keys.")

  #score
  df2 = lapply(seq_along(df), function(x) {
    df[[x]] == key[x]
  }) %>% as.data.frame

  colnames(df2) = colnames(df)

  return(df2)
}

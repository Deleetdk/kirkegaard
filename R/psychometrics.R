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
#' @details
#' Returns a data.frame of the same size as that given with the values 0 representing incorrect answers and 1 representing correct answers.
#' @examples
#' library(psych) #for ICAR data
#' d = score_items(iqitems, c(4,4,4, 6,  6,3,4,4,   5,2,2,4,   3,2,6,7)) #score and save
#' psych::alpha(d) #examine internal consistency
score_items = function(df, key) {
  library(magrittr)

  #checks
  if (ncol(df) != length(key)) stop("Key length does not match data.frame! There must be the same number of columns as keys.")

  #score
  df2 = lapply(seq_along(df), function(x) {
    (df[[x]] == key[x]) %>% as.numeric() #convert to numeric
  }) %>% as.data.frame #convert to df

  #names
  colnames(df2) = colnames(df)

  return(df2)
}

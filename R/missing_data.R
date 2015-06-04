## FUNCTIONS FOR DEALING WITH MISSING DATA

#' Missing datapoint counter, case-level
#'
#' Counts the number of missing datapoints per case
#' @param x a matrix or data.frame
#' @keywords missing data
#' @export
#' @examples
#' miss_case()
miss_case = function(x){
  y = apply(x, 1, is.na)
  y = apply(y, 2, sum)
  return(y)
}

#' Missing datapoint counter, variable-level
#'
#' Counts the number of missing datapoints per variable
#' @param x a matrix or data.frame
#' @keywords missing data
#' @export
#' @examples
#' miss_table()
miss_table = function(x){
  y = apply(x, 2, is.na)
  y = apply(y, 2, sum)
  return(y)
}

#' Missing data histogram with ggplot2.
#'
#' Returns a ggplot2 histogram plot.
#' @param df a data.frame.
#' @param percent whether to use percent or not. Defaults to true.
#' @keywords missing data, histogram, plot
#' @export
#' @examples
#' plot_miss()
plot_miss = function(df, percent=T) {
  m = miss.case(df)
  d = data.frame(number.of.NA = m)
  max.miss = max(m)
  min.miss = min(m)
  
  if (percent) {
    d$percent = (d$number.of.NA/sum(d$number.of.NA))*100
    g = ggplot(data = d, aes(x = factor(number.of.NA))) +
      geom_bar(aes(y = ((..count..)/sum(..count..))*100)) + 
      scale_y_continuous('percent') +
      xlab("Number of NAs") +
      scale_x_discrete(breaks=min.miss:max.miss)
    return(g)
  }
  else {
    g = ggplot(data = d, aes(x = factor(number.of.NA))) +
      geom_histogram() +
      xlab("Number of NAs") +
      scale_x_discrete(breaks=min.miss:max.miss)
    return(g)
  }
}

#' Wrapper for matrixplot()
#'
#' Wrapper for matrixplot() from VIM, but automatically substrings the variable names to 8 characters.
#' @param df a data.frame.
#' @keywords missing data, plot, matrix, VIM
#' @export
#' @examples
#' matrixplot2()
matrixplot2 = function(df) {
	library(VIM) #load VIM if not already loaded
  	return(matrixplot(df, labels=substr(colnames(df),1,8)))
}


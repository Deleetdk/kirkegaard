#' Histogram with an empirical density curve and a vertical line at the mean
#'
#' Plots a histogram with an empirical density curve and a vertical line at the mean using ggplot2.
#' @param df A data.frame with variables.
#' @param var A string of the name of the variable to use.
#' @keywords ggplot2, plot, density, histogram
#' @export
#' @examples
#' GG_denhist()
GG_denhist = function(df, var) {
  library(ggplot2)

  g = ggplot(df, aes_string(var)) +
    geom_histogram(aes(y=..count..),  # Histogram with density instead of count on y-axis
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
    geom_vline(aes(xintercept=mean(r, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1)

  return(g)
}

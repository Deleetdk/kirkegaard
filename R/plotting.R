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
    geom_histogram(aes(y=..density..),  # Histogram with density instead of count on y-axis
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
    geom_vline(aes(xintercept=mean(r, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1)

  return(g)
}


#' Scatter plot with kmeans clustering
#'
#' Performs kmeans and factor analysis on a dataset. Then plots a scatter plot using the first two factors (orthogonal) and color codes cases by their kmeans cluster.
#' @param df A data.frame with variables.
#' @param clusters The number of clusters to find.
#' @param runs Number of runs to use. The best run is used in the plot.
#' @param standardize Whether to standardize the data first. Defaults to TRUE.
#' @keywords ggplot2, plot, density, histogram
#' @export
#' @examples
#' plot_kmeans()
plot_kmeans = function(df, clusters, runs, standardize=T) {
  library(psych)
  library(ggplot2)

  #standardize
  if (standardize) df = std_df(df)

  #cluster
  tmp_k = kmeans(df, centers = clusters, nstart = 100)

  #factor
  tmp_f = fa(df, 2, rotate = "none")

  #collect data
  tmp_d = data.frame(matrix(ncol=0, nrow=nrow(df)))
  tmp_d$cluster = as.factor(tmp_k$cluster)
  tmp_d$fact_1 = as.numeric(tmp_f$scores[, 1])
  tmp_d$fact_2 = as.numeric(tmp_f$scores[, 2])
  tmp_d$label = rownames(df)

  #plot
  g = ggplot(tmp_d, aes(fact_1, fact_2, color = cluster)) + geom_point() + geom_text(aes(label = label), size = 3, vjust = 1, color = "black")
  return(g)
}



#' Scatter plot with regression line and correlation information using ggplot2
#'
#' Plots a scatterplot with a regression line and correlation information. Returns a ggplot2 object.
#' @param df A data.frame with variables.
#' @param x_var X variable string.
#' @param y_var Y variable string.
#' @param text_pos Where to put the text. Defaults to top right ("tl") if correlation is positive, or tr if negative. Can be tl, tr, bl, or br.
#' @param case_names Whether to add case names or not. Defaults to true. Row names are used for case names.
#' @keywords ggplot2, plot, scatter
#' @export
#' @examples
#' GG_scatter()
GG_scatter = function(df, x_var, y_var, text_pos, case_names = T) {
  library(ggplot2)
  library(grid)

  #check if vars exist
  if (!x_var %in% colnames(df)) stop("X variable not found in data.frame!")
  if (!y_var %in% colnames(df)) stop("Y variable not found in data.frame!")

  #subset
  df = na.omit(df[c(x_var, y_var)])

  ## text
  #correlation
  cor = round(cor(df, use = "p")[1, 2], 2) #get correlation, rounded

  #auto detect text position
  if (missing(text_pos)) {
    if (cor>0) text_pos = "tl" else text_pos = "tr"
  }

  #text object location
  if (text_pos == "tl") {
    x = .02
    y = .98
    hjust = 0
    vjust = 1
  }
  if (text_pos == "tr") {
    x = .98
    y = .98
    hjust = 1
    vjust = 1
  }
  if (text_pos == "bl") {
    x = .02
    y = .02
    hjust = 0
    vjust = -.1
  }
  if (text_pos == "br") {
    x = .98
    y = .02
    hjust = 1
    vjust = -.1
  }

  #text
  text = paste0("r=", cor, " (orange line)",
                "\nn=", nrow(df))

  #labels
  df$label = rownames(df)

  #text object
  text_object = grobTree(textGrob(text, x = x,  y = y, hjust = hjust, vjust = vjust),
                         gp = gpar(fontsize = 11))

  g = ggplot(df, aes_string(x_var, y_var)) +
    geom_point() +
    geom_smooth(method = lm, se = F, color = "orange") +
    annotation_custom(text_object)

  #case names?
  if (case_names) {
    g = g + geom_text(aes(label = label), size = 3, vjust = 1)
  }

  return(g)
}


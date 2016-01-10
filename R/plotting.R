#' Histogram with an empirical density curve and a vertical line at the mean
#'
#' Plots a histogram with an empirical density curve and a vertical line at the mean using ggplot2.
#' @param df (data.frame or something coercible into) A data.frame with variables.
#' @param var (character scalar) The name of the variable to use. Not needed if df is a vector.
#' @export
#' @examples
#' GG_denhist(iris, "Sepal.Length")
GG_denhist = function(df, var, binwidth = NULL) {
  library(ggplot2)

  #input type
  if (is_simple_vector(df)) {
    var = deparse(substitute(df))
    df = data.frame(df)
    colnames(df) = var
  }

  g = ggplot(df, aes_string(var)) +
    geom_histogram(aes(y=..density..),  # Histogram with density instead of count on y-axis
                   colour="black", fill="white", binwidth = binwidth) +
    geom_density(alpha=.2, fill="#FF6666") +  # Overlay with transparent density plot
    geom_vline(aes_string(xintercept = mean(df[[var]], na.rm=T)),   # Ignore NA values for mean
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
plot_kmeans = function (df, clusters, runs = 100, standardize = T) {
  library(psych)
  library(ggplot2)

  #class
  df = as.data.frame(df)

  #standardize?
  if (standardize)
    df = std_df(df)

  #analyze
  tmp_k = kmeans(df, centers = clusters, nstart = runs)
  tmp_f = fa(df, 2, rotate = "none")
  tmp_d = data.frame(matrix(ncol = 0, nrow = nrow(df)))
  tmp_d$cluster = as.factor(tmp_k$cluster)
  tmp_d$fact_1 = as.numeric(tmp_f$scores[, 1])
  tmp_d$fact_2 = as.numeric(tmp_f$scores[, 2])
  tmp_d$label = rownames(df)
  g = ggplot(tmp_d, aes(fact_1, fact_2, color = cluster)) +
    geom_point() + geom_text(aes(label = label), size = 3,
                             vjust = 1, color = "black")
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
#' @param CI Confidence interval. Defaults to .95. Set to NULL to disable.
#' @param clean_names (boolean) Whether to clean the axes names using str_clean(). Default=T.
#' @keywords ggplot2, plot, scatter
#' @export
#' @examples
#' GG_scatter()
GG_scatter = function(df, x_var, y_var, text_pos, case_names = T, CI = .95, clean_names = T) {
  library(ggplot2)
  library(grid)
  library(psychometric)
  library(psych)
  library(stringr)

  #check if vars exist
  if (!x_var %in% colnames(df)) stop("X variable not found in data.frame!")
  if (!y_var %in% colnames(df)) stop("Y variable not found in data.frame!")

  #subset + remove NA
  df = na.omit(df[c(x_var, y_var)])

  ## text
  #correlation + CI
  cor = cor(df, use = "p")[1, 2] #get correlation
  cor_CI = CIr(cor, n = count.pairwise(df)[1, 2], level = CI)

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
  if (!is.null(CI)) {
    text = paste0("r=", cor %>% round(2), " [CI", CI*100,": ", cor_CI[1] %>% round(2), " ", cor_CI[2] %>% round(2), "] (orange line)",
                  "\nn=", nrow(df))
  } else {
    text = paste0("r=", cor %>% round(2), " (orange line)",
                  "\nn=", nrow(df))
  }


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

  #clean?
  if (clean_names) {
    g = g + xlab(str_clean(x_var)) + ylab(str_clean(y_var))
  }

  return(g)
}


#' Plot factor loadings.
#'
#' Returns a ggplot2 plot with sorted loadings and numerical results in a corner. Supports reversing of the factor is reversed.
#' @param fa.object a factor analysis object from the fa() function from the psych package.
#' @param reverse whether to reverse all loadings. Default to false.
#' @param text_pos which corner to write the numerical results in. Options are "tl", "tr", "bl", "br". Defaults to "tl".
#' @keywords psychometrics, psychology, latent variable, factor analysis, plot, ggplot2
#' @export
#' @examples
#' plot_loadings()
plot_loadings = function(fa.object, reverse = F, text_pos = "tl") {
  library("plotflow") #needed for reordering the variables
  library("grid") #for grob
  if (reverse) {
    loadings = as.vector(fa.object$loadings)*-1
    reverse = "\nIndicators reversed"
  }
  else {
    loadings = as.vector(fa.object$loadings)
    reverse = ""
  }

  #indicator names
  indicators = dimnames(fa.object$loadings)[[1]]
  DF = data.frame(loadings, indicators)

  #text object location
  if (text_pos=="tl") {
    x = .02
    y = .98
    hjust = 0
    vjust = 1
  }
  if (text_pos=="tr") {
    x = .98
    y = .98
    hjust = 1
    vjust = 1
  }
  if (text_pos=="bl") {
    x = .02
    y = .02
    hjust = 0
    vjust = -.1
  }
  if (text_pos=="br") {
    x = .98
    y = .02
    hjust = 1
    vjust = -.1
  }

  #text
  var.pct = round(mean(fa.object$communality),3) #the proportion of variance accounted for
  text = paste0("proportion of variance explained ",var.pct,reverse)

  #text object
  text_object = grobTree(textGrob(text, x=x,  y=y, hjust = hjust, vjust = vjust),
                         gp=gpar(fontsize=11))

  g = ggplot(reorder_by(indicators, ~ loadings, DF), aes(loadings, indicators)) +
    geom_point() +
    annotation_custom(text_object) +
    xlab("loadings") + ylab("indicators")

  return(g)
}


#' Plot multiple factor loadings in one plot.
#'
#' Returns a ggplot2 plot with sorted loadings colored by the analysis they belong to. Supports reversing óf any factors that are reversed. Dodges to avoid overplotting. Only works for factor analyses with 1 factor solutions!
#' @param fa_objects a list of factor analyses objects from the fa() function from the psych package.
#' @param fa_labels a character vector for names of the analyses. Defaults to fa.1, fa.2, etc..
#' @param reverse.vector a numeric vector to multiple factor loadings with. Use e.g. c(1, -1) to reverse the second factor. Defaults not reversing.
#' @param reorder (character scalar or NA) Which factor analysis to order the loadings by. Can be integers, "all" or NA for not reordering.
#' @keywords psychometrics, psychology, latent variable, factor analysis, plot, ggplot2
#' @export
#' @examples
#' plot_loadings_multi()
plot_loadings_multi = function (fa_objects, fa_labels, reverse_vector = NA, reorder = "all") {
  library("plotflow")
  library("stringr")
  library("ggplot2")
  library("plyr")

  fa_num = length(fa_objects)
  fa_names = str_c("fa.", 1:fa_num)
  if (!is.list(fa_objects)) {
    stop("fa_objects parameter is not a list.")
  }
  if (class(fa_objects) %in% c("psych", "fa")) {
    fa_objects = list(fa_objects)
    fa_num = length(fa_objects)
    fa_names = str_c("fa.", 1:fa_num)
  }
  if (missing("fa_labels")) {
    if (!is.null(names(fa_objects))) {
      fa_labels = names(fa_objects)
    }
    else {
      fa_labels = fa_names
    }
  }
  if (length(fa_labels) != fa_num) {
    stop("Factor analysis labels length is not identical to number of analyses.")
  }
  if (all(is.na(reverse_vector))) {
    reverse_vector = rep(1, fa_num)
  }
  else if (length(reverse_vector) != fa_num) {
    stop("Length of reversing vector does not match number of factor analyses.")
  }
  d = data.frame()
  for (fa.idx in 1:fa_num) {
    loads = fa_objects[[fa.idx]]$loadings * reverse_vector[fa.idx]
    rnames = rownames(loads)
    loads = as.data.frame(as.vector(loads))
    rownames(loads) = rnames
    colnames(loads) = fa_names[fa.idx]
    suppressor({
      d = merge_datasets(d, loads, 1)
    })
  }
  d2 = reshape(d, varying = 1:fa_num, direction = "long", ids = rownames(d))
  d2$time = as.factor(d2$time)
  d2$id = as.factor(d2$id)
  colnames(d2)[2] = "fa"


  #reorder factor?
  if (!is.na(reorder)) {
    if (reorder == "all") {
      suppressor({
        d2 = reorder_by(id, ~fa, d2)
      })
    } else if (reorder == "mean") {
      browser()
      v_aggregate_values = daply(d2, .(id), function(x) {
        mean(x$fa)
      })

    } else if (reorder == "median") {
      v_aggregate_values = daply(d2, .(id), function(x) {
        median(x$fa)
      })

    } else {
      d2_sub = d2[d2$time == reorder, ] #subset the analysis whose loading is to be used for the reorder
      suppressor({
        d2_sub = reorder_by(id, ~fa, d2_sub)
      })

      library(gdata)
      d2$id = reorder.factor(d2$id, new.order = levels(d2_sub$id))
    }
  }

  #plot
  g = ggplot(d2, aes(x = id, y = fa, color = time, group = time)) +
    geom_point(position = position_dodge(width = 0.5)) +
    ylab("Loading") + xlab("Indicator") + scale_color_discrete(name = "Analysis",
                                                               labels = fa_labels) + coord_flip()
  return(g)
}



#' ggplot2 with group means and error bars.
#'
#' Draws a nice ggplot2 with group means and error bars.
#' @param df (data.frame) A data.frame with variables.
#' @param var (character scalar) The name of the variable to plot.
#' @param groupvar (character scaler) The name of the grouping variable.
#' @param CI (numeric scalar) The confidence interval to use. Default = .95.
#' @param type (character scalar) The type of plot. Options: bar, point, points. Default = bar.
#' @keywords ggplot2, plot, group, means, confidence interval, error bars
#' @export
#' @examples
#' GG_group_means()
GG_group_means = function(df, var, groupvar, CI = .95, type = "bar") {
  library(psych)
  library(stringr)
  library(ggplot2)

  #checks
  df = as.data.frame(df)
  if (any(is.na(df[[groupvar]])) ) stop("There must not be missing values in the group variable!")
  if (!var %in% colnames(df)) stop("Variable isn't in the data.frame!")
  if (!groupvar %in% colnames(df)) stop("Group variable isn't in the data.frame!")

  #summarize
  df_sum = describeBy(df[[var]], df[[groupvar]], mat = T)

  #calculate CIs
  df_sum$ci_bar = apply(df_sum, 1, function(x) {
    qt(1 - ((1 - CI) / 2), df = x[4] %>% as.numeric)
  })

  #plot
  if (type == "bar") {
    g = ggplot(df_sum, aes(group1, mean)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
  }

  if (type == "point") {
    g = ggplot(df_sum, aes(group1, mean)) + geom_point() + geom_errorbar(aes(ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
  }

  if (type == "points") {
    g = ggplot(df, aes_string(groupvar, var)) + geom_point() + geom_point(data = df_sum, aes(group1, mean), color = "red", size = 3) + geom_errorbar(data = df_sum, aes(group1, mean, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
  }

  return(g)
}


#' Jensen method (method of correlated vectors) plot
#'
#' Returns a ggplot2 scatter plot with numerical results in a corner. Also supports reversing for dealing with factors that have negative indicators.
#' @param loadings a vector of factor loadings.
#' @param loadings a vector of correlations of the indicators with the criteria variable.
#' @param reverse whether to reverse indicators with negative loadings. Default to true.
#' @param text_pos which corner to write the numerical results in. Options are "tl", "tr", "bl", "br". Defaults to "tl".
#' @keywords psychometrics psychology latent variable
#' @export
#' @examples
#' Jensen_plot()
Jensen_plot = function(loadings, cors, reverse = TRUE, text_pos, var_names = TRUE){
  #libs
  library(ggplot2)
  library(grid)

  #initial
  temp_loadings = as.numeric(loadings) #conver to vector
  names(temp_loadings) = rownames(loadings) #set names again
  loadings = temp_loadings #back to normal name
  DF = data.frame(loadings, cors) #DF

  #reverse
  if (reverse) {
    for (idx in 1:nrow(DF)) {
      if (DF[idx, 1] < 0){ #if loading <0
        DF[idx, ] = DF[idx, ] * -1 #reverse
        rownames(DF)[idx] = paste0(rownames(DF)[idx], "_r")
      }
    }
  }

  #method text
  if (reverse) {method_text = "Jensen's method with reversing\n"}
  else {method_text = "Jensen's method without reversing\n"}

  #correlation
  cor = round(cor(DF)[1, 2], 2) #get correlation, rounded

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
  text = paste0(method_text,
                "r=", cor, " (orange line)",
                "\nn=", nrow(DF))

  #text object
  text_object = grobTree(textGrob(text, x = x,  y = y, hjust = hjust, vjust = vjust),
                         gp = gpar(fontsize = 11))

  #regression line
  model = lm(cors ~ loadings, DF)
  coefs = coef(model)

  #plot
  DF$rnames = rownames(DF)

  g = ggplot(data = DF, aes(x = loadings, y = cors)) +
    geom_point() +
    xlab("Loadings") +
    ylab("Correlation with criteria variable") +
    annotation_custom(text_object) +
    geom_abline(intercept = coefs[1], slope = coefs[2], color = "darkorange")

  #add var_names if desired
  if (var_names) g = g + geom_text(aes(label = rnames), alpha = .7, size = 3, vjust = 1)

  return(g)
}


#' Scatter plot of Jensens method.
#'
#' Takes a factor analysis, data.frame and name of the criteria variable as inputs and returns a ggplot2 scatter plot with Jensen's method applied.
#' @param fa A factor analysis object from fa().
#' @param df A data.frame that contains all the variables.
#' @param criteria A character string of the name of the criteria variable.
#' @param reverse_factor Whether to reverse the factor first.
#' @param loading_reversing Whether to use loading reversing to avoid inflated results. Defaults to TRUE.
#' @param text_pos Which corner to put the text in. Defaults to "tl". Other options: tr, bl, br.
#' @keywords factor analysis, Jensen, method of correlated vectors
#' @export
#' @examples
#' Jensens_method()
Jensens_method = function(fa, df, criteria, reverse_factor = F, loading_reversing = T, text_pos) {
  #get loadings
  fa_loadings = as.numeric(fa$loadings)

  #reverse factor is desired
  if (reverse_factor) fa_loadings = fa_loadings * -1

  #get indicator names
  indicator_names = rownames(fa$loadings)
  indicator_num = length(indicator_names)

  #make new df
  df2 = df[c(indicator_names, criteria)]

  #correlate
  df2_cors = cor(df2, use = "p")

  #criteria x indicator cor vector
  criteria_indi_cor = df2_cors[1:indicator_num, (indicator_num+1)]

  #call plotter
  g = Jensen_plot(fa_loadings, cors = criteria_indi_cor, reverse = loading_reversing, text_pos = text_pos)

  #return ggplot object
  return(g)
}


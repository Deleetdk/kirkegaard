#' Histogram with an empirical density curve and a vertical line at the mean
#'
#' Plots a histogram with an empirical density curve and a vertical line at the mean using ggplot2.
#' @param data (data.frame or something coercible into) A data.frame with variables.
#' @param var (chr sclr) The name of the variable to use. Not needed if data is a vector. Not needed if data has 1 column.
#' @param vline (chr sclr) Whether to plot a vertical line at some point. Can be "mean" or "median". Set to NULL for none. Default="mean". Can also be a custom function as long as it takes an na.rm=T parameter.
#' @param binwidth (num sclr) The width of the bins to use for the histogram. Default=NULL, which means that stat_bin() chooses one.
#' @param group (chr sclr) The name of the grouping variable to use.
#' @export
#' @examples
#' GG_denhist(iris, "Sepal.Length") #plot overall distribution
#' GG_denhist(iris, "Sepal.Length", group = "Species") #plot by group
#' #also accepts vectors
#' GG_denhist(iris[[1]])
#' #also accepts 1-column data.frames, but throws a warning
#' GG_denhist(iris[1])
GG_denhist = function(data, var, vline = "mean", binwidth = NULL, group) {
  library(ggplot2)

  #input type
  if (is_simple_vector(data)) {
    var = deparse(substitute(data))
    data = data.frame(data)
    colnames(data) = var
  }

  df = data; rm(data)

  #1 column df
  if (is.data.frame(df) & ncol(df) == 1 & missing("var")) {
    var = names(df)
    warning("received a data.frame but no var: used the only available column")
  }

  #check if var is in df
  if (!var %in% colnames(df)) stop("Variable " + var + " not found in the data.frame!")

  #remove NA group
  if (!missing("group")) {
    #any miss?
    if (anyNA(df[[group]])) {
          df = df[!is.na(df[[group]]), ]
    warning("Grouping variable contained missing values. These were removed. If you want an NA group, convert to explicit value.")
    }
  }

  #plot
  if (missing("group")) {
    g = ggplot(df, aes_string(var)) +
      geom_histogram(aes(y=..density..),  # Histogram with density instead of count on y-axis
                     colour="black", fill="white", binwidth = binwidth) +
      geom_density(alpha=.2, fill="#FF6666") # Overlay with transparent density plot
  } else {

    g = ggplot(df, aes_string(var, fill = group)) +
      geom_histogram(aes(y=..density..),  # Histogram with density instead of count on y-axis
                     colour="black", binwidth = binwidth, position = "dodge") +
      geom_density(alpha=.2) # Overlay with transparent density plot
  }


  #vline
  if (!is.null(vline) & missing("group")) {
    #calculate central tendency using given function
    central_tendency = do.call(what = vline, args = list(x = df[[var]], na.rm = T))

    #add it
    g = g + geom_vline(xintercept = central_tendency,
                       color="red",
                       linetype="dashed", size=1)
  }

  if (!is.null(vline) & !missing("group")) {
    #calculate central tendencies using given function
    library(plyr)

    #fetch the actual function
    func = get(vline)
    central_tendency = daply(df, .variables = group, .fun = function(block) {
      func(block[[var]], na.rm=T)
    })

    #get the colors
    #http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }

    colors = gg_color_hue(length(unique(df[[group]])))

    #add it
    g = g + geom_vline(xintercept = central_tendency, linetype="dashed", size=1, color = colors)
  }

  return(g)
}

#' Scatter plot with kmeans clustering
#'
#' Performs kmeans and factor analysis on a dataset. Then plots a scatter plot using the first two factors (orthogonal) and color codes cases by their kmeans cluster.
#' @param df A data.frame with variables.
#' @param clusters The number of clusters to find.
#' @param runs Number of runs to use. The best run is used in the plot.
#' @param standardize Whether to standardize the data first. Defaults to TRUE.
#' @export GG_kmeans plot_kmeans
#' @aliases plot_kmeans
#' @examples
#' GG_kmeans(iris[-5], 3)
GG_kmeans = function (df, clusters, runs = 100, standardize = T) {
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

#old name
plot_kmeans = GG_kmeans


#' Scatter plot with regression line and correlation information using ggplot2
#'
#' Plots a scatterplot with a regression line and correlation information. Returns a ggplot2 object.
#' @param df (data.frame) A data.frame with variables.
#' @param x_var (chr scalar) X variable string.
#' @param y_var (chr scalar) Y variable string.
#' @param weights (num scalar) A set of weights to use.
#' @param text_pos (chr scalar) Where to put the text. Defaults to top right ("tl") if correlation is positive, or tr if negative. Can be tl, tr, bl, or br.
#' @param case_names (log scalar) Whether to add case names or not (default true).
#' @param case_names_vector (chr vector) The case names to use. If missing, uses row names.
#' @param CI (num scalar) interval. Defaults to .95. Set to NULL to disable.
#' @param clean_names (log scalar) Whether to clean the axes names using str_clean(). Default=T.
#' @param check_overlap (log scalar) Whether to avoid overplotting names. Default=T.
#' @export
#' @examples
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width") #default plot
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names_vector = rep("A", 150)) #other case names
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", text_pos = "br") #other text location
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", CI = .99) #other CI
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", clean_names = F) #don't clean names
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", weights = 1:150) #add weights
GG_scatter = function(df, x_var, y_var, weights, text_pos, case_names = T, case_names_vector, CI = .95, clean_names = T, check_overlap = T) {
  library("ggplot2")
  library("grid")
  library("psychometric")
  library("psych")
  library("stringr")
  library("weights")

  #check if vars exist
  if (!x_var %in% colnames(df)) stop("X variable not found in data.frame!")
  if (!y_var %in% colnames(df)) stop("Y variable not found in data.frame!")

  #case names?
  if (!missing(case_names_vector)) {
    if (!lengths_match(df, case_names_vector)) stop("Vector of case names is of the wrong length!")
    df$.label = case_names_vector #use supplied names
  } else {
    df$.label = rownames(df) #use rownames
  }

  #weights
  if (missing(weights)) {
    df$.weights = rep(1, nrow(df)) #fill with 1's
  } else {
    df$.weights = weights
  }

  #subset + remove NA
  df = na.omit(df[c(x_var, y_var, ".weights", ".label")])

  ## text
  #correlation + CI
  cor = wtd.cors(df[1:2], weight = df$.weights)[1, 2] #get correlation
  cor_CI = CIr(cor, n = count.pairwise(df)[1, 2], level = CI)

  #auto detect text position
  if (missing(text_pos)) {
    if (cor>0) text_pos = "tl" else text_pos = "tr"
  }

  #validate text_pos
  check_if_in(text_pos, c("tl", "tr", "bl", "br"))

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
    text = paste0("r=", cor %>% format(nsmall = 2, digits = 2), " [CI", CI*100,": ", cor_CI[1] %>% format(nsmall = 2, digits = 2), " ", cor_CI[2] %>% format(nsmall = 2, digits = 2), "] (orange line)",
                  "\nn=", nrow(df))
  } else {
    text = paste0("r=", cor %>% format(nsmall = 2, digits = 2), " (orange line)",
                  "\nn=", nrow(df))
  }


  #text object
  text_object = grobTree(textGrob(text, x = x,  y = y, hjust = hjust, vjust = vjust),
                         gp = gpar(fontsize = 11))

  #plot
  if (missing(weights)) {
    g = ggplot(df, aes_string(x_var, y_var)) +
      geom_point()
  } else {
    g = ggplot(df, aes_string(x_var, y_var, weight = ".weights")) +
      geom_point(aes(size = .weights)) +
      scale_size_continuous(guide = F)
  }

  #add the rest
  g = g + geom_smooth(method = lm, se = F, color = "orange") +
    annotation_custom(text_object)

  #case names?
  if (missing(weights)) {y_nudge = 1.25} else {y_nudge = 2}
  if (case_names) {
    g = g + geom_text(aes(label = .label), size = 3, vjust = y_nudge, check_overlap = check_overlap)
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
#' @export
#' @examples
#' library(psych)
#' plot_loadings(fa(iris[-5])) #plot loadings from analysis of iris data
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
#'
#' Non-overlapping indicates are put in the bottom.
#' @param fa_objects (list of fa-class objects) Factor analyses objects from the fa() function from the \code{\link{psych}} package.
#' @param fa_labels (chr vector) Names of the analyses. Defaults to fa.1, fa.2, etc..
#' @param reverse_vector (num vector) Vector of numbers to use for reversing factors. Use e.g. c(1, -1) to reverse the second factor. Defaults not reversing.
#' @param reorder (chr scalar or NA) Which factor analysis to order the loadings by. Can be integers that reprensent each factor analysis. Can also be "mean", "median" to use the means and medians of the loadings. Use "all" for the old method. Default = "mean".
#' @export
#' @examples
#' library(psych)
#' plot_loadings_multi(fa(iris[-5])) #extract a factor and plot
#' #list of FAs
#' fa_list2 = list(part1 = fa(iris[1:50, -c(1, 5)]),
#'                 part2 = fa(iris[51:100, -c(2, 5)]),
#'                 part3 = fa(iris[101:150, -c(3, 5)]))
#' #notice that it handles non-overlapping indicators
#' plot_loadings_multi(fa_list2)
#' #reorder by a particular FA
#' plot_loadings_multi(fa_list2, reorder = 1)
plot_loadings_multi = function (fa_objects, fa_labels, reverse_vector = NA, reorder = "mean") {
  library("stringr")
  library("ggplot2")
  library("plyr")


  #how many?
  fa_num = length(fa_objects)
  fa_names = str_c("fa.", 1:fa_num)

  #is fa_objects a single fa?
  if (all(class(fa_objects) %in% c("psych", "fa"))) {
    fa_objects = list(fa_objects)
    fa_num = length(fa_objects)
    fa_names = str_c("fa.", 1:fa_num)
  } else {
    is_(fa_objects, class = "list", error_on_false = T)
  }

  #labels to use
  if (missing("fa_labels")) {
    if (!is.null(names(fa_objects))) {
      fa_labels = names(fa_objects)
    }
    else {
      fa_labels = fa_names
    }
  }

  #if given labels, check their length
  if (length(fa_labels) != fa_num) {
    stop("Factor analysis labels length is not identical to number of analyses.")
  }

  #check reverse_vector
  if (all(is.na(reverse_vector))) {
    reverse_vector = rep(1, fa_num)
  } else if (length(reverse_vector) != fa_num) {
    stop("Length of reversing vector does not match number of factor analyses.")
  }

  #extract data
  d = data.frame()
  for (fa.idx in 1:fa_num) {
    loads = fa_objects[[fa.idx]]$loadings * reverse_vector[fa.idx]
    rnames = rownames(loads)
    loads = as.data.frame(as.vector(loads))
    rownames(loads) = rnames
    colnames(loads) = fa_names[fa.idx]
    silence({
      d = merge_datasets(d, loads, 1)
    })
  }

  #reshape data to long form
  d2 = reshape(d, varying = 1:fa_num, direction = "long", ids = rownames(d))
  d2$time = as.factor(d2$time)
  d2$id = as.factor(d2$id)
  colnames(d2)[2] = "fa"

  #reorder factor?
  if (!is.na(reorder)) {
    if (reorder == "all") {
      message("reorder = all is depreciated. You probably want to use reorder = mean")
      library("plotflow")

      silence({
        d2 = reorder_by(id, ~fa, d2)
      })
    } else if (reorder == "mean") {
      v_aggregate_values = daply(d2, .(id), function(x) {
        mean(x$fa, na.rm=T)
      })

      #re-level
      d2$id = factor(d2$id, levels = names(sort(v_aggregate_values, decreasing = F)))

    } else if (reorder == "median") {
      v_aggregate_values = daply(d2, .(id), function(x) {
        median(x$fa, na.rm=T)
      })

      #re-level
      d2$id = factor(d2$id, levels = names(sort(v_aggregate_values, decreasing = F)))

    } else {
      # browser()
      d2_sub = d2[d2$time == reorder, ] #subset the analysis whose loading is to be used for the reorder

      #get vector of the chosen analysis
      v_values = d2_sub$fa; names(v_values) = d2_sub$id

      #re-level
      d2$id = factor(d2$id, levels = names(sort(v_values, decreasing = F, na.last = F)))
    }
  }

  #plot
  if (fa_num > 1) {
    g = ggplot(d2, aes(x = id, y = fa, color = time, group = time)) +
      geom_point(position = position_dodge(width = 0.5)) +
      ylab("Loading") + xlab("Indicator") +
      scale_color_discrete(name = "Analysis", labels = fa_labels) +
      coord_flip()
  } else {
    g = ggplot(d2, aes(x = id, y = fa)) +
      geom_point(position = position_dodge(width = 0.5)) +
      ylab("Loading") +
      xlab("Indicator") +
      coord_flip()
  }

  return(g)
}



#' ggplot2 with group means and error bars.
#'
#' Draws a nice ggplot2 with group means and error bars.
#' @param df (data.frame) A data.frame with variables.
#' @param var (chr scalar) The name of the variable to plot.
#' @param groupvar (chr scaler) The name of the grouping variable.
#' @param subgroupvar (chr scalar) The name of the subgrouping variable, if any.
#' @param CI (num scalar) The confidence interval to use. Default = .95.
#' @param type (chr scalar) The type of plot. Options: bar (default), point, points.
#' @param msg_NA (log scalar) Show a message if NAs were removed? (default true)
#' @param split_group_labels (log scalar) Whether to automatically insert newlines into group labels if they are too long (default yes).
#' @param line_length (num scalar) The desired line width (default 95). Only used when split_group_labels = T.
#' @export
#' @examples
#' #simple examples
#' GG_group_means(iris, "Sepal.Length", "Species")
#' GG_group_means(iris, "Sepal.Length", "Species", type = "point")
#' GG_group_means(iris, "Sepal.Length", "Species", type = "points")
#' GG_group_means(iris, "Sepal.Length", "Species", type = "points", CI = .999999)
#' GG_group_means(iris, "Sepal.Length", "Species", type = "violin")
#' GG_group_means(iris, "Sepal.Length", "Species", type = "violin2")
#'
#' #subgroups too
#' iris$type = sample(LETTERS[1:3], size = nrow(iris), replace = T)
#' GG_group_means(iris, var = "Sepal.Length", groupvar = "Species", subgroupvar = "type")
#' GG_group_means(iris, var = "Sepal.Length", groupvar = "Species", subgroupvar = "type", type = "point")
#' GG_group_means(iris, var = "Sepal.Length", groupvar = "Species", subgroupvar = "type", type = "points")
#' GG_group_means(iris, var = "Sepal.Length", groupvar = "Species", subgroupvar = "type", type = "violin")
#' GG_group_means(iris, var = "Sepal.Length", groupvar = "Species", subgroupvar = "type", type = "violin2")
GG_group_means = function(df, var, groupvar, subgroupvar, CI = .95, type = "bar", na.rm = T, msg_NA = T, split_group_labels = T, line_length = 95) {
  library(psych)
  library(stringr)
  library(ggplot2)
  library(plyr)

  #convert
  df = as.data.frame(df)

  #no subgroupvar variable, simple
  if(missing("subgroupvar")) {

    #checks
    if (!var %in% colnames(df)) stop("Variable isn't in the data.frame!")
    if (!groupvar %in% colnames(df)) stop("Group variable isn't in the data.frame!")
    if (!type %in% c("bar", "point", "points", "violin", "violin2")) stop("Type not recognized! Supported values: bar, point, points")

    #subset
    df = df[c(var, groupvar)]

    #check for missing
    if (count_NA(df) > 0 ) {
      #remove missing?
      if (na.rm) {
        df = filter_by_missing_values(df, missing = 0)
        silence(message("Missing values were removed."), messages = msg_NA)
      } else {
        stop("There must not be missing values in the group variable when na.rm = F!")
      }
    }

    #summarize
    df_sum = describeBy(df[[var]], df[[groupvar]], mat = T)

    #reorder groups in line with data
    if (is.factor(df[[groupvar]])) { #only do it if the data is a factor, if not, use default order
      df_sum$group1 = factor(df_sum$group1, levels = levels(df[[groupvar]]))
    }

    #calculate CIs
    df_sum$ci_bar = apply(df_sum, 1, function(x) {
      qt(1 - ((1 - CI) / 2), df = as.numeric(x[4]) - 1)
    })

    #plot
    if (type == "bar") {
      g = ggplot(df_sum, aes(group1, mean)) +
        geom_bar(stat="identity") +
        geom_errorbar(aes(ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
    }

    if (type == "point") {
      g = ggplot(df_sum, aes(group1, mean)) +
        geom_point() +
        geom_errorbar(aes(ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
    }

    if (type == "points") {
      g = ggplot(df_sum) + #use summed as the default data, otherwise the code for adding newlines removes the labels
        geom_point(data = df, aes_string(groupvar, var)) +
        geom_point(aes(group1, mean), color = "red", size = 3) +
        geom_errorbar(aes(group1, mean, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
    }

    if (type == "violin") {
      g = ggplot() +
        geom_violin(data = df, aes_string(groupvar, var, fill = groupvar), alpha = .5) +
        scale_fill_discrete(guide = F) +
        geom_point(data = df_sum, aes(group1, mean), color = "red", size = 3) +
        geom_errorbar(data = df_sum, aes(group1, mean, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
    }

    if (type == "violin2") {
      g = ggplot() +
        geom_violin(data = df, aes_string(groupvar, var, fill=groupvar), alpha = .5) +
        geom_count(data = df, aes_string(groupvar, var)) +
        scale_fill_discrete(guide = F) +
        geom_point(data = df_sum, aes(group1, mean), color = "red", size = 3) +
        geom_errorbar(data = df_sum, aes(group1, mean, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
    }

    if (split_group_labels) {
      g = g + scale_x_discrete(labels = levels(g$data$group1) %>% add_newlines(line_length = line_length))
    }

    #labels
    g = g + xlab(groupvar) + ylab(var)
  }

  #if plot by subgroup too
  if(!missing("subgroupvar")) {

    #checks
    if (!var %in% colnames(df)) stop("Variable isn't in the data.frame!")
    if (!groupvar %in% colnames(df)) stop("Group variable isn't in the data.frame!")
    if (!subgroupvar %in% colnames(df)) stop("Color variable isn't in the data.frame!")
    if (!type %in% c("bar", "point", "points", "violin", "violin2")) stop("Type not recognized! Supported values: bar, point, points")

    #subset
    df = df[c(var, groupvar, subgroupvar)]

    #check for missing
    if (count_NA(df) > 0 ) {
      #remove missing?
      if (na.rm) {
        df = filter_by_missing_values(df, missing = 0)
        silence(message("Missing values were removed."), messages = msg_NA)
      } else {
        stop("There must not be missing values in the group variable when na.rm = F!")
      }
    }

    #summarize
    df_sum = ddply(df, .variables = c(groupvar, subgroupvar), .fun = function(d_sub) {
      desc = psych::describe(d_sub[[var]])
      c("mean" = desc$mean,
        "n" = desc$n,
        "se" = desc$se)
    })

    #copy vars
    df_sum$groupvar = df_sum[[groupvar]]
    df_sum$subgroupvar = df_sum[[subgroupvar]]
    df$var = df[[var]]
    df$groupvar = df[[groupvar]]
    df$subgroupvar = df[[subgroupvar]]

    #reorder factors in line with data
    if (is.factor(df[[groupvar]])) { #only do it if the data is a factor, if not, use default order
      df_sum$groupvar = factor(df_sum[[groupvar]], levels = levels(df[[groupvar]]))
    }

    if (is.factor(df[[subgroupvar]])) { #only do it if the data is a factor, if not, use default order
      df_sum$subgroupvar = factor(df_sum[[subgroupvar]], levels = levels(df[[subgroupvar]]))
    }

    #calculate CIs
    df_sum$ci_bar = apply(df_sum, 1, function(x) {
      qt(1 - ((1 - CI) / 2), df = as.numeric(x[4]) - 1)
    })

    #plot
    if (type == "bar") {
      g = ggplot(df_sum, aes(x = groupvar, y = mean, fill = subgroupvar)) +
        geom_bar(stat="identity", position = "dodge") +
        geom_errorbar(aes(ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), position = position_dodge(width = .9), width = .2)
    }

    if (type == "point") {
      g = ggplot(df_sum, aes(groupvar, mean, color = subgroupvar)) +
        geom_point(position = position_dodge(width = .9)) +
        geom_errorbar(aes(ymin = mean - ci_bar*se, ymax = mean + ci_bar*se, group = subgroupvar), position = position_dodge(width = .9), color = "black", width = .2)
    }

    if (type == "points") {
      g = ggplot(df_sum) + #use summed as the default data, otherwise the code for adding newlines removes the labels
        geom_point(data = df, aes(groupvar, y = var, color = subgroupvar), position = position_dodge(width = .9)) +
        geom_point(aes(groupvar, y = mean, group = subgroupvar), color = "black", size = 4, position = position_dodge(width = .9), shape = 5) +
        geom_errorbar(aes(groupvar, y = mean, group = subgroupvar, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), position = position_dodge(width = .9), width = .2)

    }

    if (type == "violin") {
      g = ggplot(df_sum) + #use summed as the default data, otherwise the code for adding newlines removes the labels
        geom_violin(data = df, aes(groupvar, y = var, fill = subgroupvar), position = position_dodge(width = .9)) +
        geom_point(aes(groupvar, y = mean, group = subgroupvar), color = "black", size = 4, position = position_dodge(width = .9), shape = 5) +
        geom_errorbar(aes(groupvar, y = mean, group = subgroupvar, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), position = position_dodge(width = .9), width = .2)
    }

    if (type == "violin2") {
      g = ggplot(df_sum) + #use summed as the default data, otherwise the code for adding newlines removes the labels
        geom_violin(data = df, aes(groupvar, y = var, fill = subgroupvar), position = position_dodge(width = .9), alpha = .5) +
        geom_count(data = df, aes(groupvar, y = var, group = subgroupvar), position = position_dodge(width = .9)) +
        geom_point(aes(groupvar, y = mean, group = subgroupvar), color = "red", size = 4, position = position_dodge(width = .9), shape = 5) +
        geom_errorbar(aes(groupvar, y = mean, group = subgroupvar, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), position = position_dodge(width = .9), width = .2, color = "red")
    }

    if (split_group_labels) {
      g = g + scale_x_discrete(labels = levels(g$data$groupvar) %>% add_newlines(line_length = line_length))
    }


    #labels
    g = g + xlab(groupvar) + scale_color_discrete(name = subgroupvar) + scale_fill_discrete(name = subgroupvar) + ylab(var)
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
#' @export
#' @examples
#' Jensen_plot()
Jensen_plot = function(loadings, cors, reverse = TRUE, text_pos, var_names = TRUE, check_overlap = TRUE){
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
  if (var_names) g = g + geom_text(aes(label = rnames), alpha = .7, size = 3, vjust = 1.5, check_overlap = check_overlap)

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
#' @export
#' @examples
#' Jensens_method()
Jensens_method = function(fa, df, criteria, reverse_factor = F, loading_reversing = T, text_pos, var_names = TRUE, check_overlap = TRUE) {
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
  g = Jensen_plot(fa_loadings, cors = criteria_indi_cor, reverse = loading_reversing, text_pos = text_pos, var_names = var_names, check_overlap = check_overlap)

  #return ggplot object
  return(g)
}



#' Plot a contingency table with ggplot2
#'
#' Makes a pretty contingency table with ggplot2 using geom_tile.
#' @param data (data.frame) The data.
#' @param var1 (chr scalar) The name of the first variable (vertical)
#' @param var2 (chr scalar) The name of the second variable (horizontal)
#' @param margin (NULL, 1, 2) Which margin to use. Default = NULL.
#' @export
#' @examples
#' GG_contingency_table(mpg, "drv", "cyl")
#' GG_contingency_table(mpg, "drv", "cyl", margin = 1)
#' GG_contingency_table(mpg, "drv", "cyl", margin = 2)
GG_contingency_table = function(data, var1, var2, margin = NULL) {
  library(ggplot2); library(magrittr)

  #copy table
  data = data[c(var1, var2)]

  #convert to factors (if they are strings to begin with, will get an error)
  data[] = lapply(data, as.factor)

  #calculate table
  t_table = table(data[[var1]], data[[var2]]) %>% prop.table(margin = margin)

  #make df
  d_table = t_table %>% as.data.frame()

  #plot
  ggplot(d_table, aes(Var2, Var1)) + geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = round(Freq, 2))) +
    scale_fill_continuous(name = "Proportion") +
    ylab(substitute(var1)) + xlab(substitute(var2))
}

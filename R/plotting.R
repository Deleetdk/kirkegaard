#' Histogram with an empirical density curve
#'
#' Plots a histogram with an empirical density curve and a vertical line at desired central tendency measure.
#'
#' Automatically rescales the histogram and density fit so the heights match. Applies \code{theme_bw}.
#' @param data (data.frame or something coercible into) A data frame or a vector.
#' @param var (chr sclr) The name of the variable to use. Not needed if data is a vector. Not needed if data has 1 column.
#' @param group (chr sclr) The name of the grouping variable to use.
#' @param vline (chr sclr) Whether and how to plot vertical line(s) at some point(s. Set to NULL for none. Default is \code{mean}. Can also be a custom function. Beware, it should ignore values.
#' @param clean_name (lgl) Wheter to call str_clean on the x axis label.
#' @param binwidth (num sclr) The width of the bins to use for the histogram. Default=NULL, which means that stat_bin() chooses one.
#' @export
#' @return A ggplot2 object.
#' @examples
#' GG_denhist(iris, "Sepal.Length") #plot overall distribution
#' GG_denhist(iris, "Sepal.Length", vline = median) #use another central tendency
#' GG_denhist(iris, "Sepal.Length", group = "Species") #plot by group
#' #also accepts vectors
#' iris$Sepal.Length %>% GG_denhist
#' #also accepts 1-column data.frames, but throws a warning
#' GG_denhist(iris[1])
#' #warns you if some groups have no data
#' data.frame(x = c(1, 2, NA), y = c(1, 2, 3)) %>% GG_denhist("x", "y")
#' #warns you if grouping variable has missing data
#' data.frame(x = c(1, 2, 3), y = c(1, 2, NA)) %>% GG_denhist("x", "y")
GG_denhist = function(data, var = NULL, group = NULL, vline = mean, binwidth = NULL, clean_name = T) {

  #check input
  data

  #vline input
  if (!is.null(vline)) {
    if (is.logical(vline)) {
      if (!vline) {
        vline = NULL
      } else {
        stop("vline cannot be TRUE! Use NULL or FALSE for not drawing a line.")
      }
    }
  }

  #data input type
  if (is_simple_vector(data)) {
    var = deparse(substitute(data))
    data = data.frame(data)
    colnames(data) = var
  }

  #1 column df
  if (is.data.frame(data) && ncol(data) == 1 && is.null(var)) {
    var = names(data)
    warning("received a data frame but no var: used the only available column")
  }

  #rename and drop unused
  df = data[c(var, group)]
  rm(data)

  #check if var is in df
  if (!var %in% colnames(df)) stop("Variable " + var + " not found in the data frame!")

  #remove NA group
  if (!is.null(group)) {
    #convert to factor
    df[[group]] %<>% as.factor

    #any miss in grouping variable?
    if (anyNA(df[[group]])) {
          df = df[!is.na(df[[group]]), ]
          warning("Grouping variable contained missing values. These were removed. If you want an NA group, convert to explicit value.")
    }

    #groups without any data?
    if (df[c(var, group)] %>% anyNA) {
      warning("There were groups without any data. These were removed")
      #also drop the other variables
      df = df[!is.na(df[[var]]) & !is.na(df[[group]]), ]

      #drop unused levels to prevent error in plyr::daply later
      df[[group]] %<>% fct_drop()
    }

  }

  #plot
  if (is.null(group)) {
    g = ggplot2::ggplot(df, aes_string(var)) +
      geom_histogram(aes(y=..density..),  # Histogram with density instead of count on y-axis
                     colour="black", fill="white", binwidth = binwidth) +
      geom_density(alpha=.2, fill="#FF6666") # Overlay with transparent density plot
  } else {

    g = ggplot2::ggplot(df, aes_string(var, fill = group)) +
      geom_histogram(aes(y=..density..),  # Histogram with density instead of count on y-axis
                     colour="black", binwidth = binwidth, position = "dodge") +
      geom_density(alpha=.2) # Overlay with transparent density plot
  }


  #vline?
  if (!is.null(vline)) {
    #if chr func, get the function
    if (is.character(vline)) vline = get(vline)

    #build function
    tryCatch({
      vline_func = purrr::partial(vline, na.rm=T)
      #test it
      vline_func(1)
    },
    error = function(e){
      #just use the supplied one
      vline_func = vline
    })

    #no groups
    if (is.null(group)) {
      #add it
      g = g + geom_vline(xintercept = vline_func(df[[var]]),
                         color="red",
                         linetype="dashed", size=1)
    }

    #groups
    if (!is.null(group)) {
      #calculate central tendencies using given function
      central_tendency = plyr::daply(df, .variables = group, .fun = function(block) {
        vline_func(block[[var]])
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
  }

  #clean name?
  if (clean_name) g = g + scale_x_continuous(name = str_clean(var))

  return(g + ggplot2::theme_bw())
}

#' Scatter plot with kmeans clustering
#'
#' Performs kmeans and factor analysis on a dataset. Then plots a scatter plot using the first two factors (orthogonal) and color codes cases by their kmeans cluster.
#' @param df A data.frame with variables.
#' @param clusters The number of clusters to find.
#' @param runs Number of runs to use. The best run is used in the plot.
#' @param standardize Whether to standardize the data first. Defaults to TRUE.
#' @export
#' @examples
#' GG_kmeans(iris[-5], 3)
GG_kmeans = function (df, clusters, runs = 100, standardize = T) {

  #class
  df = as.data.frame(df)

  #standardize?
  if (standardize)
    df = df_standardize(df)

  #analyze
  tmp_k = kmeans(df, centers = clusters, nstart = runs)
  tmp_f = fa(df, 2, rotate = "none")
  tmp_d = data.frame(matrix(ncol = 0, nrow = nrow(df)))
  tmp_d$cluster = as.factor(tmp_k$cluster)
  tmp_d$fact_1 = as.numeric(tmp_f$scores[, 1])
  tmp_d$fact_2 = as.numeric(tmp_f$scores[, 2])
  tmp_d$label = rownames(df)
  g = ggplot2::ggplot(tmp_d, aes(fact_1, fact_2, color = cluster)) +
    geom_point() + geom_text(aes(label = label), size = 3,
                             vjust = 1, color = "black")
  return(g + ggplot2::theme_bw())
}


#' Scatter plot with regression line and correlation information using ggplot2
#'
#' Plots a scatterplot with a regression line and correlation information. Returns a ggplot2 object.
#' @param df (data.frame) A data.frame with variables.
#' @param x_var (chr scalar) X variable string.
#' @param y_var (chr scalar) Y variable string.
#' @param weights (num scalar) A set of weights to use.
#' @param text_pos (chr scalar) Where to put the text. Defaults to top right ("tl") if correlation is positive, or tr if negative. Can be tl, tr, bl, or br.
#' @param case_names (lgl scalar) Whether to add case names or not (default true).
#' @param case_names_vector (chr vector) The case names to use. If NA, uses rownames. If length one, is taken to be a variable in the data.
#' @param CI (num scalar) interval. Defaults to .95. Set to NULL to disable.
#' @param clean_names (lgl scalar) Whether to clean the axes names using str_clean(). Default=T.
#' @param check_overlap (lgl scalar) Whether to avoid overplotting names. Default=T.
#' @export
#' @examples
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width") #default plot
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names_vector = rep("A", 150)) #other case names
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names_vector = "Species") #casenames from variable
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", text_pos = "br") #other text location
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", CI = .99) #other CI
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", clean_names = F) #don't clean names
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", weights = 1:150) #add weights
GG_scatter = function(df, x_var, y_var, weights = NULL, text_pos = NA, case_names = T, case_names_vector = NA, CI = .95, clean_names = T, check_overlap = T) {

  #check if vars exist
  if (!x_var %in% colnames(df)) stop("X variable not found in data.frame!")
  if (!y_var %in% colnames(df)) stop("Y variable not found in data.frame!")

  #case names?
  #default to rownames
  df$.label = rownames(df)
  if (case_names) {

    #any given names?
    if (!is_scalar_NA(case_names_vector)) {
      #is it length 1?
      if (is_scalar(case_names_vector)) {
        #is it there?
        if (!case_names_vector %in% names(df)) stop(sprintf("Variable %s wasn't in the data.frame!", case_names_vector))
        df$.label = df[[case_names_vector]]
      } else {
        #if not length 1, does it match the data length?
        if (!(lengths_match(df, case_names_vector))) stop("Vector of case names is of the wrong length!")

          #use supplied names
          df$.label = case_names_vector
        }
      }
  }


  #weights
  if (is.null(weights)) {
    df$.weights = rep(1, nrow(df)) #fill with 1's
  } else {
    df$.weights = weights
  }

  #subset + remove NA
  df = na.omit(df[c(x_var, y_var, ".weights", ".label")])

  ## text
  #correlation + CI
  cor = weights::wtd.cors(df[1:2], weight = df$.weights)[1, 2] #get correlation
  cor_CI = psychometric::CIr(cor, n = psych::count.pairwise(df)[1, 2], level = CI)

  #auto detect text position
  if (is.na(text_pos)) {
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
  text_object = grid::grobTree(grid::textGrob(text, x = x,  y = y, hjust = hjust, vjust = vjust),
                         gp = grid::gpar(fontsize = 11))

  #plot
  if (is.null(weights)) {
    g = ggplot2::ggplot(df, aes_string(x_var, y_var)) +
      geom_point()
  } else {
    g = ggplot2::ggplot(df, aes_string(x_var, y_var, weight = ".weights")) +
      geom_point(aes(size = .weights)) +
      scale_size_continuous(guide = F)
  }

  #add the rest
  g = g + geom_smooth(method = lm, se = F, color = "orange") +
    annotation_custom(text_object)

  #case names?
  if (is.null(weights)) {y_nudge = 1.25} else {y_nudge = 2}
  if (case_names) {
    g = g + geom_text(aes(label = .label), size = 3, vjust = y_nudge, check_overlap = check_overlap)
  }

  #clean?
  if (clean_names) {
    g = g + xlab(str_clean(x_var)) + ylab(str_clean(y_var))
  }

  return(g + ggplot2::theme_bw())
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
GG_group_means = function(df, var, groupvar = NULL, subgroupvar = NULL, CI = .95, type = "bar", na.rm = T, msg_NA = T, split_group_labels = T, line_length = 95) {

  #convert
  df = as.data.frame(df)

  #no subgroupvar variable, simple
  if (is.null(subgroupvar)) {

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
        df = miss_filter(df, missing = 0)
        silence(message("Missing values were removed."), messages = msg_NA)
      } else {
        stop("There must not be missing values in the group variable when na.rm = F!")
      }
    }

    #check for no data
    if (nrow(df) == 0) stop("No overlapping non-missing data.")

    #summarize
    df_sum = psych::describeBy(df[[var]], df[[groupvar]], mat = T)

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
      g = ggplot2::ggplot(df_sum, aes(group1, mean)) +
        geom_bar(stat="identity") +
        geom_errorbar(aes(ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
    }

    if (type == "point") {
      g = ggplot2::ggplot(df_sum, aes(group1, mean)) +
        geom_point() +
        geom_errorbar(aes(ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
    }

    if (type == "points") {
      g = ggplot2::ggplot(df_sum) + #use summed as the default data, otherwise the code for adding newlines removes the labels
        geom_point(data = df, aes_string(groupvar, var)) +
        geom_point(aes(group1, mean), color = "red", size = 3) +
        geom_errorbar(aes(group1, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
    }

    if (type == "violin") {
      g = ggplot2::ggplot(df_sum) +
        geom_violin(data = df, aes_string(groupvar, var, fill = groupvar), alpha = .5) +
        scale_fill_discrete(guide = F) +
        geom_point(data = df_sum, aes(group1, mean), color = "red", size = 3) +
        geom_errorbar(data = df_sum, aes(group1, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
    }

    if (type == "violin2") {
      g = ggplot2::ggplot(df_sum) +
        geom_violin(data = df, aes_string(groupvar, var, fill=groupvar), alpha = .5) +
        geom_count(data = df, aes_string(groupvar, var)) +
        scale_fill_discrete(guide = F) +
        geom_point(data = df_sum, aes(group1, mean), color = "red", size = 3) +
        geom_errorbar(data = df_sum, aes(group1, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), width = .2, color = "red")
    }

    if (split_group_labels) {
      g = g + scale_x_discrete(labels = levels(g$data$group1) %>% add_newlines(line_length = line_length))
    }

    #labels
    g = g + xlab(groupvar) + ylab(var)
  }

  #if plot by subgroup too
  if (!is.null(subgroupvar)) {

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
        df = miss_filter(df, missing = 0)
        silence(message("Missing values were removed."), messages = msg_NA)
      } else {
        stop("There must not be missing values in the group variable when na.rm = F!")
      }
    }

    #check for no data
    if (nrow(df) == 0) stop("No overlapping non-missing data.")

    #summarize
    df_sum = plyr::ddply(df, .variables = c(groupvar, subgroupvar), .fun = function(d_sub) {
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
      g = ggplot2::ggplot(df_sum, aes(x = groupvar, y = mean, fill = subgroupvar)) +
        geom_bar(stat="identity", position = "dodge") +
        geom_errorbar(aes(ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), position = position_dodge(width = .9), width = .2)
    }

    if (type == "point") {
      g = ggplot2::ggplot(df_sum, aes(groupvar, mean, color = subgroupvar)) +
        geom_point(position = position_dodge(width = .9)) +
        geom_errorbar(aes(ymin = mean - ci_bar*se, ymax = mean + ci_bar*se, group = subgroupvar), position = position_dodge(width = .9), color = "black", width = .2)
    }

    if (type == "points") {
      g = ggplot2::ggplot(df_sum) + #use summed as the default data, otherwise the code for adding newlines removes the labels
        geom_point(data = df, aes(groupvar, y = var, color = subgroupvar), position = position_dodge(width = .9)) +
        geom_point(aes(groupvar, y = mean, group = subgroupvar), color = "black", size = 4, position = position_dodge(width = .9), shape = 5) +
        geom_errorbar(aes(groupvar, group = subgroupvar, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), position = position_dodge(width = .9), width = .2)

    }

    if (type == "violin") {
      g = ggplot2::ggplot(df_sum) + #use summed as the default data, otherwise the code for adding newlines removes the labels
        geom_violin(data = df, aes(groupvar, y = var, fill = subgroupvar), position = position_dodge(width = .9)) +
        geom_point(aes(groupvar, y = mean, group = subgroupvar), color = "black", size = 4, position = position_dodge(width = .9), shape = 5) +
        geom_errorbar(aes(groupvar, group = subgroupvar, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), position = position_dodge(width = .9), width = .2)
    }

    if (type == "violin2") {
      g = ggplot2::ggplot(df_sum) + #use summed as the default data, otherwise the code for adding newlines removes the labels
        geom_violin(data = df, aes(groupvar, y = var, fill = subgroupvar), position = position_dodge(width = .9), alpha = .5) +
        geom_count(data = df, aes(groupvar, y = var, group = subgroupvar), position = position_dodge(width = .9)) +
        geom_point(aes(groupvar, y = mean, group = subgroupvar), color = "red", size = 4, position = position_dodge(width = .9), shape = 5) +
        geom_errorbar(aes(groupvar, group = subgroupvar, ymin = mean - ci_bar*se, ymax = mean + ci_bar*se), position = position_dodge(width = .9), width = .2, color = "red")
    }

    if (split_group_labels) {
      g = g + scale_x_discrete(labels = levels(g$data$groupvar) %>% add_newlines(line_length = line_length))
    }


    #labels
    g = g + xlab(groupvar) + scale_color_discrete(name = subgroupvar) + scale_fill_discrete(name = subgroupvar) + ylab(var)
  }


  return(g + ggplot2::theme_bw())
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

  #copy table
  data = data[c(var1, var2)]

  #convert to factors (if they are strings to begin with, will get an error)
  data[] = lapply(data, as.factor)

  #calculate table
  t_table = table(data[[var1]], data[[var2]]) %>% prop.table(margin = margin)

  #make df
  d_table = t_table %>% as.data.frame()

  #plot
  ggplot2::ggplot(d_table, aes(Var2, Var1)) + geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = round(Freq, 2))) +
    scale_fill_continuous(name = "Proportion") +
    ylab(substitute(var1)) + xlab(substitute(var2)) +
    ggplot2::theme_bw()
}



#' Plot a contingency table with ggplot2
#'
#' Makes a pretty contingency table with ggplot2 using geom_tile.
#' @param .analysis (rma object) The rma analysis from metafor.
#' @param .names (chr vector) An optional vector of names to use.
#' @param .alphabetic_sort_names (lgl sclar) Alphabetically sort names? Default yes.
#' @export
#' @examples
#' library(metafor); data(european_ancestry)
#' meta = rma(european_ancestry$r, sei = european_ancestry$SE_r)
#' GG_forest(meta, .names = european_ancestry$Author_sample)
GG_forest = function(.analysis, .names = NULL, .alphabetic_sort_names = T) {
  if (!inherits(.analysis, "rma")) stop("This function only works for rma objects from the metafor package.")

  #extract effect sizes and SEs
  d = tibble::data_frame(es = .analysis$yi,
                         var = .analysis$vi,
                         se = sqrt(var),
                         meta = "study")

  #names
  if (!is.null(.names)) {
    d$names = .names
  } else {
    d$names = "Study " + 1:nrow(d)
  }

  #make names unique if necessary
  if (any(duplicated(d$names))) {
    d$names %<>% str_uniquify
  }

  #sort?
  if (.alphabetic_sort_names) {
    d$names %<>% factor() %>% forcats::fct_rev()
  }

  #extract main effect
  d_meta = tibble::data_frame(es = .analysis$b %>% as.vector,
                      var = .analysis$se %>% sqrt,
                      se = .analysis$se,
                      meta = "meta",
                      names = "Main effect")

  #horizontal space case
  d_hline = tibble::data_frame(es = .analysis$b %>% as.vector,
                       var = .analysis$se %>% sqrt,
                       se = .analysis$se,
                       meta = "invis",
                       names = "")

  #add main effect to d
  d = rbind(d, d_meta, d_hline)

  #make sure meta effect is in the bottom
  d$names %<>% factor() %>% forcats::fct_relevel(c("Main effect", ""))

  #plot
  ggplot2::ggplot(d, aes(es, names, color = meta)) +
    geom_point() +
    geom_errorbarh(aes(xmin = es - se * 1.96,
                       xmax = es + se * 1.96)) +
    geom_hline(yintercept = 2, linetype = "dashed") +
    theme_bw() +
    scale_y_discrete(name = NULL) +
    scale_colour_manual(values = c("white", "black", "black"), guide = F) +
    xlab("Effect size")
}


#' Plot a contingency table with ggplot2
#'
#' Makes a pretty contingency table with ggplot2 using geom_tile.
#' @param .analysis (rma object) The rma analysis from metafor.
#' @param .CI (chr vector) Confidence interval to use. Default = .95.
#' @param .study_CI (lgl vector) Whether to plot confidence intervals for individual studies. Default no.
#' @export
GG_funnel = function(.analysis, .CI = .95, .study_CI = F) {
  if (!inherits(.analysis, "rma")) stop("This function only works for rma objects from the metafor package.")

  #convert CI to se z
  se_z = qnorm(1 - (1-.CI)/2)

  #extract main effect
  d_meta = tibble::data_frame(es = .analysis$b %>% as.vector,
                      var = .analysis$se %>% sqrt,
                      se = .analysis$se
  )

  #extract effect sizes and SEs
  d = tibble::data_frame(es = .analysis$yi,
                 var = .analysis$vi,
                 se = sqrt(var),
                 upper = d_meta$es + se_z * se,
                 lower = d_meta$es - se_z * se,
                 outlier = !is_between(es, lower, upper)
  )

  #calculate funnel
  d_funnel = tibble::data_frame(se = seq(0, max(d$se)*1.1, length.out = 1000),
                        upper = d_meta$es + se * se_z,
                        lower = d_meta$es - se * se_z)

  d_polygon = tibble::data_frame(x = c(min(d_funnel$lower), d_meta$es, max(d_funnel$upper)),
                         y = c(max(d_funnel$se), 0, max(d_funnel$se)))

  #plot
  gg = ggplot2::ggplot() +
    geom_line(data = d_funnel, aes(upper, se)) +
    geom_line(data = d_funnel, aes(lower, se)) +
    geom_polygon(data = d_polygon, aes(x, y), fill = "grey") +
    geom_vline(linetype = "dashed", xintercept = d_meta$es) +
    geom_point(data = d, aes(es, se, color = outlier)) +
    scale_color_manual(guide = F, values = c("black", "red")) +
    scale_y_reverse() +
    theme_bw() +
    xlab("Effect size")

  #study CIs
  if (.study_CI) {
    gg = gg +
      geom_errorbarh(data = d, aes(xmin = es - se_z * se,
                                   xmax = es + se_z * se,
                                   x = es,
                                   y = se))
  }

  gg
}


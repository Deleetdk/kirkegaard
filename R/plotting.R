#' Add text to a ggplot
#'
#' Convenient function to add text to a ggplot object
#' @details
#' Builds a grob using [grid::grobTree()] and [grid::textGrob()]. This text can then be positioned wherever the user wants it. It is more convenient than using [ggplot2::geom_text()] because positioning is difficult to get right with that as it depends on the data ranges. This function instead uses fractional positioning, e.g. .95 is the 95th centile of x/y, whatever that happens to be in the data. On top of this, 6 premade positions are made for common placements: top/bottom left/middle/right abbreviated as tl, tm, tr, bl, bm, br, respectively.
#' @param text (chr) A character scalar with text to use.
#' @param text_pos (chr) An abbreviation of the easy placement to use. If you want to set these manually, use "manual".
#' @param font_size (int) Font size.
#' @param font_color (various) Font color. Can be of various types, see details.
#' @param ... Extra arguments passed to various functions, see details.
#' @export
#' @return The outfrom from [ggplot2::annotation_custom()].
#' @examples
#' #make a plot with a point at 0,0, and large red text in middle
#' ggplot(tibble(x = 0, y = 0), aes(x, y)) +
#'   geom_point() +
#'   GG_text("red test", text_pos = "mm", font_color = "red", font_size = 20)
#'
#' #manually making the text position
#' ggplot(tibble(x = 0, y = 0), aes(x, y)) +
#'   geom_point() +
#'   GG_text("blue test", text_pos = "manual", font_color = "blue", font_size = 20, x = .25, y = .75)
GG_text = function(text, text_pos = "tl", font_size = 11, font_color = "black", ...) {
  text
  if (!(str_detect(text_pos, "[tmb][lmr]") | text_pos == "manual")) stop("`text_pos` incorrect. See details.")
  args = list(...)

  #text object location
  #if manual, let user set
  if (text_pos != "manual") {
    #vertical
    switch (str_sub(text_pos, start = 1, end = 1),
            t = {y = .98; vjust = 1},
            m = {y = .50; vjust = .5},
            b = {y = .02; vjust = -.1}
    )

    #horizontal
    switch (str_sub(text_pos, start = 2, end = 2),
            l = {x = .02; hjust = 0},
            m = {x = .50; hjust = 0.5},
            r = {x = .98; hjust = 1}
    )

  } else {
    #x and y must be present
    if (!all(c("x", "y") %in% names(args))) stop('When using `text_pos = "manual"`, you must supply both x and y values.')
    #set from ...
    x = args$x
    y = args$y

    #get hjust and vjust if present
    if ("hjust" %in% names(args)) {
      hjust = args$hjust
    } else {
      hjust = 0
    }

    if ("vjust" %in% names(args)) {
      vjust = args$vjust
    } else {
      vjust = 0
    }
  }

  #make text object
  #did the user supply a custom gpar object?
  if (! "gpar" %in% names(args)) {
    #use default gpar
    gpar = grid::gpar(fontsize = font_size,
                      col = font_color)
  } else {
    #use user's
    gpar = args$gapar
  }

  #make grob
  text_object = grid::grobTree(grid::textGrob(text,
                                              x = x,
                                              y = y,
                                              hjust = hjust,
                                              vjust = vjust),
                               gp = gpar)


  #return grob for ggplot
  return(annotation_custom(text_object))
}



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
#' @param histogram_pars (list) Histogram pars
#' @param no_y_axis_values (lgl) Hide numbers on the Y axis?
#' @param alpha Alpha for density fit.
#' @param ... Any other params we can try to match, mainly for backwards compatibility with geom_histogram pars
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
#' #autodetect fractional data
#' runif(1000) %>% GG_denhist()
GG_denhist = function(data, var = NULL, group = NULL, vline = mean, histogram_pars = NULL, clean_name = T, no_y_axis_values = T, alpha = .2, auto_fraction_bounary = T, ...) {

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
  if (is.atomic(data)) {
    #get name
    var = deparse(substitute(data))

    #put in a data frame as a vector
    data = data.frame(x = data %>% as.vector())

    #fix name
    colnames(data) = var
  }

  #1 column df
  if (is.data.frame(data) && ncol(data) == 1 && is.null(var)) {
    var = names(data)
    warning("received a data frame but no var: used the only available column")
  }

  #rename and drop unused
  df = data[c(var, group)]

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

  #histogram pars
  if (is.null(histogram_pars)) histogram_pars = list()

  #backwards compatible, look for pars and put in the par list
  pars = list(...)

  #fractions
  if (auto_fraction_bounary) {
    #values are fractions?
    if (all(is_between(df[[var]], 0, 1), na.rm = T)) {
      message("Input seems like a fraction, set `boundary=0` and `binwidth=1/30` to avoid issues near the limits. Disable this with `auto_fraction_bounary=F`")
      histogram_pars$boundary = 0
      histogram_pars$binwidth = 1/30
    }
  }

  #named args
  if ("binwidth" %in% names(pars)) histogram_pars$binwidth = pars$binwidth
  if ("bins" %in% names(pars)) histogram_pars$bins = pars$bins
  if ("center" %in% names(pars)) histogram_pars$center = pars$center
  if ("boundary" %in% names(pars)) histogram_pars$boundary = pars$boundary
  if ("breaks" %in% names(pars)) histogram_pars$breaks = pars$breaks

  #plot
  if (is.null(group)) {
    g = ggplot2::ggplot(df, aes(.data[[var]])) +
      geom_histogram(aes(y = after_stat(density)),  # Histogram with density instead of count on y-axis
                     colour="black", fill="white",
                     binwidth = histogram_pars$binwidth,
                     bins = histogram_pars$bins,
                     center = histogram_pars$center,
                     boundary = histogram_pars$boundary,
                     breaks = histogram_pars$breaks
      ) +
      geom_density(alpha = alpha, fill="#FF6666") # Overlay with transparent density plot
  } else {

    g = ggplot2::ggplot(df, aes(.data[[var]], fill = .data[[group]])) +
      geom_histogram(aes(y = after_stat(density)),  # Histogram with density instead of count on y-axis
                     colour="black", position = "dodge",
                     binwidth = histogram_pars$binwidth,
                     bins = histogram_pars$bins,
                     center = histogram_pars$center,
                     boundary = histogram_pars$boundary,
                     breaks = histogram_pars$breaks
      ) +
      geom_density(alpha = alpha) # Overlay with transparent density plot
  }


  #vline?
  if (!is.null(vline)) {
    #if chr func, get the function
    if (is.character(vline)) vline = get(vline)

    #if we can add na.rm=T, do it
    vline_func = try_else({
      vline_func = purrr::partial(vline, na.rm=T)
      #test it
      vline_func(1)

      vline_func
    }, else. = vline)

    #no groups
    if (is.null(group)) {
      #add it
      g = g + geom_vline(xintercept = vline_func(df[[var]]),
                         color="red",
                         linetype="dashed", linewidth = 1)
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
      g = g + geom_vline(xintercept = central_tendency, linetype = "dashed", linewidth = 1, color = colors)
    }
  }

  #clean name?
  if (clean_name) g = g + scale_x_continuous(name = str_clean(var))

  #theme
  g = g + ggplot2::theme_bw()

  #y axis values
  if (no_y_axis_values) {

    #hack solution to find the highest density
    gg = suppressMessages(ggplot_build(g))
    max_density = gg$layout$panel_scales_y[[1]]$range$range[2]

    #dont show values
    g = g + scale_y_continuous(breaks = c(0, max_density), labels = function(x) {
      #first value as lower, last as higher
      x[1] = "lower"
      x[length(x)] = "higher"
      #middle empty
      x[-c(1, length(x))] = ""
      x
    })

    #no ticks lines etc.
    g = g + theme(panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.ticks.y = element_blank())
  }

  g + ylab("Density")
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
#' Plots a scatterplot with a regression line and correlation information. Returns a `ggplot2` object.
#' @details Internally uses the ad hoc variables `.weights`, `.label` and `.color`. If you name your variables these, then you will get odd problems.
#'
#' @param df (data.frame) A data frame with variables.
#' @param x_var (chr scalar) X variable string.
#' @param y_var (chr scalar) Y variable string.
#' @param weights (num scalar) A set of weights to use.
#' @param color (chr) A variable to color points by.
#' @param alpha (num) The alpha to use.
#' @param text_pos (chr scalar) Where to put the text. Defaults to top left ("tl") if correlation is positive, or tr if negative. Can be tl, tr, bl, or br.
#' @param case_names (lgl scalar) Whether to add case names or not (default true).
#' @param case_names_color (lgl scalar) Color of case names.
#' @param CI (num scalar) Confidence interval as a fraction.
#' @param clean_names (lgl scalar) Whether to clean the axes names using `str_clean()`.
#' @param check_overlap (lgl scalar) Whether to avoid overplotting names.
#' @param weight_as_size (lgl scalar) Whether to resize points by the weights.
#' @param repel_names (lgl) If using case names, should they be repelled?
#' @param text_size (num) The size of the text. Defaults to 3.
#' @param ...
#'
#' @export
#' @examples
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width") #default plot
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", se = F) #no SE ribbon
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = rep("A", 150)) #case names
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = "Species") #case names from variable
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = "Species", case_names_color = "purple") #case names in purple
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = "Species", repel_names = T) #case names from variable, repelled
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", text_size = 5, case_names = "Species") #case names from variable, larger text
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", text_pos = "br") #other text location
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", CI = .99) #other CI
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", clean_names = F) #don't clean names
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", weights = 1:150) #add weights with vector
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", weights = "Petal.Width") #add weights with name
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", weights = 1:150, weight_as_size = F) #add weights with vector but don't resize
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", color = "Species") #color points
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", color = "Species", case_names = "Species") #color points, but labels stay black
#' GG_scatter(iris, "Sepal.Length", "Sepal.Width", alpha = .1) #change alpha
GG_scatter = function(df,
                      x_var,
                      y_var,
                      weights = NULL,
                      color = NULL,
                      se = T,
                      alpha = 1,
                      text_pos = NA,
                      case_names = NULL,
                      case_names_color = "black",
                      CI = .95,
                      clean_names = T,
                      check_overlap = T,
                      repel_names = F,
                      weight_as_size = T,
                      text_size = 3,
                      ...) {

  arg_list = list(...)
  if ("case_names_vector" %in% names(arg_list)) stop("The argument `case_names_vector` is no longer used. Use `case_names`.")

  #check if vars exist
  if (!x_var %in% colnames(df)) stop("X variable not found in data frame!")
  if (!y_var %in% colnames(df)) stop("Y variable not found in data frame!")

  #fail on 0 cases
  if (nrow(df) == 0) stop("There were no cases", call. = F)

  #case names?
  if (!is.null(case_names) && !are_equal(case_names, NA) && !are_equal(case_names, F)) {

    #chr?
    if (!is.character(case_names)) stop("`case_names` must be a character, NULL, NA or F, but it was %s", class(case_names))

    #is scalar chr, then assume it is name of variable
    if (is.character(case_names) && is_scalar(case_names)) {
      #is .rownames?
      if (kirkegaard::are_equal(case_names, ".rownames")) {
        df$.label = rownames(df)
      } else {
        #does it exist in data?
        if (!case_names %in% names(df)) stop(sprintf("Case names variable `%s` wasn't in the data.frame!", case_names))
        df$.label = df[[case_names]]
      }
    }

    #is non-scalar chr?
    if (is.character(case_names) && !is_scalar(case_names)) {
      if (length(case_names) != nrow(df)) stop("`case_names` wasn't the right length!")
      df$.label = case_names
    }

    case_names = T
  } else {
    #fill with empty
    #cannot fill with NA because this causes all cases to be dropped!
    df$.label = rep("", nrow(df))
    case_names = F
  }


  #weights
  weights_used = !is.null(weights)
  if (is.null(weights)) {
    df$.weights = rep(1, nrow(df)) #fill with 1's
  } else {
    #chr scalar
    if (is_scalar(weights) & is.character(weights)) {
      #does it exist in data?
      if (!weights %in% names(df)) stop(sprintf("`weights` variable `%s` wasn't in the data frame!", weights))
      df$.weights = df[[weights]]
    } else {
      #vector
      df$.weights = weights
    }
  }

  #color
  if (!is.null(color)) {
    #chr scalar?
    if (is_scalar(color) & is.character(color)) {
      #does it exist in data?
      if (!color %in% names(df)) stop(sprintf("`color` variable `%s` wasn't in the data frame!", color))
      df$.color = df[[color]]

      color_axis_title = color
    } else {
      #vector
      df$.color = color

      #get name of object
      color_axis_title = deparse(substitute(color))
    }

    #clean names? we clean the factor levels too
    if (clean_names) {
      #factor?
      if (is.factor(df$.color)) {
        levels(df$.color) = levels(df$.color) %>% str_clean()
      }

      #character?
      if (is.character(df$.color)) {
        #clean the chr
        df$.color = df$.color %>% str_clean()
      }
    }
  } else {
    #insert placeholder .color variable
    df$.color = T
  }

  #subset + remove NA
  df = na.omit(df[c(x_var, y_var, ".weights", ".label", ".color")])

  #fail on 0 cases
  if (nrow(df) == 0) stop("There were no complete cases", call. = F)

  ## text
  #correlation + CI
  #with weights, we have to rely on symmetric approach (bad)
  #without weights, we can use cor.test() from base
  if (weights_used) {
    #the warnings are about near-perfect fit, which is not a concern
    cor_res = suppressWarnings(weights::wtd.cor(df[1:2], weight = df$.weights))
    cor = cor_res$correlation[1, 2]
    #CI
    cor_CI = c(
      cor - qnorm(CI + ((1-CI)/2), lower.tail = T) * cor_res$std.err[1, 2],
      cor + qnorm(CI + ((1-CI)/2), lower.tail = T) * cor_res$std.err[1, 2]
    ) %>% winsorise(upper = 1, lower = -1)
  } else {
    #the warning is about 0 SD, but we throw an error about this below instead
    cor_res = suppressWarnings(cor.test(df[[1]], df[[2]], conf.level = CI))
    cor = cor_res$estimate
    cor_CI = cor_res$conf.int
  }


  #fail on NA correlation
  if (is.na(cor)) stop("Correlation could not be computed because of no variation in complete cases or at all", call. = F)



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
    text = paste0("r=", cor %>% format(nsmall = 2, digits = 2), " [CI", CI*100,": ", cor_CI[1] %>% format(nsmall = 2, digits = 2), " ", cor_CI[2] %>% format(nsmall = 2, digits = 2), "]",
                  "\nn=", nrow(df))
  } else {
    text = paste0("r=", cor %>% format(nsmall = 2, digits = 2),
                  "\nn=", nrow(df))
  }


  #text object
  text_object = grid::grobTree(grid::textGrob(text, x = x,  y = y, hjust = hjust, vjust = vjust),
                               gp = grid::gpar(fontsize = 11))


  #plot!
  #4 options due to weights and coloring params
  if (is.null(color)) {
    if (is.null(weights)) {
      g = ggplot2::ggplot(df, aes(.data[[x_var]], .data[[y_var]])) +
        geom_point(alpha = alpha)
    } else {
      #weight as size?
      if (weight_as_size) {
        g = ggplot2::ggplot(df, aes(.data[[x_var]], .data[[y_var]], weight = .weights)) +
          geom_point(aes(size = .weights), alpha = alpha) +
          scale_size_continuous(guide = "none")
      } else {
        g = ggplot2::ggplot(df, aes(.data[[x_var]], .data[[y_var]], weight = .weights)) +
          geom_point(alpha = alpha)
      }

    }
  } else {
    if (is.null(weights)) {
      g = ggplot2::ggplot(df, aes(.data[[x_var]], .data[[y_var]], color = .color)) +
        geom_point(alpha = alpha)
    } else {
      if (weight_as_size) {
        g = ggplot2::ggplot(df, aes(.data[[x_var]], .data[[y_var]], weight = .weights, color = .color)) +
          geom_point(aes(size = .weights), alpha = alpha) +
          scale_size_continuous(guide = "none")
      } else {
        g = ggplot2::ggplot(df, aes(.data[[x_var]], .data[[y_var]], weight = .weights, color = .color)) +
          geom_point(alpha = alpha)
      }

    }

  }


  #add regression line
  #note that weights are automatically taken into account because they are set above
  g = g + geom_smooth(method = lm, se = se, color = "orange") +
    annotation_custom(text_object)


  #case names?
  if (is.null(weights)) {y_nudge = 1.25} else {y_nudge = 2}
  if (case_names) {
    #note, remove color aes
    if (!repel_names) {
      #show.legend fix due to http://stackoverflow.com/questions/18337653/remove-a-from-legend-when-using-aesthetics-and-geom-text
      g = g + geom_text(aes(label = .label), color = case_names_color, size = text_size, vjust = y_nudge, check_overlap = check_overlap, show.legend = FALSE)
    } else {
      g = g + ggrepel::geom_text_repel(aes(label = .label), color = case_names_color, size = text_size, show.legend = FALSE)
    }
  }

  #clean?
  if (clean_names) {
    #axes labels
    g = g + xlab(str_clean(x_var)) + ylab(str_clean(y_var))
  }

  #color label
  if (!is.null(color)) {
    g = g + ggplot2::labs(color = color_axis_title)
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
#' @param type (chr scalar) The type of plot. Options: bar, point, points, boxplot, violin, violin2.
#' @param msg_NA (log scalar) Show a message if NAs were removed? (default true)
#' @param split_group_labels (log scalar) Whether to automatically insert newlines into group labels if they are too long (default yes).
#' @param line_length (num scalar) The desired line width (default 95). Only used when split_group_labels = T.
#' @param min_n Minimum sample size per group.
#' @param detect_prop Detect proportions and use prop.test().
#' @export
#' @examples
#' #simple examples
#' GG_group_means(iris, "Sepal.Length", "Species")
#' GG_group_means(iris, "Sepal.Length", "Species", type = "point")
#' GG_group_means(iris, "Sepal.Length", "Species", type = "points")
#' GG_group_means(iris, "Sepal.Length", "Species", type = "points", CI = .999999)
#' #dont want a CI
#' GG_group_means(iris, "Sepal.Length", "Species", type = "points", CI = NA)
#' GG_group_means(iris, "Sepal.Length", "Species", type = "points", CI = F)
#' GG_group_means(iris, "Sepal.Length", "Species", type = "points", CI = NULL)
#' GG_group_means(iris, "Sepal.Length", "Species", type = "points", CI = 0)
#' GG_group_means(iris, "Sepal.Length", "Species", type = "boxplot")
#' GG_group_means(iris, "Sepal.Length", "Species", type = "violin")
#' GG_group_means(iris, "Sepal.Length", "Species", type = "violin2")
#' #sample sizes
#' GG_group_means(iris, "Sepal.Length", "Species", type = "points", add_sample_sizes_to_labels = T)
#'
#' #subgroups too
#' iris$type = sample(LETTERS[1:3], size = nrow(iris), replace = T)
#' GG_group_means(iris, var = "Sepal.Length", groupvar = "Species", subgroupvar = "type")
#' GG_group_means(iris, var = "Sepal.Length", groupvar = "Species", subgroupvar = "type", type = "point")
#' GG_group_means(iris, var = "Sepal.Length", groupvar = "Species", subgroupvar = "type", type = "points")
#' GG_group_means(iris, var = "Sepal.Length", groupvar = "Species", subgroupvar = "type", type = "boxplot")
#' GG_group_means(iris, var = "Sepal.Length", groupvar = "Species", subgroupvar = "type", type = "violin")
#' GG_group_means(iris, var = "Sepal.Length", groupvar = "Species", subgroupvar = "type", type = "violin2")
#'
#' #proportion
#' iris$onezero = sample(c(0, 1), size = nrow(iris), replace = T)
#' GG_group_means(iris, "onezero", "Species")
#' GG_group_means(iris, "onezero", "Species", subgroupvar = "type")
#'
#' #different dataset
#' GG_group_means(mtcars, "mpg", "cyl", add_sample_sizes_to_labels = T, type = "points")
GG_group_means = function(df, var, groupvar = NULL, subgroupvar = NULL, CI = .95, type = "bar", na.rm = T, msg_NA = T, split_group_labels = T, line_length = 95, min_n = 0, detect_prop = T, add_sample_sizes_to_labels = F) {

  #check input
  if (is.factor(df[[var]]) | is.character(df[[var]])) stop("You probably want to use `GG_proportions()` for these data", call. = F)

  #convert
  df = as.data.frame(df)
  df[[var]] = as.numeric(df[[var]])
  df[[groupvar]] = factor(df[[groupvar]])
  if (!is.null(subgroupvar)) df[[subgroupvar]] = factor(df[[subgroupvar]])

  #draw CIs?
  draw_CI = !is_nullish(CI)

  #prop?
  is_prop = F
  if (detect_prop) {
    if (setequal(unique(na.omit(df[[var]])), c(0, 1))) {
      is_prop = T
      message("Proportion variable detected, using `prop.test()`")
    }
  }

  # make symbols
  var_sym = as.symbol(var)

  #no subgroupvar variable, simple
  if (is.null(subgroupvar)) {

    #checks
    if (!var %in% colnames(df)) stop("Numerical isn't in the data frame!")
    if (!groupvar %in% colnames(df)) stop("Group variable isn't in the data frame!")
    if (!type %in% c("bar", "point", "points", "boxplot", "violin", "violin2")) stop("Type not recognized! Supported values: bar, boxplot, point, points, violin, violin2")

    #subset
    df = df[c(var, groupvar)]

    #check for missing
    if (miss_count(df) > 0 ) {
      #remove missing?
      if (na.rm) {
        df = miss_filter(df, missing = 0)
        silence(message("Missing values were removed."), messages = msg_NA)
      } else {
        stop("There must not be missing values present when na.rm = F!")
      }
    }

    #check for no data
    if (nrow(df) == 0) stop("No overlapping non-missing data.")

    #enforce factor, drop empty
    df[[groupvar]] = factor(df[[groupvar]]) %>% fct_drop()

    #add sample sizes to labels?
    if (add_sample_sizes_to_labels) {
      df_sum = plyr::ddply(df, groupvar, function(dd) {
        tibble(n = nrow(dd))
      })

      #replace levels
      levels(df[[groupvar]]) = levels(df[[groupvar]]) + "\nn=" + df_sum$n
    }

    #summarize
    df_sum = plyr::ddply(df, groupvar, function(dd) {
      #describe
      desc = describe2(dd[[var]], all_vars = T)
      desc$group1 = dd[[groupvar]][1]

      #add CIs if wanted
      if (draw_CI) {
        if (is_prop) {
          #prop
          proptest = prop.test(sum(dd[[var]] == 1), length(dd[[var]]))

          desc %<>% mutate(
            ci_lower = proptest$conf.int[1],
            ci_upper = proptest$conf.int[2]
          )
        } else {
          #standard symmetric CI
          desc %<>% mutate(
            ci_bar = suppressWarnings(qt(1 - ((1 - CI) / 1.96), df = n - 1)),
            ci_lower = mean - ci_bar * se,
            ci_upper = mean + ci_bar * se
          )
        }
      }

      desc
    })

    #filter too small groups
    df_sum = df_sum %>% filter(n >= min_n)

    #drop empty levels
    df_sum$group1 %<>% fct_drop()

    #drop raw data rows if their level was dropped
    df %<>% filter(df[[groupvar]] %in% levels(df_sum$group1))

    #drop empty levels there too
    df[[groupvar]] %<>% fct_drop()

    #check for data
    if (nrow(df_sum) == 0) stop("No groups left after filtering to sample size requirement", call. = F)

    #error bar
    g_eb1 = geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .2, color = "red")
    g_eb2 = geom_errorbar(aes(group1, ymin = ci_lower, ymax = ci_upper), width = .2, color = "red")
    g_eb3 = geom_errorbar(data = df_sum, aes(group1, ymin = ci_lower, ymax = ci_upper), width = .2, color = "red")

    #plot
    if (type == "bar") {
      g = ggplot2::ggplot(df_sum, aes(group1, mean)) +
        geom_bar(stat = "identity")

      if (draw_CI) g = g + g_eb1
    }

    if (type == "boxplot") {
      df$group1 = df[[groupvar]]
      g = ggplot2::ggplot(df, aes(group1, !!var_sym)) +
        geom_boxplot()
    }

    if (type == "point") {
      g = ggplot2::ggplot(df_sum, aes(group1, mean)) +
        geom_point()

      if (draw_CI) g = g + g_eb1
    }

    if (type == "points") {
      g = ggplot2::ggplot(df_sum) + #use summed as the default data, otherwise the code for adding newlines removes the labels
        geom_point(data = df, aes(.data[[groupvar]], .data[[var]])) +
        geom_point(aes(group1, mean), color = "red", size = 3)

      if (draw_CI) g = g + g_eb2
    }

    if (type == "violin") {
      g = ggplot2::ggplot(df_sum) +
        geom_violin(data = df, aes(.data[[groupvar]], .data[[var]], fill = .data[[groupvar]]), alpha = .5) +
        scale_fill_discrete(guide = "none") +
        geom_point(data = df_sum, aes(group1, mean), color = "red", size = 3)


      if (draw_CI) g = g + g_eb3
    }

    if (type == "violin2") {
      g = ggplot2::ggplot(df_sum) +
        geom_violin(data = df, aes(.data[[groupvar]], .data[[var]], fill = .data[[groupvar]]), alpha = .5) +
        geom_count(data = df, aes(.data[[groupvar]], .data[[var]])) +
        scale_fill_discrete(guide = "none") +
        geom_point(data = df_sum, aes(group1, mean), color = "red", size = 3)

      if (draw_CI) g = g + g_eb3
    }

    if (split_group_labels) {
      g = g + scale_x_discrete(labels = levels(g$data$group1) %>% add_newlines(line_length = line_length))
    }

    #labels
    g = g +
      xlab(groupvar) +
      ylab(var)
  }

  #if plot by subgroup too
  if (!is.null(subgroupvar)) {
    #make copy of variable
    df$subgroupvar = df[[subgroupvar]]

    #checks
    if (!var %in% colnames(df)) stop("Variable isn't in the data frame!")
    if (!groupvar %in% colnames(df)) stop("Group variable isn't in the data frame!")
    if (!subgroupvar %in% colnames(df)) stop("Color variable isn't in the data frame!")
    if (!type %in% c("bar", "point", "points", "boxplot", "violin", "violin2")) stop("Type not recognized! Supported values: bar, point, points, boxplot, violin, violin2")

    #subset
    df = df[c(var, groupvar, subgroupvar)]

    #check for missing
    if (miss_count(df) > 0 ) {
      #remove missing?
      if (na.rm) {
        df = miss_filter(df, missing = 0)
        silence(message("Missing values were removed."), messages = msg_NA)
      } else {
        stop("There must not be missing values in the data when na.rm = F!")
      }
    }

    # enforce factors, drop empty levels
    df[[groupvar]] = factor(df[[groupvar]]) %>% fct_drop()
    df[[subgroupvar]] = factor(df[[subgroupvar]]) %>% fct_drop()

    #add sample sizes to labels?
    if (add_sample_sizes_to_labels) {
      df_sum = plyr::ddply(df, groupvar, function(dd) {
        tibble(n = nrow(dd))
      })

      #replace levels
      levels(df[[groupvar]]) = levels(df[[groupvar]]) + "\nn=" + df_sum$n
    }

    #check for no data
    if (nrow(df) == 0) stop("No overlapping non-missing data.")

    #summarize
    df_sum = plyr::ddply(df, .variables = c(groupvar, subgroupvar), .fun = function(dd) {
      #describe
      desc = describe2(dd[[var]], all_vars = T)

      #add CIs
      if (draw_CI) {
        if (is_prop) {
          #prop
          proptest = prop.test(sum(dd[[var]] == 1), length(dd[[var]]))

          desc %<>% mutate(
            ci_lower = proptest$conf.int[1],
            ci_upper = proptest$conf.int[2]
          )
        } else {
          #standard symmetric CI
          desc %<>% mutate(
            ci_bar = suppressWarnings(qt(1 - ((1 - CI) / 1.96), df = n - 1)),
            ci_lower = mean - ci_bar * se,
            ci_upper = mean + ci_bar * se
          )
        }
      }

      desc
    })

    #copy vars
    df_sum$groupvar = df_sum[[groupvar]]
    df_sum$subgroupvar = df_sum[[subgroupvar]]
    df$var = df[[var]]
    df$groupvar = df[[groupvar]]
    df$subgroupvar = df[[subgroupvar]]

    #filter too small groups
    df_sum = df_sum %>% filter(n >= min_n)

    #drop empty levels again
    df_sum$groupvar %<>% fct_drop()
    df_sum$subgroupvar %<>% fct_drop()

    #drop raw data rows if their level was dropped
    df %<>% filter(groupvar %in% levels(df_sum$groupvar),
                   subgroupvar %in% levels(df_sum$subgroupvar))

    #drop empty levels there too
    df[[groupvar]] %<>% fct_drop()
    df[[subgroupvar]] %<>% fct_drop()

    #check for data
    if (nrow(df_sum) == 0) stop("No groups left after filtering to sample size requirement", call. = F)

    #plot
    if (type == "bar") {
      g = ggplot2::ggplot(df_sum, aes(x = groupvar, y = mean, fill = subgroupvar)) +
        geom_bar(stat="identity", position = "dodge")

      if (draw_CI) g = g + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = .9), width = .2)
    }

    if (type == "boxplot") {
      # browser()
      df$group1 = df[[groupvar]]
      g = ggplot2::ggplot(df, aes(x = group1, y = !!var_sym, fill = subgroupvar)) +
        geom_boxplot(position = "dodge")
    }

    if (type == "point") {
      g = ggplot2::ggplot(df_sum, aes(groupvar, mean, color = subgroupvar)) +
        geom_point(position = position_dodge(width = .9))

      if (draw_CI) g = g + geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, group = subgroupvar), position = position_dodge(width = .9), color = "black", width = .2)
    }

    if (type == "points") {
      g = ggplot2::ggplot(df_sum) + #use summed as the default data, otherwise the code for adding newlines removes the labels
        geom_point(data = df, aes(groupvar, y = var, color = subgroupvar), position = position_dodge(width = .9)) +
        geom_point(aes(groupvar, y = mean, group = subgroupvar), color = "black", size = 4, position = position_dodge(width = .9), shape = 5)
      if (draw_CI) g = g + geom_errorbar(aes(groupvar, group = subgroupvar, ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = .9), width = .2)
    }

    if (type == "violin") {
      g = ggplot2::ggplot(df_sum) + #use summed as the default data, otherwise the code for adding newlines removes the labels
        geom_violin(data = df, aes(groupvar, y = var, fill = subgroupvar), position = position_dodge(width = .9)) +
        geom_point(aes(groupvar, y = mean, group = subgroupvar), color = "black", size = 4, position = position_dodge(width = .9), shape = 5)
      if (draw_CI) g = g + geom_errorbar(aes(groupvar, group = subgroupvar, ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = .9), width = .2)
    }

    if (type == "violin2") {
      g = ggplot2::ggplot(df_sum) + #use summed as the default data, otherwise the code for adding newlines removes the labels
        geom_violin(data = df, aes(groupvar, y = var, fill = subgroupvar), position = position_dodge(width = .9), alpha = .5) +
        geom_count(data = df, aes(groupvar, y = var, group = subgroupvar), position = position_dodge(width = .9)) +
        geom_point(aes(groupvar, y = mean, group = subgroupvar), color = "red", size = 4, position = position_dodge(width = .9), shape = 5)

      if (draw_CI) g = g + geom_errorbar(aes(groupvar, group = subgroupvar, ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = .9), width = .2, color = "red")
    }

    if (split_group_labels) {
      g = g +
        scale_x_discrete(labels = levels(g$data$groupvar) %>% add_newlines(line_length = line_length))
    }


    #labels
    g = g +
      xlab(groupvar) +
      scale_color_discrete(name = subgroupvar) +
      scale_fill_discrete(name = subgroupvar) +
      ylab(var)
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





#' Save a ggplot2 figure
#'
#' Save a ggplot2 figure. Now with a sensible default size (10 x 6.5 inches). Same arguments as `ggplot2::ggsave`
#' @export
GG_save = function(filename, plot = last_plot(), path = NULL, width = 10, height = 6.5, ...) {
  #make dir if needed
  if (!dir.exists(dirname(filename))) {
    message("Directory did not exist, creating...")
    dir.create(dirname(filename), showWarnings = F, recursive = T)
  }

  #ggtern
  #i dont see a way to detect this class easily
  if (plot$coordinates %>% is(class2 = "CoordTern")) {
    ggtern::ggsave(filename = filename, plot = plot, width = width, height = height, path = path, ...)
    return(invisible(NULL))
  }

  #save standard
  ggplot2::ggsave(filename = filename, plot = plot, width = width, height = height, path = path, ...)
  return(invisible(NULL))
}





#' Save list of ggplot2 objects to single pdf
#'
#' @param list (list) List of ggplot2 objects.
#' @param filename (chr) What to call the pdf.
#'
#' @return Invisible NULL.
#' @export
#'
#' @examples
#' #plot histogram of each numeric variable in iris
#' list_iris = map(names(iris[-5]), ~ggplot(iris, aes(.data[[.]])) + geom_histogram())
#' #save to a single pdf
#' GG_save_pdf(list_iris, "test.pdf")
#' file.remove("test.pdf")
GG_save_pdf = function(list, filename) {
  #start pdf
  pdf(filename)

  #loop
  for (p in list) {
    print(p)
  }

  #end pdf
  dev.off()

  invisible(NULL)
}



#http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

#' Heatmap correlation matrix with ggplot2
#'
#' @param data Data frame or a matrix
#' @param add_values Whether to add the correlation sizes as text to plot
#' @param reorder_vars Whether to reorder variables so strongly related ones are close to each other
#' @param digits How many digits to print when plotting them
#' @param font_size If correlation values are plotted, their size
#' @param color_label Which label to use for the color scale legend
#' @param legend_position Position of the legend. Sometimes you may need to move it a bit
#' @param short_x_labels Adds integers to the y axis labels, and replaces the x labels with the same integers. Useful when there are many variables.
#' @param axis_labels_clean_func A function to clean the labels with, typically to remove underscores. NULL means disable.
#' @param dodge_x_labels How much to dodge the x labels. Useful when there are many variables.
#' @param min_n Minimum number of observations to include a correlation
#' @param pairwise_n Pairwise number of observations. If not given, it is computed from the data.
#' @param pairwise_p Pairwise p-values. If not given, it is computed from the data unless data is a matrix.
#' @param cross_out_nonsig Cross out non-significant correlations
#' @param p_sig Significance level for cross out
#' @param remove_diag Remove diagonal values
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' #data input
#' mtcars[c(1,3,4,5,6,7)] %>% GG_heatmap()
#' mtcars[c(1,3,4,5,6,7)] %>% GG_heatmap(reorder_vars = F)
#' mtcars[c(1,3,4,5,6,7)] %>% GG_heatmap(color_label = "some other text")
#' mtcars[c(1,3,4,5,6,7)] %>% GG_heatmap(short_x_labels = T)
#' mtcars[c(1,3,4,5,6,7)] %>% GG_heatmap(cross_out_nonsig = T)
#' mtcars[c(1,3,4,5,6,7)] %>% GG_heatmap(remove_diag = T)
#'
#' #Automatic cleaning of the axis labels, can be turned off
#' iris[-5] %>% GG_heatmap()
#' iris[-5] %>% GG_heatmap(axis_labels_clean_func = NULL)
#'
#' #cor matrix input
#' mt_cars_cors = psych::corr.test(mtcars[c(1,3,4,5,6,7)], adjust = "none")
#' mt_cars_cors$r %>% GG_heatmap()
#' GG_heatmap(mt_cars_cors$r, pairwise_p = mt_cars_cors$p, cross_out_nonsig = T)
#'
#' #custom values input
#' MAT_vector2full(c(.5, .3, .2), diag_value = 1) %>%
#' set_colnames(letters[1:3]) %>%
#' set_rownames(letters[1:3]) %>%
#' GG_heatmap()
GG_heatmap = function(
    data,
    add_values = T,
    reorder_vars = T,
    digits = 2,
    font_size = 4,
    color_label = "Correlation",
    legend_position = c(0.4, 0.7),
    short_x_labels = F,
    axis_labels_clean_func = str_clean,
    dodge_x_labels = 1,
    min_n = 0,
    pairwise_n = NULL,
    pairwise_p = NULL,
    cross_out_nonsig = F,
    p_sig = 0.01,
    remove_diag = F
) {

  #bad arguments
  if (remove_diag & short_x_labels) stop("Cannot remove diagonal and use short x labels", call. = F)

  #correlations
  #compute if given as data
  #coerce to numeric to deal with ordinals
  if (is.data.frame(data)) {
    # browser()
    cormat_full = psych::corr.test(data, adjust = "none")
    cormat = cormat_full$r
    pairwise_p = cormat_full$p
  } else if (is.matrix(data)) {
    cormat = data

    #if min_n is given, must also specify pairwise_n
    if (min_n > 0 & is.null(pairwise_n)) stop("If `min_n` is given with matrix input, `pairwise_n` must also be given", call. = F)
  } else {
    stop("Data must be a data frame or a matrix")
  }

  #can we reorder data? not if there's missing values in cor matrix
  if (any(is.na(cormat)) & reorder_vars) {
    message("Cannot reorder variables due to missing values in correlation matrix.")
    reorder_vars = F
  }

  #reorder variable?
  if (reorder_vars) {
    # Use correlation between variables as distance
    dd = as.dist((1-cormat)/2)
    hc = hclust(dd)
    cormat_reordered = cormat[hc$order, hc$order]
  } else {
    cormat_reordered = cormat
  }

  #remove values with too few observations
  if (min_n > 0) {
    if (is.null(pairwise_n)) pairwise_n = psych::pairwiseCount(data)
    cormat[pairwise_n < min_n] = NA_real_

    #reorder again if needed
    if (reorder_vars) {
      cormat_reordered = cormat[hc$order, hc$order]
    }
  }

  #if we reordered, use the reordered version
  if (reorder_vars) {
    cormat = cormat_reordered
  }

  #remove lower tri values
  cormat[lower.tri(cormat)] = NA
# browser()
  #remove diag
  if (remove_diag) diag(cormat) = NA

  #'melt' to long form
  #https://stackoverflow.com/questions/47475897/correlation-matrix-tidyr-gather-v-reshape2-melt
  melted_cormat = as.data.frame(cormat) %>%
    mutate(Var1 = factor(row.names(.), levels=row.names(.))) %>%
    gather(key = Var2, value = value, -Var1, na.rm = TRUE, factor_key = TRUE)

  #recode out of bounds values
  #error in weights package
  melted_cormat$value %<>% winsorise(upper = 1, lower = -1)

  #make axis labels
  #depends on diagonals being removed
  if (!remove_diag) {
    x_labels = melted_cormat$Var1 %>% levels()
    y_labels = melted_cormat$Var1 %>% levels()
  } else {
    x_labels = melted_cormat$Var1 %>% levels() %>% .[-1]
    y_labels = melted_cormat$Var1 %>% levels() %>% {.[1:(length(.) - 1)]}
  }

  #clean labels?
  if (is.function(axis_labels_clean_func)) {
    y_labels = y_labels %>% axis_labels_clean_func()
    x_labels = x_labels %>% axis_labels_clean_func()
  }

  #make them short? if many labels, this can help
  if (short_x_labels) {
    y_labels = y_labels + " " + seq_along(y_labels)
    x_labels = seq_along(y_labels)
  }

  #plot
  ggheatmap = ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_x_discrete(labels = x_labels, guide = guide_axis(n.dodge = dodge_x_labels)) +
    scale_y_discrete(labels = y_labels) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name = color_label) +
    theme_minimal() + # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 12, hjust = 1),
          legend.justification = c(1, 0),
          legend.position = "inside",
          legend.position.inside = legend_position,
          legend.direction = "horizontal",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank()
    ) +
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5)) +
    coord_fixed()

  #add values?
  if (add_values) {
    #round values
    if (!is.null(digits)) {
      melted_cormat$value2 = melted_cormat$value %>% str_round(digits = digits)
    } else {
      melted_cormat$value2 = melted_cormat$value
    }

    ggheatmap = ggheatmap +
      geom_text(data = melted_cormat, mapping = aes(Var2, Var1, label = value2), color = "black", size = font_size)

    #nonsig p value marking
    if (cross_out_nonsig) {
      #we need p values if this is to be done
      #which we have if correlations are automatically calculated
      #but not if we were given a cor mat alone
      assertthat::assert_that(!is.null(pairwise_p), msg = "Need pairwise p values to cross out non-significant correlations (these are calculated automatically if using data input with Pearson correlations).")

      #reorder p values too if need be
      if (reorder_vars) {
        pairwise_p = pairwise_p[hc$order, hc$order]
      }

      #remove lower tri values
      pairwise_p[lower.tri(pairwise_p)] = NA

      #remove diag
      diag(pairwise_p) = NA

      #melt
      melted_p = as.data.frame(pairwise_p) %>%
        mutate(Var1 = factor(row.names(.), levels=row.names(.))) %>%
        gather(key = Var2, value = value, -Var1, na.rm = TRUE, factor_key = TRUE) %>%
        filter(value >= p_sig)

      #add marking for non-significant values
      ggheatmap = ggheatmap +
        geom_point(data = melted_p, mapping = aes(Var2, Var1), color = "black", size = font_size, shape = 4, alpha = 0.8)
    }
  }

  ggheatmap
}


#' Matrix of data values
#'
#' @param x A data frame
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' iris[1:4] %>% GG_matrix()
GG_matrix = function(x) {
  # browser()
  #scale numeric data to 0-1 range
  is_num = map_lgl(x, ~is.numeric(.) & !is.factor(.))
  for (i in seq_along(x)) {
    if (is_num[i]) x[[i]] = x[[i]] %>% transform_01()
  }

  #to long form
  x_long = x[, is_num] %>% mutate(.id = 1:n()) %>% pivot_longer(cols = -.id)

  #plot
  ggplot(x_long, aes(.id, name, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "blue", high = "green", na.value = "red") +
    xlab("Case number") +
    ylab("Variable")
}



#' ggplot2 proportions plot
#'
#' @param x A factor variable
#' @param group Grouping variable
#' @param drop_empty Drop empty combinations
#' @param stacked Stacked bar
#' @param add_values Add values to the plot
#' @param repel Repel the text labels in case of problematic overplotting
#' @param seed Seed for the repel algorithm
#'
#' @return A ggplot2 of proportions
#' @export
#'
#' @examples
#' #plot the proportions of cylinders by year
#' GG_proportions(mpg$year, mpg$cyl)
#'
#' #remove the 0%'s
#' GG_proportions(mpg$year, mpg$cyl, drop_empty = T)
#' #stacked
#' GG_proportions(mpg$year, mpg$cyl, drop_empty = T, stacked = T)
#' #with values
#' GG_proportions(mpg$year, mpg$cyl, drop_empty = T, stacked = T, add_values = T)
#' #with text repel, but it doesn't work so well
#' GG_proportions(mpg$year, mpg$cyl, drop_empty = T, stacked = T, add_values = T, repel = T)
GG_proportions = function(x, group, drop_empty = F, stacked = F, add_values = F, repel = F, seed = 1) {

  #assert data type
  x = as.factor(x)
  group = as.factor(group)

  #not both add_values and non-stacked
  if (add_values & !stacked) stop("Cannot have both `add_values = T` and `stacked = F`")

  #get the proportion tests
  prop_tests(x, group) ->
    prop_test_results

  #filter 0%'s
  if (drop_empty) {
    prop_test_results = prop_test_results %>% filter(n_level > 0)
  }

  # Calculate cumulative percentages and label positions
  # browser()
  prop_test_results = prop_test_results %>%
    arrange(
      desc(group), desc(level)
    ) %>%
    plyr::ddply(c("group"), function(dd) {
      dd$label = scales::percent(dd$estimate)
      dd$label_position = cumsum(dd$estimate) - (dd$estimate / 2)
      dd
    }
    )

  #plot them
  if (!stacked) {
    prop_test_results %>%
      ggplot(aes(group, estimate, fill = level)) +
      geom_bar(position = "dodge", stat = "identity") +
      geom_errorbar(aes(ymax = conf.high, ymin = conf.low), position = "dodge", alpha = .3) +
      theme_bw() +
      scale_y_continuous(NULL, labels = scales::percent) ->
      gg
  } else {
    #do stacked proportions plot

    prop_test_results %>%
      ggplot(aes(group, estimate, fill = level)) +
      geom_bar(position = "stack", stat = "identity") +
      theme_bw() +
      scale_y_continuous(NULL, labels = scales::percent) ->
      gg
  }

  #if add values
  if (add_values) {
    if (!repel) {
      gg = gg + geom_text(aes(label = label, y = label_position), vjust = 0)
    } else {
      gg = gg + ggrepel::geom_text_repel(aes(label = label, y = label_position),
                                         vjust = 0,
                                         direction = "x",
                                         box.padding = 0,
                                         point.padding = 0,
                                         seed = seed)
    }

  }

  #attach results in case people want to reuse
  attr(gg, "prop_tests") = prop_test_results

  gg
}


#' Save a non-ggplot2 plot to a png file
#'
#' @param code A code chunk that produces a plot
#' @param filename The desired filename
#' @param width Width of image
#' @param height Height of image
#'
#' @return The output of the code chunk if any, invisibly
#' @export
#'
#' @examples
#' plot(1:3)
#' save_plot_to_file(plot(1:3), filename = "test.png")
#' file.remove("test.png")
save_plot_to_file <- function(code, filename, width = 1000, height = 750) {
  #stop graphics device on exit of function
  on.exit(dev.off())

  #call png()
  # rlang::exec(graphical_device, filename = filename, width=width, height=height, !!!list(...))
  png(filename = filename, height = height, width = width)

  #make plot
  p <- eval.parent(substitute(code))

  #print plot (sometimes needed)
  if (!is.null(p)) print(p)

  invisible(p)
}



#' Plot model coefficients for comparison
#'
#' Expects output from `broom::tidy()` on a list of models. Most conveniently from `get_model_coefs()` or `compare_predictors`.
#'
#' @param model_coefs A data frame with model coefficients
#' @param exclude A string to exclude from the plot. Default is "(Intercept)"
#' @param highlight_sig Highlight variables with p values below this threshold. Default is NULL, which means no highlighting.
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' #compare the coefficients of two models
#' iris_model_coefs = compare_predictors(iris, names(iris)[1], names(iris)[-1])
#' GG_plot_models(iris_model_coefs)
#' GG_plot_models(iris_model_coefs, highlight_sig = .05)
GG_plot_models = function(model_coefs, exclude = "(Intercept)", highlight_sig = NULL) {
  #filter excluded variables
  model_coefs = model_coefs %>% filter(term != exclude)

  #plot model coefficients
  if (is.null(highlight_sig)) {
    p = model_coefs %>%
      ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = model, group = model)) +
      geom_pointrange(position = position_dodge(width = 0.5), alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = 2) +
      coord_flip()
  } else {
    #highlight variables with p values below threshold
    p = model_coefs %>%
      mutate(
        highlight = ifelse(p.value < highlight_sig, 1, 0.5)
      ) %>%
      ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = model, group = model)) +
      geom_pointrange(aes(alpha = highlight), position = position_dodge(width = 0.5)) +
      geom_hline(yintercept = 0, linetype = 2) +
      coord_flip() +
      scale_alpha_identity()
  }

  #add labels and theme
  p + labs(
    x = "Predictor",
    y = "Coefficient",
    color = "Model",
    alpha = str_glue("p < {highlight_sig}")
  ) +
    theme_bw()
}

#create a term mapping table between raw variables and their expanded dummy coded subvariables
#Grok solved this issue
create_term_mapping <- function(data, formula) {
  # Convert the formula to a terms object to handle '.' expansion
  formula <- as.formula(formula)
  terms_obj <- terms(formula, data = data)

  # Get the response variable (if any) and predictor variables
  response <- attr(terms_obj, "response")  # Index of response variable (0 if none)
  predictor_vars <- attr(terms_obj, "term.labels")  # Names of predictor variables

  # If there's a response, exclude it from the terms to process
  all_vars <- all.vars(formula)
  if (response > 0) {
    response_var <- all_vars[response]
    vars_to_process <- predictor_vars
  } else {
    vars_to_process <- predictor_vars
  }

  # Initialize vectors for term_raw and term
  term_raw <- c()
  term_expanded <- c()

  # Loop through each predictor term
  for (term in vars_to_process) {
    if (term %in% names(data)) {  # Ensure the term exists in the data frame
      # Check if the term is a factor
      if (is.factor(data[[term]])) {
        # Get levels of the factor (excluding the first level for dummy coding)
        levels <- levels(data[[term]])[-1]  # Drop the first level (reference category)
        # For each level, create a dummy variable name
        for (level in levels) {
          term_raw <- c(term_raw, term)
          term_expanded <- c(term_expanded, paste0(term, level))
        }
      } else {
        # For non-factors (numeric), keep the term as is
        term_raw <- c(term_raw, term)
        term_expanded <- c(term_expanded, term)
      }
    }
  }

  # Create the resulting data frame
  result <- data.frame(term_raw = term_raw, term = term_expanded, stringsAsFactors = FALSE)
  return(result)
}

#convert to data frame for plotting
bma_to_df = function(x) {


  #methods for different classes, corresponding to different packages
  if (inherits(x, "coef.bas")) {
    y = tibble(
      term = x$namesx,
      term_nice = str_clean(term),
      PIP = x$probne0,
      mean = x$postmean,
      sd = x$postsd
    ) %>%
      #remove intercept
      slice(2:n())

  } else if (inherits(x, "bic.glm")) {

    #term map
    term_map = create_term_mapping(x$x, x$formula)

    y_terms = tibble(
      term_raw = names(x$probne0),
      PIP = x$probne0/100
    )

    #expand factors
    y_terms2 = full_join(
      y_terms,
      term_map,
      by = c("term_raw")
    )

    y_coefs = tibble(
      term = names(x$postmean)[-1],
      mean = x$postmean[-1],
      sd = x$postsd[-1]
    )

    #join coefs
    y = full_join(
      y_terms2,
      y_coefs,
      by = c("term" = "term")
    ) %>%
      mutate(
        term_nice = str_clean(term)
      )

  } else if (inherits(x, "bma")) {
    #get coefs
    x_coefs = coef(x)

    y = tibble(
      term = rownames(x_coefs),
      term_nice = term %>% str_clean(),
      PIP = x_coefs[, 1],
      mean = x_coefs[, 2],
      sd = x_coefs[, 3]
    )
  } else if (is.data.frame(x)) {
    #if data frame, assume it's the coefficients
    if (!all(c("term", "PIP", "mean", "sd") %in% names(x))) {
      stop("Data frame must have the columns: term, PIP, mean, sd", call. = F)
    }

    y = x

    #if no term_nice, make it
    if (!"term_nice" %in% names(y)) {
      y$term_nice = y$term %>% str_clean()
    }
  }
  else {
    #if bas object, remind user they need to use coef()
    if (inherits(x, "bas")) {
      stop("Please use `coef()` on the BAS model first", call. = F)
    }

    stop("Unknown input (not from package BMA, BAS, or BMS)", call. = F)
  }

  #change factor level order of term to PIP
  y %<>% mutate(
    term = fct_reorder(term, PIP),
    term_nice = fct_reorder(term_nice, PIP)
  )

  y
}

# BMA::bic.glm(Sepal.Length ~ ., data = iris, glm.family = "gaussian") %>% GG_BMA()

#PIP part
bma_pip_plot = function(x) {
  x %>%
    ggplot(aes(y = term_nice, x = PIP)) +
    geom_bar(stat = "identity") +
    scale_y_discrete("Term") +
    scale_x_continuous("PIP", limits = c(0, 1), breaks = seq(0, 1, by = .50), labels = scales::percent) +
    theme_bw()
}

#coefficients part
bma_coef_plot = function(x, confidence_level = .95) {
  x %>%
    ggplot(aes(y = term_nice, x = mean)) +
    geom_point() +
    geom_errorbarh(aes(xmin = mean - conf_interval_width(sd, confidence_level = confidence_level), xmax = mean + conf_interval_width(sd, confidence_level = confidence_level))) +
    #remove y-axis
    theme_update(
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    ) +
    theme_update(theme_bw()) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous("Coefficient")

}


#' Plot BMA results
#'
#' A ggplot2 function for plotting the output of BMA models, from the BMA, BAS, or BMS packages.
#'
#' @param x BMA model fit from BMA, BAS (call `coef()` first), or BMS
#' @param confidence_level Confidence level for the error bars. Default is 0.95.
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' #fit a model
#' fit = BAS::bas.lm(Sepal.Length ~ ., data = iris)
#' #get coefs
#' fit_coefs = coef(fit)
#' #plot
#' GG_BMA(fit_coefs)
#' GG_BMA(fit_coefs, confidence_level = .99)
GG_BMA = function(x, confidence_level = .95) {
  #convert to data frame
  x_dx = x %>%
    bma_to_df()

  #plot PIP and coefficients
  pip_plot = x_dx %>%
    bma_pip_plot()

  coef_plot = x_dx %>%
    bma_coef_plot(confidence_level = confidence_level)

  #combine them
  patchwork::wrap_plots(
    pip_plot,
    coef_plot,
    ncol = 2,
    widths = c(1, 3)
  )
}


#' Plot results from scale abbreviation
#'
#' Plots results from the output of `scale_abbreviation()`.
#'
#' @param x Output from `scale_abbreviation()`
#' @param vars Which variables to plot. Default is c("reliability_frac", "mean_criterion_cors_frac", "r_full_score", "reliability")
#' @param add_lines_for_full_model Add lines for the full model for comparison. Default is FALSE.
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' library(mirt)
#' #simulate some mirt data 2PL
#' set.seed(1)
#' dat = mirt::simdata(N = 1e3, itemtype = "2PL", a = runif(100, 0.5, 2), d = rnorm(100, sd = 0.5))
#' #fit the model
#' fit = mirt::mirt(dat, 1)
#' #scale abbreviation
#' short_scale = abbreviate_scale(as.data.frame(dat), method = "max_loading", item_target = 10)
#' #plot
#' GG_scale_abbreviation(short_scale)
GG_scale_abbreviation = function(x, vars = c("reliability_frac", "mean_criterion_cors_frac", "r_full_score", "reliability"), add_lines_for_full_model = F) {
  #determine method used for making the plot

  if (x$method %in% c("forwards", "backwards", "max_loading")) {
    p = x$best_sets %>%
      #display both reliability and criterion value
      pivot_longer(
        cols = all_of(vars),
        names_to = "criterion",
        values_to = "value"
      ) %>%
      ggplot(aes(x = items_in_scale, y = value, color = criterion)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = seq(0, 10000, 5)) +
      scale_y_continuous(limits = c(NA, 1)) +
      theme_minimal() +
      labs(
        x = "Number of items in scale",
        y = "Criterion value"
      )
  }

  if (x$method %in% c("genetic")) {
    max_gen = max(x$best_sets$generation)

    p = x$best_sets %>%
      #display both reliability and criterion value
      pivot_longer(
        cols = all_of(vars),
        names_to = "criterion",
        values_to = "value"
      ) %>%
      ggplot(aes(x = generation, y = value, color = criterion)) +
      geom_point() +
      geom_line() +
      scale_x_continuous() +
      scale_y_continuous(limits = c(NA, 1)) +
      theme_minimal() +
      labs(
        x = "Generation",
        y = "Criterion value"
      )
  }

  #add lines for full model
  if (add_lines_for_full_model) {
    browser()
    #which lines to add
    #not fractionalized
    vars_lines = setdiff(vars, c("reliability_frac", "mean_criterion_cors_frac", "r_full_score"))

    #collect lines
    d_lines = tibble(
      criterion = vars_lines,
      value = map_dbl(vars_lines, ~ x$full_fit_stats[[.x]])
    )

    p = p +
      geom_hline(data = d_lines,
                 mapping = aes(yintercept = value, color = criterion),
                 linetype = "dashed")
  }

  p
}


#' Plot ordinal proportions
#'
#' @param data A data frame with ordinal variables
#' @param clean_factor_levels Whether to clean the factor levels
#' @param order How to order the variables. Default is "positive" which sorts them by largest value. Alternatives are "negative", and "none" which leaves them in place.
#' @param reverse_factor_levels Whether to reverse the factor levels. Default is FALSE.
#' @param percentages Whether to plot percentages or counts. Default is TRUE.
#' @param add_values Whether to add values to the plot. Default is TRUE.
#' @param font_size Font size for the values. Default is 4.
#' @param exclude_values_below Exclude values below this value. Default is 0.
#' @param vars Which variables to plot. Default is all.
#' @param group Grouping variable. Default is NULL.
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' xx = tibble(
#' ord_1 = cut(rnorm(200), breaks = c(-Inf, -1, 0, 1, Inf), labels = c("A", "B", "C", "D")),
#' ord_2 = cut(rnorm(200, mean = 1), breaks = c(-Inf, -1, 0, 1, Inf), labels = c("A", "B", "C", "D")),
#' ord_3 = cut(rnorm(200, mean = -1), breaks = c(-Inf, -1, 0, 1, Inf), labels = c("A", "B", "C", "D")),
#' ord_4 = cut(rnorm(200, mean = 0), breaks = c(-Inf, -1, 0, 1, Inf), labels = c("A", "B", "C", "D"))
#' )
#' GG_ordinal(xx)
#' GG_ordinal(xx, order = "negative")
#' GG_ordinal(xx, order = "none")
#'
#' #long form
#' xx %>% pivot_longer(everything()) %>% GG_ordinal(vars = "value", group = "name")
GG_ordinal = function(
    data,
    vars = NULL,
    group = NULL,
    clean_factor_levels = T,
    order = "positive",
    reverse_factor_levels = F,
    percentages = T,
    add_values = T,
    font_size = 4,
    exclude_values_below = 0
) {

  #if data given, and not vars, then use all vars
  if (is.null(vars)) {
    vars = names(data)
  }

  #if group is given, then things work differently
  if (is.null(group)) {
    #ensure data are ordinals
    #and have the same levels
    data_levels = map(data, levels)
    if (!all_the_same(data_levels)) {
      stop("All variables must have the same levels", call. = F)
    }
    data = map_df(data, as.ordered)

    #convert to long form
    data_long = data %>%
      mutate(
        id = 1:n()
      ) %>%
      pivot_longer(cols = -id) %>%
      mutate(
        value_num = value %>% as.numeric(value)
      )

    #save name levels
    name_levels = vars
  } else {
    #convert to the same long format as without groups
    #only 1 var can be chosen, at least for now
    assertthat::assert_that(is_character(vars, 1))

    #subset
    data_long = data %>%
      mutate(
        id = row_number(),
        value = !!sym(vars),
        value_num = value %>% as.numeric(value),
        name = !!sym(group) %>% as.factor()
      ) %>%
      select(id, name, value, value_num)

    #save data levels
    data_levels = map(data[vars], levels)

    #save name levels
    name_levels = levels(data_long$name)
    # browser()
  }

  #reverse factor levels
  if (reverse_factor_levels) {
    data_levels[[1]] = data_levels[[1]] %>% base::rev()
  }

  #calculate proportions
  data_props = data_long %>%
    group_by(name) %>%
    reframe(table2(value, include_NA = F))

  #set ordinal levels
  data_props$Group = factor(data_props$Group, levels = data_levels[[1]])
# browser()
  #how to sort the variables
  if (order == "positive") {
    data_long %<>% mutate(
      name_ordered = name %>% fct_reorder(value_num, .fun = mean, .na_rm = T)
    )
  } else if (order == "negative") {
    data_long %<>% mutate(
      name_ordered = name %>% fct_reorder(-value_num, .fun = mean, .na_rm = T)
    )
  } else if (order == "none") {

    data_long %<>% mutate(
      name_ordered = name %>% factor(levels = name_levels)
    )
  }

  #sort proportions the same way
  data_props$name = ordered(data_props$name, levels = levels(data_long$name_ordered))

  #clean factor levels
  if (clean_factor_levels) {
    data_props$name = data_props$name %>% fct_relabel(str_clean)
  }


  #use percentages or counts
  if (percentages) {
    data_props$value = data_props$Percent/100
  } else {
    data_props$value = data_props$Count
  }

  #plot
  p = data_props %>%
    ggplot(aes(x = name, y = value, fill = Group)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_bw() +
    scale_fill_brewer(palette = "Paired")  # Optional: Improves color distinction

  #add values
  if (add_values) {
    #exclude some values maybe
    data_props_filtered = data_props %>%
      mutate(
        alpha = if_else(value < exclude_values_below, 0, 1) %>% as.numeric()
      )

    if (percentages) {
      p = p + geom_text(data = data_props_filtered, aes(label = paste0(round(Percent, 0), "%"), alpha = alpha), position = position_stack(vjust = 0.5), size = font_size) +
        scale_alpha_identity(guide = 'none')
    } else {
      p = p + geom_text(data = data_props_filtered, aes(label = Count, alpha = alpha), position = position_stack(vjust = 0.5), size = font_size) +
        scale_alpha_identity(guide = 'none')
    }
  }

  #appropriate label and scales
  if (percentages) {
    p = p + scale_y_continuous("Percentage", labels = scales::percent)
  } else {
    p = p + scale_y_continuous("Count")
  }

  p
}

#based on https://r-graph-gallery.com/web-line-chart-with-labels-at-end-of-line.html

#' Plot lines with labels
#'
#' @param data A data frame with x, y, and color variables
#' @param x A string with the x variable name
#' @param y A string with the y variable name
#' @param color A string with the color variable name
#' @param right_margin Right margin for the plot. Default is 100.
#'
#' @returns A ggplot2 object
#' @export
#'
#' @examples
#' tidyr::population %>%
#' filter(country %in% (.env$population$country %>% unique() %>% str_subset(pattern = "^A") %>% head(10))) %>%
#' GG_lines("year", "population", "country") +
#' scale_y_log10()
GG_lines = function(data, x, y, color, right_margin = 100) {
  # browser()

  #use strings as symbols
  x_sym = rlang::ensym(x)
  y_sym = rlang::ensym(y)
  color_sym = rlang::ensym(color)

  #subset to last value for each color group
  data_last = data %>%
    group_by({{ color_sym }}) %>%
    slice_tail(n = 1) %>%
    ungroup()

  #plot
  p = data %>%
    ggplot(aes(x = {{ x_sym }}, y = {{ y_sym }}, color = {{ color_sym }})) +
    geom_line() +
    geom_point() +
    theme_bw() +
    theme(plot.margin = margin(t = 5.5, r = right_margin, b = 5.5, l = 5.5, unit = "pt")) +
    ggrepel::geom_text_repel(data = data_last, aes(label = {{ color_sym }}), size = 3, show.legend = F, xlim = c(data_last[[y]] %>% max(na.rm = T), NA), segment.linetype = "dotted") +
    #remove color guide
    guides(color = "none") +
    coord_cartesian(clip = "off")

  p
}





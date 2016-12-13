#' Dataset merger function
#'
#' This function allows you to merge two data.frames by their overlapping rownames.
#' @param DF1 the first data.frame
#' @param DF2 the second data.frame
#' @param main which data.frame should be used as the main? Choose the larger one if working with large datasets. Default to using neither.
#' @param join (character scalar) Which data.frames to use cases from. Defaults to "both". Can be: both, left, right.
#' @export
#' @examples
#' merge_datasets()
merge_datasets = function (DF1, DF2, main=1, time=F, join = "both"){
  #time if desired
  if (time) {time1 = proc.time()} #start timer


  #checks
  if (!main %in% 0:2){ #check for valid input
    stop("Invalid input to main parameter provided!")
  }
  if (!join %in% c("both", "left", "right")) stop("Invalid join parameter!")


  #main setting decides how to combine
  if (join == "left") {
    DF2 = DF2[intersect(rownames(DF1), rownames(DF2)), , drop = F] #subset to overlap with DF1
  }
  if (join == "right") {
    DF1 = DF1[intersect(rownames(DF1), rownames(DF2)), , drop = F] #subset to overlap with DF2
  }

  #if nothing to join
  if (nrow(DF1) == 0) {
    message("Warning, nothing joined! No case in DF1 matches any in DF2!")
    return(DF2)
  }
  if (nrow(DF2) == 0) {
    message("Warning, nothing joined! No case in DF2 matches any in DF1!")
    return(DF1)
  }

  #combined dataset
  if (main==0){ #create a combined dataset
    #colnames, remove duplicates
    total.colnames = c(colnames(DF1), colnames(DF2))
    total.colnames.unique = unique(total.colnames)

    #rownames, remove duplicates
    total.rownames = c(rownames(DF1), rownames(DF2))
    total.rownames.unique = unique(total.rownames)

    #make DF3
    DF3 = as.data.frame(matrix(nrow = length(total.rownames.unique),
                               ncol = length(total.colnames.unique)))
    rownames(DF3) = sort(total.rownames.unique)
    colnames(DF3) = total.colnames.unique
  }
  if (main==1){ #use first DF as main
    DF3 = DF1
  }
  if (main==2){ #use second DF as main
    DF3 = DF2
  }

  if (main!=2){
    #loop over input dataset 2
    for (variable in 1:length(colnames(DF2))){ #loop over variables/cols
      for (case in 1:length(rownames(DF2))){ #loop over cases/rows
        if (is.na(DF2[case, variable])){ #skip if datapoint is missing
          next
        }
        DF3[rownames(DF2)[case], colnames(DF2)[variable]] = DF2[case, variable]
        #print(DF3[rownames(DF2)[case], colnames(DF2)[variable]]) #used for debugging
      }
    }
  }
  if (main!=1){ #if DF2 is main
    #loop over input dataset 1
    for (variable in 1:length(colnames(DF1))){ #loop over variables/cols
      for (case in 1:length(rownames(DF1))){ #loop over cases/rows
        if (is.na(DF1[case, variable])){ #skip if datapoint is missing
          next
        }
        DF3[rownames(DF1)[case], colnames(DF1)[variable]] = DF1[case, variable]
        #print(DF3[rownames(DF1)[case], colnames(DF1)[variable]]) #used for debugging
      }
    }
  }

  #output time
  if (time) {
    time2 = proc.time() - time1 #end timer
    message(time2) #print time
  }

  return(DF3)
}


#' Dataset merger function for multiple data.frames.
#'
#' This is a wrapper for merge_datasets().
#' @param ... (data.frames) Two or more data.frames to merge.
#' @keywords merging, combining, datasets, data.frame, multi, wrapper
#' @export
#' @examples
#' merge_datasets_multi(iris[1:50, ], iris[51:100, ], iris[101:150, ]) #merge three-part iris
merge_datasets_multi = function(...) {
  #wrap with Reduce
  Reduce(function(x, y) merge_datasets(x, y), list(...))
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
  if (reverse) {method_text = "Jensen's method with reversing\n"} else {method_text = "Jensen's method without reversing\n"}

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
  text_object = grid::grobTree(grid::textGrob(text, x = x,  y = y, hjust = hjust, vjust = vjust),
                               gp = grid::gpar(fontsize = 11))

  #regression line
  model = lm(cors ~ loadings, DF)
  coefs = coef(model)

  #plot
  DF$rnames = rownames(DF)

  g = ggplot2::ggplot(data = DF, aes(x = loadings, y = cors)) +
    geom_point() +
    xlab("Loadings") +
    ylab("Correlation with criteria variable") +
    annotation_custom(text_object) +
    geom_abline(intercept = coefs[1], slope = coefs[2], color = "darkorange")

  #add var_names if desired
  if (var_names) g = g + geom_text(aes(label = rnames), alpha = .7, size = 3, vjust = 1.5, check_overlap = check_overlap)

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
  if (reverse) {method_text = "Jensen's method with reversing\n"} else {method_text = "Jensen's method without reversing\n"}

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
  text_object = grid::grobTree(grid::textGrob(text, x = x,  y = y, hjust = hjust, vjust = vjust),
                               gp = grid::gpar(fontsize = 11))

  #regression line
  model = lm(cors ~ loadings, DF)
  coefs = coef(model)

  #plot
  DF$rnames = rownames(DF)

  g = ggplot2::ggplot(data = DF, aes(x = loadings, y = cors)) +
    geom_point() +
    xlab("Loadings") +
    ylab("Correlation with criteria variable") +
    annotation_custom(text_object) +
    geom_abline(intercept = coefs[1], slope = coefs[2], color = "darkorange")

  #add var_names if desired
  if (var_names) g = g + geom_text(aes(label = rnames), alpha = .7, size = 3, vjust = 1.5, check_overlap = check_overlap)

  return(g)
}


# Correlates all variables, finds the pair with the highest correlation, and removes one of them using the specified method.
#' Remove the n most redundant variables from a data.frame.
#'
#' Removes the n top variables that highly correlated with another variable so as to avoid problems in analysis.
#' @param df a data.frame.
#' @param num.to.remove the number of variables to remove.
#' @param remove.method the method to use to remove variables. Methods are "c", "l", "r", "f" and "s": conversative, liberal, random, first or second.
#' @export
remove_redundant_vars = function(df, num.to.remove = 1, remove.method = "s") {
  if (!is.data.frame(df)) {
    stop(paste0("First parameter is not a data frame. Instead it is ", class(df)))
  }
  if (!is.numeric(num.to.remove)) {
    stop(paste0("Second parameter is not numeric. Instead is ", class(num.to.remove)))
  }
  remove.method.1 = substr(remove.method, 1,1) #get first char
  if (!remove.method %in% c("f", "s", "r")) { #conversative, liberal or random, first or second
    stop(paste0("Third parameter was neither identifable as first, second or random. It was: ", remove.method))
  }

  old.names = colnames(df) #save old variable names

  for (drop.num in 1:num.to.remove) {
    message(paste0("Dropping variable number ", drop.num))
    names = colnames(df) #current names

    #correlations
    cors = as.data.frame(cor(df, use="pair"))

    #remove diagnonal 1's
    for (idx in 1:nrow(cors)) {
      cors[idx, idx] = NA
    }

    #absolute values because we don't care if cor is .99 or -.99
    cors.abs = abs(cors)

    #dropping
    max.idx = which_max2(cors.abs) #indexes of max value (first one if multiple identical)

    topvars = paste(rownames(cors)[max.idx[2]], "and", rownames(cors)[max.idx[1]]) #names of top correlated variables
    r = round(cors[max.idx[1], max.idx[2]], 3)
    message(paste0("Most correlated vars are ", topvars, " r=", r)) #info

    #first
    if (remove.method.1 == "f") {
      df[, max.idx[2]] = NULL #remove the second var
    }
    #second
    if (remove.method.1 == "s") {
      df[, max.idx[1]] = NULL #remove the second var
    }
    #random
    if (remove.method.1 == "r") {
      if (rnorm(1) > 0){
        df[, max.idx[1]] = NULL #remove the second var
      }
      else {
        df[, max.idx[2]] = NULL #remove the first var
      }

    }
  }
  #Which variables were dropped?
  new.names = colnames(df)
  dropped.names = setdiff(old.names, new.names)
  message("Dropped the following variables:")
  message(dropped.names)

  #return reduced df
  return(df)
}

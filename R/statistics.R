## STATISTICS functions


#' Wrapper for rcorr()
#'
#' A wrapper for rcorr() that takes data.frames or matrices as opposed to only matrices.
#' @param x a matrix or data.frame
#' @keywords wrapper correlation matrix
#' @export
#' @examples
#' rcorr2()
rcorr2 = function(x, ...) {
  library(Hmisc) #for rcorr()
  x = as.matrix(x)
  x = rcorr(x, ...)
  return(x)
}

#' Jensen method (method of correlated vectors) plot
#'
#' Returns a ggplot2 scatter plot with numerical results in a corner. Also supports reversing for dealing with factors that have negative indicators.
#' @param loadings a vector of factor loadings.
#' @param loadings a vector of correlations of the indicators with the criteria variable.
#' @param reverse whether to reverse indicators with negative loadings. Default to true.
#' @param text.location which corner to write the numerical results in. Options are "tl", "tr", "bl", "br". Defaults to "tl".
#' @keywords psychometrics psychology latent variable
#' @export
#' @examples
#' Jensen_plot()
Jensen_plot = function(loadings, cors, reverse = TRUE, text.location = "tl"){
  temp.loadings = as.numeric(loadings) #conver to vector
  names(temp.loadings) = rownames(loadings) #set names again
  loadings = temp.loadings #back to normal name
  DF = data.frame(loadings, cors) #DF
  
  #reverse
  if (reverse) {
    for (idx in 1:nrow(DF)) {
      if (DF[idx,1] < 0){ #if loading <0
        DF[idx,] = DF[idx,]*-1 #reverse
        rownames(DF)[idx] = paste0(rownames(DF)[idx],"_r")
      }
    }
  }
  
  #method text
  if (reverse) {mcv.method = "Jensen method with reversing\n"}
  else {mcv.method = "Jensen method without reversing\n"}
  
  #correlation
  cor = round(cor(DF)[1,2],2) #get correlation, rounded
  
  #make title text
  if (!is.null(main)) {
    if (is.na(main)) {
      main = paste0(mcv.method,nrow(DF)," indicators, r=",cor)
    }
  }
  
  #text object location
  if (text.location=="tl") {
    x = .02
    y = .98
    hjust = 0
    vjust = 1
  }
  if (text.location=="tr") {
    x = .98
    y = .98
    hjust = 1
    vjust = 1
  }
  if (text.location=="bl") {
    x = .02
    y = .02
    hjust = 0
    vjust = -.1
  }
  if (text.location=="br") {
    x = .98
    y = .02
    hjust = 1
    vjust = -.1
  }
  
  #text
  text = paste0(mcv.method,
                "r=",cor, " (orange line)",
                "\nn=",nrow(DF))
  
  #text object
  text_object = grobTree(textGrob(text, x=x,  y=y, hjust = hjust, vjust = vjust),
                         gp=gpar(fontsize=11))
  
  #regression line
  model = lm(cors ~ loadings, DF)
  coefs = coef(model)
  
  #plot
  DF$rnames = rownames(DF)
  
  g = ggplot(data=DF, aes(x=loadings, y=cors)) +
    geom_point() +
    geom_text(aes(label=rnames), alpha=.7, size=3, vjust=1) +
    #geom_smooth(method=lm, se=FALSE, color="darkorange") + #this sometimes fails (?)
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(main) +
    annotation_custom(text_object) +
    geom_abline(intercept=coefs[1], slope=coefs[2], color="darkorange")

  return(g)
}


#' Plot factor loadings.
#'
#' Returns a ggplot2 plot with sorted loadings and numerical results in a corner. Supports reversing of the factor is reversed.
#' @param fa.object a factor analysis object from the fa() function from the psych package.
#' @param reverse whether to reverse all loadings. Default to false.
#' @param text.location which corner to write the numerical results in. Options are "tl", "tr", "bl", "br". Defaults to "tl".
#' @keywords psychometrics, psychology, latent variable, factor analysis, plot, ggplot2
#' @export
#' @examples
#' plot_loadings()
plot_loadings = function(fa.object, reverse = F, text.location = "tl") {
  library("plotflow") #needed for reordering the variables
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
  if (text.location=="tl") {
    x = .02
    y = .98
    hjust = 0
    vjust = 1
  }
  if (text.location=="tr") {
    x = .98
    y = .98
    hjust = 1
    vjust = 1
  }
  if (text.location=="bl") {
    x = .02
    y = .02
    hjust = 0
    vjust = -.1
  }
  if (text.location=="br") {
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
#' @param fa.objects a list of factor analyses objects from the fa() function from the psych package.
#' @param fa.labels a character vector for names of the analyses. Defaults to fa.1, fa.2, etc..
#' @param reverse.vector a numeric vector to multiple factor loadings with. Use e.g. c(1, -1) to reverse the second factor. Defaults not reversing.
#' @keywords psychometrics, psychology, latent variable, factor analysis, plot, ggplot2
#' @export
#' @examples
#' plot_loadings_multi()
plot_loadings_multi = function(fa.objects, fa.labels = NA, reverse.vector = NA) {
  #dependecy
  library("plotflow")

  fa.num = length(fa.objects) #number of fas
  fa.names = str_c("fa.", 1:fa.num)
  
  #check if fa.objects is a list
  if (!is.list(fa.objects)) {
    stop("fa.objects parameter is not a list.")
  }
  
  #check if there is no list (i.e. if user just gave one fa object)
  if (class(fa.objects) %in% c("psych", "fa")) {
    fa.objects = list(fa.objects)
    #must rerun these
    fa.num = length(fa.objects) #number of fas
    fa.names = str_c("fa.", 1:fa.num)
  }
  
  #check if labels have the right length
  if (length(fa.labels) == 1) {
      if (is.na(fa.labels)) {
          fa.labels = fa.names
      }
  } else if (length(fa.labels) != fa.num) {
    stop("Factor analysis labels length is not identical to number of analyses.")
  }
  
  #make reverse vector if not given
  if (is.na(all(reverse.vector))) {
    reverse.vector = rep(1, fa.num)
  } else if (length(reverse.vector) != fa.num) {
    stop("Length of reversing vector does not match number of factor analyses.")
  }
  
  #merge into df
  d = data.frame() #to merge into
  for (fa.idx in 1:fa.num) { #loop over fa objects
    loads = fa.objects[[fa.idx]]$loadings*reverse.vector[fa.idx]
    rnames = rownames(loads)
    loads = as.data.frame(as.vector(loads))
    rownames(loads) = rnames
    colnames(loads) = fa.names[fa.idx]
    
    d = merge_datasets(d, loads, 1)
  }
  
  #reshape to long form
  d2 = reshape(d,
               varying = 1:fa.num,
               direction="long",
               ids = rownames(d))
  d2$time = as.factor(d2$time)
  d2$id = as.factor(d2$id)
  colnames(d2)[2] = "fa"
  
  #plot
  g = ggplot(reorder_by(id, ~ fa, d2), aes(x=id, y=fa, color=time, group=time)) +
    geom_point(position=position_dodge(width = .5)) +
    xlab("Loading") + ylab("Indicator") +
    scale_color_discrete(name="Analysis",
                         labels=fa.labels) +
    coord_flip()
  
  return(g)
}



# Correlates all variables, finds the pair with the highest correlation, and removes one of them using the specified method.
#' Remove the n most redundant variables from a data.frame. 
#'
#' Removes the n top variables that highly correlated with another variable so as to avoid problems in analysis.
#' @param df a data.frame.
#' @param num.to.remove the number of variables to remove.
#' @param remove.method the method to use to remove variables. Methods are "c", "l", "r", "f" and "s": conversative, liberal, random, first or second.
#' @keywords psychometrics, psychology, latent variable, factor analysis, redundant
#' @export
#' @examples
#' remove_redundant_vars()
remove_redundant_vars = function(df, num.to.remove = 1, remove.method = "s") {
  if (!is.data.frame(df)) {
    warning(paste0("First parameter is not a data frame. Instead it is ", class(df)))
  }
  if (!is.numeric(num.to.remove)) {
    stop(paste0("Second parameter is not numeric. Instead is ", class(num.to.remove)))
  }
  remove.method.1 = substr(remove.method, 1,1) #get first char
  if (!remove.method %in% c("c","l", "r", "f", "s")) { #conversative, liberal or random, first or second
  stop(paste0("Third parameter was neither identifable as conversative, liberal or random. It was: ", remove.method))
  }
  
  old.names = colnames(df) #save old variable names
  
  for (drop.num in 1:num.to.remove) {
  print(paste0("Dropping variable number ",drop.num))
  names = colnames(df) #current names
  
    #correlations
    cors = as.data.frame(cor(df, use="pair")) #correlations
    #remove diagnonal 0's
    for (idx in 1:nrow(cors)) {
      cors[idx,idx] = NA 
    }
    
    #dropping
    max.idx = which_max2(cors) #indexes of max value (first one if multiple identical)
    topvars = paste(rownames(cors)[max.idx[1]], "and", rownames(cors)[max.idx[2]]) #names of top correlated variables
    r = round(cors[max.idx[1],max.idx[2]],3)
    print(paste0("Most correlated vars are ", topvars, " r=", r)) #print info
    
    #first
    if (remove.method.1=="f") {
      df[,max.idx[2]] = NULL #remove the second var
    }
    #second
    if (remove.method.1=="s") {
      df[,max.idx[1]] = NULL #remove the second var
    }
    #random
    if (remove.method.1=="r") {
      if (rnorm(1)>0){
        df[,max.idx[1]] = NULL #remove the second var
      }
      else {
        df[,max.idx[2]] = NULL #remove the first var
      }
      
    }
  }
  #Which variables were dropped?
  new.names = colnames(df)
  dropped.names = setdiff(old.names, new.names)
  print("Dropped the following variables:")
  print(dropped.names)
  
  #return reduced df
  return(df)
}


#' Find residuals on case-level basis for all indicators in a factor analysis.
#'
#' Extracts the first factor, then uses the factor scores to predict the indicator values for each indicator and for each case. Returns a data.frame with residuals.
#' @param data a data.frame.
#' @keywords psychometrics, psychology, latent variable, factor analysis, residuals
#' @export
#' @examples
#' FA_residuals()
FA_residuals = function(data) {
  library(stringr)
  #initial
  data = as.data.frame(scale(data)) #standardize
  fa = fa(data) #factor analyze
  factor.scores = as.vector(fa$scores) #get scores
  data2 = data; data2$factor.scores = factor.scores #insert scores into copy
  resids.table = data.frame(matrix(nrow=nrow(data), ncol=ncol(data))) #make df for resids
  colnames(resids.table) = colnames(data) #set names
  rownames(resids.table) = rownames(data) #
  
  #for each indicator
  for (indicator in colnames(data)) {
    formula = str_c(indicator," ~ factor.scores") #the regression formula as string
    model = lm(formula, data2) #regress
    resids = residuals(model) #extract residuals for this indicator
    resids.table[,indicator] = resids #set into resids df
  }
  return(resids.table) #return resids
}

#' Calculates the mean absolute residual at the case-level.
#'
#' Extracts the first factor, then uses the factor scores to predict the indicator values for each indicator and for each case. Returns a data.frame with residuals. Then calculates the mean absolute value of these by case.
#' @param data a data.frame.
#' @param sort whether to sort the results or not. Defaults to true.
#' @keywords psychometrics, psychology, latent variable, factor analysis, residuals
#' @export
#' @examples
#' FA_MAR()
FA_MAR = function(data, sort = T) {
  resids = FA_residuals(data)
  
  #mean absolute residuals
  mean.abs.resid = apply(resids, 1, function(x) {
      return(mean(abs(x)))
    }
    )
  
  #sort?
  if (sort) {
    mean.abs.resid = sort(mean.abs.resid, decreasing=T)
  }
  
  #return
  return(mean.abs.resid)
}

## For finding problematic cases in FA
# Runs FA on a dataset without a case, for each case. Returns var% for subset analyses and difference to all cases.
#' Calculates the effect of removing a case on size of the first factor, for each case.
#'
#' Returns a numerical vector of the change in the size of the first factor by removing that case.
#' @param data a data.frame.
#' @param sort whether to sort the results or not. Defaults to true.
#' @keywords psychometrics, psychology, latent variable, factor analysis
#' @export
#' @examples
#' FA_CFS()
FA_CFS = function(data, sort = T) {
  #initial
  prop.vars = as.data.frame(matrix(nrow=nrow(data)+1, ncol=2)) #for results
  colnames(prop.vars) = c("Prop.var%", "IPV")
  
  #all cases
  fa = fa(data) #factor analyze
  prop.var = round(mean(fa$communality),3) #the proportion of variance accounted for
  prop.vars[nrow(prop.vars),] = c(prop.var, 0) #insert
  rownames(prop.vars)[[nrow(prop.vars)]] = "All cases"
  
  #for each case
  for (case in 1:nrow(data)) {
    data2 = data[-case,] #get subset without that case
    tryCatch({
      fa = fa(data2) #factor analyze
      prop.var = round(mean(fa$communality),3) #the proportion of variance accounted for
      prop.var.delta = prop.var - prop.vars["All cases",1] #difference to row "all cases"
      prop.vars[case,] = c(prop.var, prop.var.delta) #insert
    },
    error = function(e) {
      prop.vars[case,] = c(NA, NA) #insert NAs
    }
    )
    
    rownames(prop.vars)[case] = rownames(data)[case] #set rowname
  }
  
  #sort?
  if (sort) {
    prop.vars = prop.vars[order(prop.vars[,2], decreasing=T),]
  }
  
  return(prop.vars)
}


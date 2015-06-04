## VARIOUS MODELING FUNCTIONS

#' Matrix of beta coefficients of all simple linear models. 
#'
#' Returns a data.frame with beta coefficients for all possible simple (without interactions) linear models given a set of predictor variables and a dependent variable.
#' @param dependent a string with the name of the dependent variable.
#' @param predictors a vector of strings with the predictor variables.
#' @param data a data.frame with the variables.
#' @param standardized whether to standardize the results. Defaults to true.
#' @param .weights weights to use. Leave blank not to use weights.
#' @keywords modeling lm linear automatic
#' @export
#' @examples
#' lm_beta_matrix()
lm_beta_matrix = function(dependent, predictors, data, standardized=TRUE, .weights=NA) {
  #find all the combinations
  library(gtools) #for combinations()
  library("QuantPsyc") #for lm.beta()
  library(stringr) #for str_c()
  num.inde = length(predictors) #how many indeps?
  num.cases = nrow(data) #how many cases?
  
  sets = list() #list of all combinations
  for (num.choose in 1:num.inde) { #loop over numbers of variables to choose
    temp.sets = combinations(num.inde,num.choose) #all combinations of picking r out of n
    temp.sets = split(temp.sets, seq.int(nrow(temp.sets))) #as a list
    sets = c(sets,temp.sets)
  }
  #return(sets) #for debugging
  
  #create all the models
  models = numeric() #empty vector for models
  for (set in sets) { #loop over each possible combination
    deps = predictors[set] #fetch predictor names
    deps = paste0(deps, collapse = " + ") #add " + " between predictors
    model = paste0(dependent, " ~ ", deps) #join up
    models = c(models,model) #add to vector
  }
  #print(models) #debug
  
  #fix weights
  if (length(.weights) == 1) {
    if (is.na(.weights)) { #if it is NA, insert weights = 1 in all cases (same as unweighted)
      .weights = rep(1, num.cases)
    }
  } else if (length(.weights) != num.cases) { #if number of weights is mismatched to cases
    stop("Number of .weights is not the same as number of cases!")
  }
  
  data$.weights = .weights
  
  #run each model
  betas = data.frame(matrix(ncol = num.inde+1, nrow = length(models))) #DF for betas
  #number of cols is the predictors +1 because the last is R2 adj.
  colnames(betas) = c(predictors, "r2.adj.") #colnames
  for (model.idx in 1:length(models)) { #loop over the index of each model
    #progress
    print(str_c("Model ", model.idx, " of ", length(models)))
          
    #fit model and extract betas
    lm.fit = lm(models[model.idx], data, weights = .weights) #fit the model
    if (standardized==FALSE) {
      lm.fit.betas = lm.fit$coefficients[-1] #get betas, remove intercept
    }
    if (standardized==TRUE) {
      lm.fit.betas = lm.beta(lm.fit)[-length(lm.beta(lm.fit))] #get betas, standardized
    }
    #print(lm.fit.betas)
    
    #insert data
    for (beta.idx in 1:length(lm.fit.betas)) { #loop over each beta
      beta.names = as.vector(names(lm.fit.betas)) #get the names
      beta.name = beta.names[beta.idx] #get the name
      #print(beta.names)
      #print(beta.name)
      betas[model.idx,beta.name] = lm.fit.betas[beta.idx] #insert beta in the right place
    }
    
    #extract r2 adj.
    r.sq = summary(lm.fit)$adj.r.squared
    
    #insert r2 adj.
    betas[model.idx,"r2.adj."] = r.sq
  }
  return(betas)
}


#Thanks to: https://stat.ethz.ch/pipermail/r-help/2011-October/293842.html
#' Residualized data.frame.
#'
#' Returns a residualized data.frame given a set of variables to partial out.
#' @param data a data.frame or matrix.
#' @param resid.vars a character vector of the variables to partial out.
#' @param exclude.resid.varss whether to residualize the residualize variables. Defaults to true.
#' @param return.resid.vars whether to include the residualize variables in the returned data.frame. Defaults to true.
#' @param print.models whether to print the lm models used in the process. Defaults to true.
#' @keywords modeling, residualize, partialing
#' @export
#' @examples
#' residualize_DF()
residualize_DF = function(data, resid.vars, suffix = "", exclude.resid.varss = T, return.resid.vars = T, print.models = T) {
  #the residuals function
  lm_f = function(x) {
    x = residuals(lm(data=data, formula= update(x ~ 0, paste0("~",resid.vars))))
  }
  
  #calculate residuals
  if (exclude.resid.varss) {
    resid = data.frame(matrix(nrow=nrow(data),ncol=ncol(data))) #make empty df same size as data
    colnames(resid) = colnames(data) #get colnames
    
    for (colname in colnames(data)) { #loop over colnames
      if (!colname %in% resid.vars) { #if colname ISNT an indepednet, get residuals
        f = str_c(colname, " ~ ", paste0(resid.vars))
        if (print.models) print(f)
        
        resid[,colname] = residuals(lm(data = data, formula = f))
      }
      if (colname %in% resid.vars) { #if colname IS an indepedent, get originals
        resid[,colname] = data[,colname]
      }
    }
  }
    else {
    resid = data.frame(apply(data,2,lm_f)) #get residuals from everything including independents
  }
  
  #this adds the suffix, if desired
  colnames(resid) = paste0(colnames(data),suffix)
  
  #remove resid vars if desired
  if (return.resid.vars==F) {
    for (resid.vars in resid.vars) {
      resid[,resid.vars] = NULL
    }
  }
  
  return(resid)
}

#' Convenient summary of a lm() model with confidence intervals.
#'
#' Returns beta coefficients and confidence intervals from a fitted lm() model.
#' @param fitted.model the fitted model to summarize.
#' @param level the level of confidence to use. Defaults to .95 (95%).
#' @param round at which digit to round the numbers. Defaults to 2.
#' @keywords modeling, summary, model
#' @export
#' @examples
#' lm_CI()
lm_CI = function(fitted.model, level = .95, round = 2) {
  sum.model = summary(fitted.model) #summary
  df = df = sum.model$df[2] #degrees of freedom
  coefs = sum.model$coef[-1,1:2, drop=F] #coefs without intercept
  coefs = as.data.frame(coefs) #conver to dataframe
  colnames(coefs) = c("Beta", "SE") #rename
  multiplier = qt(1-((1-level)/2),df) #to calculate the CIs
  coefs$CI.lower = coefs[,1] - multiplier*coefs[,2] #lower
  coefs$CI.upper = coefs[,1] + multiplier*coefs[,2] #upper
  coefs = round(coefs, round) #round to desired digit
  return(coefs)
}


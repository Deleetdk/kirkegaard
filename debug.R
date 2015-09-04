semi_par_serial = function(df, dependent, primary, secondaries, weights=NA, standardize=T) {
  library(weights) #for weighted correlations
  library(stringr) #strings

  #subset and deal with lack of weights
  if (is.na(weights)) {
    weights = "weights_var"
    df[weights] = rep(1, nrow(df))
    df = subset(df, select = c(dependent, primary, secondaries, weights))
  } else {
    df["weights_var"] = df[weights] #move the weights var to another name
    weights = "weights_var"
    df = subset(df, select = c(dependent, primary, secondaries, weights))
  }

  #complete cases only
  df = na.omit(df)

  #standardize
  if (standardize) df = std_df(df, exclude = weights)

  #primary
  r_prim = round(wtd.cor(df[, dependent], df[, primary], weight = df[, weights]), 2)
  message(str_c("Correlation of the primary variable with the dependent is ", r_prim[1]))

  #make results object
  results = data.frame(matrix(nrow = length(secondaries), ncol = 2))
  rownames(results) = secondaries
  colnames(results) = c("Orig. cor", "Semi-partial cor")
  #loop over each secondary
  for (sec_idx in seq_along(secondaries)) {

    #the current secondary var
    tmp_secondary = secondaries[sec_idx]

    #make the model
    tmp_model = str_c(dependent, " ~ ", primary)

    #regress
    df$tmp_resids = resid(lm(as.formula(tmp_model), weights = df[, weights], data = df))

    #secondary original
    r_sec = wtd.cor(df[, dependent], df[, tmp_secondary], weight = df[, weights])[1]

    #semi-partial
    r_sec_sp = wtd.cor(df$tmp_resids, df[, tmp_secondary], weight = df[, weights])[1]

    #save
    results[sec_idx, ] = c(r_sec, r_sec_sp)
  }

  return(results)
}

t = semi_par_serial(df = airquality, dependent = "Ozone", primary = "Solar.R", secondaries = colnames(airquality)[3:6])

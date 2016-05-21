### This has some functions related to spatial autocorrelation analysis


#' Calculate pairwise spherical distances for all pairs.
#'
#' Returns a vector with the pairwise spherical distances for all pairs. The calculation method used is "great circle method" using distCosine() from package geosphere.
#' @param df A data.frame with variables.
#' @param lat_var A numeric vector with the latitudes.
#' @param lon_var A numeric vector with the longitudes.
#' @param output A character vector stating which output form is desired. Can be either vector or matrix. Vector results in a vector of all the unique combinations of distances, which is n*(n-1)/2, where n is the number of cases. Matrix results in the full distance matrix with every permutation including diagonals, which are always 0.
#' @keywords spatial autocorrelation, latitude, longitude, distance
#' @export
get_spherical_dists = function(df, lat_var = "lat", lon_var = "lon", output = "vector", remove_diag = T) {
  #assumes that the latitude and longitude vars are called lat and lon, otherwise set their names

  #missing data
  if (any(is.na(df))) warning("Warning, data.frame contained cases with missing values which were excluded!")
  df = na.omit(df)

  #loads the library needed to calculate distances
  library(geosphere)

  #loads combinatorics library
  library(gtools)

  #subset
  df = df[c(lat_var, lon_var)]

  #desired output type
  output = agrep(output, c("vector", "matrix"), value = T) #fuzzy matching
  if (length(output) == 0) stop("Output parameter not recognized!")

  #vector output
  if (output == "vector") {
    #find combinations
    combs = combinations(nrow(df), 2) #pick 2 out of nrow without order

    #find distances
    geo_dist = as.vector(distCosine(df[combs[, 1], 2:1], df[combs[, 2], 2:1]))
    #bizarrely, the authors of the distCosine has swapped the order of lat and lon which made me waste 30 mins bug hunting!

    #return
    return(geo_dist)
  }

  #matrix output
  if (output == "matrix") {
    #find combinations
    combs = expand.grid(1:nrow(df), 1:nrow(df)) #all permutations

    #find distances
    geo_dist = matrix(distCosine(df[combs[, 1], 2:1], df[combs[, 2], 2:1]), nrow=nrow(df))
    #bizarrely, the authors of the distCosine has swapped the order of lat and lon which made me waste 30 mins bug hunting!

    #remove diag?
    if (remove_diag) diag(geo_dist) = NA

    #return
    return(geo_dist)
  }
}



#' Calculate pairwise euclidean distances for all pairs.
#'
#' Returns a numeric vector with the pairwise euclidean distances for all pairs.
#' @param df A data.frame with variables.
#' @param output A character vector stating which output form is desired. Can be either vector or matrix. Vector results in a vector of all the unique combinations of distances, which is n*(n-1)/2, where n is the number of cases. Matrix results in the full distance matrix with every permutation including diagonals, which are always 0.
#' @keywords spatial autocorrelation, distance, euclidean
#' @export
get_euclidean_dists = function(df, output = "vector", remove_diag = T) {
  #loads fields
  library(fields)
  library(plyr)

  #check if numeric
  l_ply(df, function(x) {
    if(!is.numeric(x)) stop("At least one column was not numeric!")
  })

  #output
  output = agrep(output, c("vector", "matrix"), value = T) #fuzzy match input
  if (length(output) == 0) stop("Could not recognize desired output format!")

  #dist matrix all points
  df_dist_mat = rdist(as.matrix(df), as.matrix(df))
  #this is the matrix of distances between all points

  if(output == "matrix") {
    #remove diags?
    if (remove_diag) diag(df_dist_mat) = NA

    #return
    return(df_dist_mat)
  }

  #as a vector
  df_dist_vector = df_dist_mat[lower.tri(df_dist_mat)] #only get lower half

  #return
  return(df_dist_vector)
}



#' Calculate pairwise means.
#'
#' Returns a data.frame with the pairwise means of the desired type.
#' @param x A numeric object that can be forced into a data.frame.
#' @param weight_method A character string indicating which averaging method to use when combining weights for cases. Defaults to harmonic mean. Other options are arithmic and geometric.
#' @keywords pairwise, means, geometric, harmonic, arithmic
#' @export
get_pairwise_means = function(x, weight_method = "harmonic") {
  #change option
  if (options()$expressions < 10000) options(expressions = 10000)
  #not doing this gives an error
  #http://stackoverflow.com/questions/22003021/explanation-of-r-optionsexpressions-to-non-computer-scientists

  #missing values
  if (any(is.na(x))) stop("Missing values present, aborting!")

  #to df
  df = as.data.frame(x)

  library(plyr)
  #for ddply

  #all combinations
  library(gtools)
  combs = combinations(nrow(df), 2)

  #pipe
  library(magrittr)

  #two vectors
  df_weights = data.frame(w1 = df[combs[ , 1], ],
                          w2 = df[combs[ , 2], ])

  #weight average method?
  #fuzzy matching
  which_method = agrep(weight_method, c("harmonic", "arithmic", "geometric"), value = T)
  #failsafe
  if (length(which_method) == 0) which_method = "harmonic" #if no match found, use default

  #harmonic
  if (which_method == "harmonic") {
    df_return = aaply(df_weights, 1, function(x) {
      1/mean(1/unlist(x))
    }, .expand=F)
  }

  #arithmic
  if (which_method == "arithmic") {
    df_return = aaply(df_weights, 1, function(x) {
      unlist(x) %>% mean
    }, .expand=F)
  }

  #geometric
  if (which_method == "geometric") {
    df_return = aaply(df_weights, 1, function(x) {
      x = unlist(x)
      n = length(x)
      prod(x)^(1/n)
    }, .expand=F)
  }

  #return
  return(df_return)

}



#' Calculate pairwise distances for all pairs for all variables in a data.frame.
#'
#' Inputs a data.frame and outputs a data.frame with distances between all possible case pairs for each variable. One can specify that a pair of variables are spherical coordinates which are then treated in a special fashion using get_spherical_dists(). One can also specify a variable that is weights and for which a suitable mean will be calculated.
#' The function requires either that it is given a distance matrix or that it can find variables to calculate distances with.
#' @param df A data.frame with variables.
#' @param dists A matrix of distances between cases.
#' @param lat_var A string with the name of the variable which has the latitude data. Defaults to "lat".
#' @param lon_var A string with the name of the variable which has the longitude data. Defaults to "lon".
#' @param weights_var A string with the name of the variable which has the weight data.
#' @param weight_method A character string indicating which averaging method to use when combining weights for cases. Defaults to harmonic mean. Other options are arithmic and geometric.
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @keywords spatial autocorrelation, latitude, longitude, distance, weights
#' @export
get_distances = function(df, dists, lat_var, lon_var, distance_method, weights_var = "", weight_method = "harmonic", auto_detect_dist_method=T) {
  library(plyr) #for llply
  library(stringr) #for str_detect
  library(gtools) #for combinations

  #check input
  if (missing("df")) stop("df input missing!")
  if (anyNA(df)) stop("Missing values present. Remove and try again.")

  #weights
  if (weights_var == "") {
    df$weights___ = rep(1, nrow(df)) #fill in 1's
  } else {
    df$weights___ = df[[weights_var]] #use chosen var
  }

  #check spatial input
  check_results = check_spatial_input(df=df, dists=dists, lat_var=lat_var, lon_var=lon_var, distance_method=distance_method, auto_detect_dist_method=auto_detect_dist_method)

  #check sizes match
  if(check_results$setting == "dists") if(!any(dim(dists) == nrow(df))) stop("Data.frame and distance matrix do not match in size!")

  #coords
  if(check_results$setting == "coords") {
    distance_method = check_results$distance_method
    lat_var = check_results$lat_var
    lon_var = check_results$lon_var

    #check method
    if (!distance_method %in% c("spherical", "euclidean")) stop("Distance method unrecognized!")

    #subset latlon vars
    df_latlon = df[c(lat_var, lon_var)]

    #spherical
    if (distance_method == "spherical") df_latlon_dist = get_spherical_dists(df_latlon, lat_var=lat_var, lon_var=lon_var, output = "vector")

    #euclidean
    if (distance_method == "euclidean") df_latlon_dist = get_euclidean_dists(df_latlon, output = "vector")

    #remove latlon from main df
    df[lat_var] = NULL
    df[lon_var] = NULL
  }

  #weights?
  if (weights_var != "") {
    #extract weights var and remove from main df
    df_weights = df[weights_var]
    df = df[!str_detect(colnames(df), weights_var)]

    #get case pair weights
    if (sd(unlist(df_weights)) == 0) {
      df_pair_weights = rep(1, choose(nrow(df), 2))
    } else {
      df_pair_weights = get_pairwise_means(df_weights, weight_method = weight_method)
    }
  }

  #get absolute differences for all other variables
  if (ncol(df) != 0) {
    df_dist = as.data.frame(llply(df, function(x) dist(x) %>% as.vector))} else { #unless they are none
      df_dist = data.frame(matrix(nrow = choose(nrow(df), 2), ncol=0))
      #in which case we make an empty df to merge dists with
    }


  #add spatial dists if they exist
  if (check_results$setting == "coords") df_dist["spatial"] = df_latlon_dist
  if (check_results$setting == "dists") df_dist["spatial"] = dists[lower.tri(dists)] %>% as.vector

  #add weights if they exist
  if (weights_var != "") df_dist["weights___"] = df_pair_weights

  #return
  return(df_dist)
}


#' Calculate distance matrices for all variables in a data.frame.
#'
#' Inputs a data.frame and outputs a list with distance matrices. One can specify that a pair of variables are spherical coordinates which are then treated in a special fashion using get_spherical_dists().
#' The function requires either that it is given a distance matrix or that it can find variables to calculate distances with.
#' This is a sister function to get_distances(), which outputs vectors.
#' @param df A data.frame with variables.
#' @param dists A matrix of distances between cases.
#' @param lat_var A string with the name of the variable which has the latitude data. Defaults to "lat".
#' @param lon_var A string with the name of the variable which has the longitude data. Defaults to "lon".
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @keywords spatial autocorrelation, latitude, longitude, distance, matrix
#' @export
get_distances_mat = function(df, lat_var, lon_var, distance_method, auto_detect_dist_method=T) {

  #check input
  if (missing("df")) stop("df input missing!")
  if (anyNA(df)) stop("Missing values present. Remove and try again.")

  #check input
  check_input = check_spatial_input(df=df, lat_var=lat_var, lon_var=lon_var, distance_method=distance_method, auto_detect_dist_method=auto_detect_dist_method)

  #if spatial vars present
  if (check_input$setting == "coords") {
    #set vars
    lat_var = check_input$lat_var
    lon_var = check_input$lon_var
    distance_method = check_input$distance_method

    #check method
    if (!distance_method %in% c("spherical", "euclidean")) stop("Distance method unrecognized!")

    #spherical
    if (distance_method == "spherical") dists_spatial = get_spherical_dists(df, lat_var=lat_var, lon_var=lon_var, output = "matrix")

    #euclidean
    if (distance_method == "euclidean") dists_spatial = get_euclidean_dists(df[c(lat_var, lon_var)], output = "matrix")

    #remove spatial from df
    df = df[!colnames(df) %in% c(lat_var, lon_var)]
  }

  #get absolute differences for all other variables
  library(plyr)
  dists_list = llply(df, function(x) {
    dist(x) %>% as.matrix
  })

  #add spatial if exists
  if (check_input$setting == "coords") dists_list[["spatial"]] = dists_spatial

  return(dists_list)
}


#' Find k neighbor cases using spherical or euclidean geometry.
#'
#' Return a list of numeric vectors. Each vector contains the k nearest neighbors to the case. When using spherical geometry, you must give names of the variables. Euclidean geometry will use all variables. Cases with missing values are removed from the data.frame.
#' @param df A data.frame with variables.
#' @param dists A matrix of distances between cases.
#' @param k Number of neighbors to find for each case. Must be smaller than number of cases. Defaults to 3.
#' @param distance_method Which geometric system to use to calculate distances. Defaults to spherical. Can be either spherical or euclidean.
#' @param lat_var A numeric vector with the latitudes.
#' @param lon_var A numeric vector with the longitudes.
#' @param dists A distance matrix.
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @keywords spatial autocorrelation, latitude, longitude, distance, neighbors, knn
#' @export
find_neighbors = function(df, dists, k=3, distance_method, lat_var, lon_var, auto_detect_dist_method=T) {
  #libs
  library(plyr)
  library(rmngb)

  #autodetect distance method
  if(!missing("dists")) auto_detect_dist_method=F
  if(auto_detect_dist_method) {
    auto = distance_method_detector(df)
    distance_method = auto[1]
    lat_var = auto[2]
    lon_var = auto[3]
  }

  #calculate distances
  if (missing("dists")) {
    distance_method = agrep(distance_method, c("spherical", "euclidean"), value = T) #which method?
    if (length(distance_method) == 0) stop("Distance method unrecognized!")

    #subset
    if(distance_method == "spherical") {
      #subset & NA
      df = df[c(lat_var, lon_var)]
      df = na.omit(df) #no NA
    }

    if(distance_method == "euclidean") {
      #subset & NA
      df = na.omit(df) #no NA
    }

    #check that k < n
    if (k >= nrow(df)) stop("k must be smaller than number of cases!")

    #spherical
    if (distance_method == "spherical") {
      dists = get_spherical_dists(df, lat_var, lon_var, output = "matrix")
      #Order matters for spherical dists. We want matrix output.
    }

    #euclidean
    if (distance_method == "euclidean") {
      dists = get_euclidean_dists(df, output = "matrix")
      #Order is irrelevant for euc. dists. We want matrix output.
    }
  }

  #remove diag
  diag(dists) = NA
  #otherwise they would always be the 1st neighbor

  #find k neighbors for each case
  neighbors = alply(dists, 1, function(x) {
    x = as.vector(x) #as vector
    in_order = order(x) #order
    k_near = in_order[1:k] #pick the k first
  })

  #fix attributes
  neighbors = rmAttr(neighbors)

  #add names
  names(neighbors) = rownames(dists)

  return(neighbors)
}


#' Calculate Moran's I.
#'
#' Returns Moran's I coefficient and related NHST statistics. Excludes cases with missing data.
#' @param df A data.frame with variables.
#' @param var A string with the name of the variable for which Moran's I should be calculated.
#' @param dists A matrix of distances between cases.
#' @param lat_var A string with the name of the variable which has the latitude data.
#' @param lon_var A string with the name of the variable which has the longitude data.
#' @param distance_method Which geometric system to use to calculate distances. Defaults to spherical. Can be either spherical or euclidean. If using euclidean it doesn't matter which variable is coded as lat or lon.
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @keywords spatial autocorrelation, latitude, longitude, distance, Moran's I
#' @export
get_Morans_I = function(df, var, dists, lat_var, lon_var, distance_method, auto_detect_dist_method=T) {
  #This function is based on the code given by Hassall & Sherratt (2011)
  #Statistical inference and spatial patterns in correlates of IQ, Intelligence
  library(geosphere)
  library(ape)
  library(magrittr)

  #autodetect distance method
  if(!missing("dists")) auto_detect_dist_method=F
  if(auto_detect_dist_method) {
    auto = distance_method_detector(df)
    distance_method = auto[1]
    lat_var = auto[2]
    lon_var = auto[3]
  }

  #subset to the needed variables
  if(missing("dists")) df = df[c(var, lat_var, lon_var)]
  df = na.omit(df) #remove cases with missing

  #distances
  if (missing("dists")) {
    distance_method = agrep(distance_method, c("spherical", "euclidean"), value = T) #which method?
    if (length(distance_method) == 0) stop("Distance method unrecognized!")

    #spherical
    if (distance_method == "spherical") dists = get_spherical_dists(df, lat_var, lon_var, output = "matrix")
    if (distance_method == "euclidean") dists = get_euclidean_dists(df[c(lat_var, lon_var)], output = "matrix")
  }

  # invert the matrix
  dists_inv = 1/dists

  # define the diagonal as zero
  diag(dists_inv) = 0

  # calculate Moran's I
  Moran.I(df[[var]], dists_inv) %>% return
}


#' Calculate multiple Moran's I's. It's a wrapper function for get_Morans_I().
#'
#' Returns a numeric vector with Moran's I coefficients.
#' @param df A data.frame with variables.
#' @param vars A character vector with the names of the variables for which Moran's I should be calculated.
#' @param dists A matrix of distances between cases.
#' @param lat_var A string with the name of the variable which has the latitude data.
#' @param lon_var A string with the name of the variable which has the longitude data.
#' @param distance_method Which geometric system to use to calculate distances. Defaults to spherical. Can be either spherical or euclidean. If using euclidean it doesn't matter which variable is coded as lat or lon.
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @keywords spatial autocorrelation, latitude, longitude, distance, Moran's I, wrapper
#' @export
get_Morans_I_multi = function(df, vars, dists, lat_var, lon_var, distance_method, auto_detect_dist_method=T) {
  #This function is based on the code given by Hassall & Sherratt (2011)
  #Statistical inference and spatial patterns in correlates of IQ, Intelligence
  library(geosphere)
  library(ape)
  library(plyr)
  library(magrittr)

  #autodetect distance method
  if(!missing("dists")) auto_detect_dist_method=F
  if(auto_detect_dist_method) {
    auto = distance_method_detector(df)
    distance_method = auto[1]
    lat_var = auto[2]
    lon_var = auto[3]
  }

  #subset % NA
  if(missing("dists")) {
    df = df[c(vars, lat_var, lon_var)]
  }
  df = na.omit(df) #remove missing

  #calculate distances if not given
  if (missing("dists")) {
    #check method
    if (length(distance_method) == 0) stop("Distance method unrecognized!")

    #spherical
    if (distance_method == "spherical") dists = get_spherical_dists(df, lat_var, lon_var, output = "matrix")
    #euclidean
    if (distance_method == "euclidean") dists = get_euclidean_dists(df[c(lat_var, lon_var)], output = "matrix")
  }

  # invert the matrix
  dists_inv = 1/dists

  # define the diagonal as zero
  diag(dists_inv) = 0

  # calculate Moran's I for each variable
  Morans_Is = ldply(vars, function(x) {
    Moran.I(df[[x]], dists_inv)[[1]]
  }) %>% unlist

  names(Morans_Is) = vars #add names

  return(Morans_Is) #return
}



#' Add spatial autocorrelation to a variable.
#'
#' Adds spatial autocorrelation to a variable using the k nearest spatial neighbor method.
#' @param df A data.frame with variables.
#' @param var A string with the name of the variable in which SAC should be added.
#' @param k The number of neighbors taken into account. Defaults to 3.
#' @param iter The number of iterations. Defaults to 1.
#' @param dists A matrix of distances between cases.
#' @param weight The weight to assign the value from the neighbors in each iteration.
#' @param lat_var A string with the name of the variable which has the latitude data.
#' @param lon_var A string with the name of the variable which has the longitude data.
#' @param distance_method Which geometric system to use to calculate distances. Defaults to spherical. Can be either spherical or euclidean. If using euclidean it doesn't matter which variable is coded as lat or lon.
#' @param verbose Adds messages with the progress.
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @keywords spatial autocorrelation, latitude, longitude, distance, knn, knsn, simulation
#' @export
add_SAC = function(df, vars, k=3, iter=1, weight=1/3, dists, lat_var, lon_var, distance_method, verbose = F, auto_detect_dist_method=T){

  #libs
  library(plyr)
  library(stringr)

  #autodetect distance method
  if (!missing("dists")) auto_detect_dist_method=F
  if (auto_detect_dist_method) {
    auto = distance_method_detector(df)
    distance_method = auto[1]
    lat_var = auto[2]
    lon_var = auto[3]
  }

  #subset & NA
  if (missing("dists")) df = df[c(vars, lat_var, lon_var)]
  df = na.omit(df) #no NA

  #find neighbors
  if (missing("dists")) {neighbors = find_neighbors(df[c(lat_var, lon_var)], k=k, distance_method=distance_method, lat_var=lat_var, lon_var=lon_var)} else
  {neighbors = find_neighbors(df[c(lat_var, lon_var)], k=k, dists=dists)}


  #for each iteration
  for (i in 1:iter) {

    #message
    if (verbose) message(str_c("Iteration ", i, " of ", iter))

    #for each var
    for (var in vars) {

      #find neighbor means
      neighbor_means = llply(neighbors, function(x) {
        mean = mean(df[x, var]) #get means of the neighbors
      })


      #new values for each case
      for (case in 1:nrow(df)) {
        new_mean = weighted.mean(c(df[case, var], neighbor_means[[case]]),
                                 c(1-weight, weight)) #use given weight

        #save
        df[case, var] = new_mean
      }
    }
  }

  return(df)
}



#' Perform k nearest spatial neighbor regression.
#'
#' Predict the value of cases on the basis of values of nearby cases. A useful measure of spatial autocorrelation. Returns a data.frame. Keeps rownames.
#' @param df A data.frame with variables.
#' @param dependent A string with the name of the dependent variable.
#' @param predictor A string with the name of the predictor variable. Only used for resids_cor output.
#' @param k The number of neighbors taken into account. Defaults to 3.
#' @param dists A matrix of distances between cases.
#' @param lat_var A string with the name of the variable which has the latitude data.
#' @param lon_var A string with the name of the variable which has the longitude data.
#' @param weights_var A string with the name of the variable that contains case weights.
#' @param distance_method Which geometric system to use to calculate distances. Can be either spherical or euclidean. If using euclidean it doesn't matter which variable is coded as lat or lon.
#' @param outout Which type of output to return. Can be (predicted) scores, cor or resids. Defaults to scores.
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @keywords spatial autocorrelation, latitude, longitude, distance, knn, knsn
#' @export SAC_knsn_reg SAC_knsnr
#' @aliases SAC_knsn_reg
SAC_knsnr = function(df, dependent, predictor, k = 3, dists, lat_var, lon_var, weights_var = "", distance_method, output = "scores", auto_detect_dist_method=T) {
  library(fields) #for rdist
  library(stringr) #for str_c


  #check input
  if (missing("df")) stop("df input missing!")
  if (anyNA(df)) stop("Missing values present. Remove and try again.")
  if (!weights_var %in% colnames(df) & weights_var != "") stop("Weights variable isn't in the data.frame!")
  if (!k < nrow(df)) stop("k must be smaller than the number of cases!")

  #check spatial input
  check_results = check_spatial_input(df=df, dists=dists, lat_var=lat_var, lon_var=lon_var, distance_method=distance_method, auto_detect_dist_method=auto_detect_dist_method)

  #if no spatial info
  if(check_results$setting == "na") {
    stop("No spatial information detected!")
  }

  #check if dists and df match in size
  if (check_results$setting == "dists") {
    if (!all(nrow(df) == dim(dists))) stop("Size of the distance matrix does not match the data.frame's number of cases. This could be due to missing values.")
  }

  #coords
  if(check_results$setting == "coords") {
    distance_method = check_results$distance_method
    lat_var = check_results$lat_var
    lon_var = check_results$lon_var

    #check method
    if (!distance_method %in% c("spherical", "euclidean")) stop("Distance method unrecognized!")

    #calculate
    if (distance_method == "spherical") dists = get_spherical_dists(df=df, lat_var=lat_var, lon_var=lon_var, output="matrix")
    if (distance_method == "euclidean") dists = get_euclidean_dists(df[c(lat_var, lon_var)], output = "matrix")
  }

  #keep orig names and length
  orig_names = rownames(df)
  orig_nrow = nrow(df)
  df_return = as.data.frame(matrix(nrow=orig_nrow, ncol=1))
  rownames(df_return) = orig_names
  colnames(df_return) = "y_hat"

  #weights
  if (weights_var == "") {
    df$weights___ = rep(1, nrow(df)) #fill in 1's
  } else {
    df$weights___ = df[[weights_var]] #use chosen var
  }

  #   #subset data
  #   #this depends on whether the dists are given are not
  #   if (missing("dists") & missing("predictor")) {
  #     df = df[c(dependent, lat_var, lon_var, "weights___")]
  #   } else if (missing("dists") & !missing("predictor")) {
  #     df = df[c(dependent, predictor, lat_var, lon_var, "weights___")]
  #   } else {df = df[c(dependent, "weights___")]}


  #object for results
  y_hat = numeric()

  #loop over each case
  for (case in 1:nrow(df)) {
    tmp = dists[, case] #k nearest cases
    y = df[[dependent]][order(dists[, case])][1:k] #find the vaulues of the k nearest cases
    w = df[["weights___"]][order(dists[, case])][1:k] #find the weights of the k nearest cases
    y = weighted.mean(y, w) #calculate weighted mean

    y_hat[case] = y #save estimate
  }

  #add names
  names(y_hat) = rownames(df)

  #merge
  df_return = merge_datasets(df_return, as.data.frame(y_hat))
  df_return = merge_datasets(df_return, df)

  #output
  if (output == "scores") {
    ret = df_return[["y_hat"]]
    names(ret) = rownames(df_return)
    return(ret)
  }
  if (output == "cor") return(cor(df_return[["y_hat"]], df_return[[dependent]]))
  if (output == "resids"){
    model = str_c(dependent, " ~ y_hat")
    fit = lm(model, df_return, weights = weights___, na.action = na.exclude)
    return(resid(fit))
  }
  if (output == "resids_cor"){
    model = str_c(dependent, " ~ y_hat")
    fit = lm(model, df_return, weights = weights___, na.action = na.exclude)
    return(cor(resid(fit), df_return[[predictor]]))
  }
}

SAC_knsn_reg = SAC_knsnr #old name


#' K nearest spatial neighbor partial correlations.
#'
#' Removes SAC in each variable using knsnr and then correlates them with each other. Returns a correlation matrix.
#' @param df A data.frame with variables.
#' @param variables A character vector of variable names that should be correlated, each corrected for SAC.
#' @param k The number of neighbors taken into account. Defaults to 3.
#' @param dists A matrix of distances between cases.
#' @param lat_var A string with the name of the variable which has the latitude data.
#' @param lon_var A string with the name of the variable which has the longitude data.
#' @param weights_var A string with the name of the variable that contains case weights.
#' @param distance_method Which geometric system to use to calculate distances. Can be either spherical or euclidean. If using euclidean it doesn't matter which variable is coded as lat or lon.
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @keywords spatial autocorrelation, latitude, longitude, distance, knn, knsn, residuals
#' @export
SAC_knsn_reg_partial = function(df, variables, k = 3, dists, lat_var, lon_var, weights_var = "", distance_method, auto_detect_dist_method=T) {

  #check input
  if (missing("df")) stop("df input missing!")
  if (missing("variables")) stop("No variables given!")
  if (anyNA(df)) stop("Missing values present. Remove/impute and try again.")

  #check spatial input
  check_results = check_spatial_input(df=df, dists=dists, lat_var=lat_var, lon_var=lon_var, distance_method=distance_method, auto_detect_dist_method=auto_detect_dist_method)

  #if no spatial info
  if(check_results$setting == "na") {
    stop("No spatial information detected!")
  }

  #coords
  if(check_results$setting == "coords") {
    distance_method = check_results$distance_method
    lat_var = check_results$lat_var
    lon_var = check_results$lon_var

    #get distances
    dists = get_distances_mat(df[c(lat_var, lon_var)], lat_var=lat_var, lon_var=lon_var, distance_method=distance_method)[[1]]
  }

  #for results
  d_return = data.frame(matrix(nrow=nrow(df), ncol=0))
  for (var in variables) {
    #get the residuals for that variable and save
    d_return[var] = SAC_knsn_reg(df=df, dependent=var, k=k, dists=dists, lat_var=lat_var, lon_var=lon_var, weights_var=weights_var, distance_method=distance_method, output="resids", auto_detect_dist_method=auto_detect_dist_method)
  }
  cor(d_return, use = "p") %>% return
}


#' Calculate multiple spatial autocorrelation measures.
#'
#' Returns a data.frame with measures of SAC using Moran's I, CD, CD_sqrt and KNSNR.
#' @param df (data.frame) The data.frame with variables.
#' @param vars (chr vector) The names of the variables for which SAC measures should be calculated.
#' @param dists (matrix) A matrix of distances between cases.
#' @param lat_var (chr scalar) The name of the variable which has the latitude/east-west data.
#' @param lon_var (chr scalar) The name of the variable which has the longitude/north-south data.
#' @param distance_method (chr scalar) Which geometric system to use to calculate distances. Defaults to spherical. Can be either spherical or euclidean. If using euclidean it doesn't matter which variable is coded as lat or lon.
#' @param k (num vector) A vector of k values to use for knsnr. Defaults to 3.
#' @param weights_var (chr scalar) The name of the variable which has the case weights. Optional.
#' @param weight_method (chr scalar) The weighing method to use. Defaults to harmonic.
#' @param auto_detect_dist_method (log scalar) Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @param measures (chr vector) A vector of the measures to use. Options are Morans, CD and KNSNR (default is to use all).
#' @param CD_convert_NaN_to_zero (log scalar) Whether to convert NaN to zeros in CD. At a point in the calculate, a square root is taken and because the first value can be negative, this may give a non-real number. This probably means that it should be thought of as 0.
#' @export get_SAC_measures SAC_measures
#' @aliases get_SAC_measures
SAC_measures = function(df, vars, dists, lat_var, lon_var, distance_method, k = 3, weights_var="", weight_method="harmonic", auto_detect_dist_method=T, measures = c("Morans", "CD", "KNSNR"), CD_convert_NaN_to_zero = T) {
  library(stringr)

  #check spatial input
  check_results = check_spatial_input(df=df, dists=dists, lat_var=lat_var, lon_var=lon_var, distance_method=distance_method, auto_detect_dist_method=auto_detect_dist_method)

  #coords
  if(check_results$setting == "coords") {
    distance_method = check_results$distance_method
    lat_var = check_results$lat_var
    lon_var = check_results$lon_var

    #get distances
    dists = get_distances_mat(df[c(lat_var, lon_var)], lat_var = lat_var, lon_var = lon_var, distance_method = distance_method)[[1]]
  }

  #weights
  if (weights_var == "") {
    df$weights___ = rep(1, nrow(df)) #fill in 1's
  } else {
    df$weights___ = df[[weights_var]] #use chosen var
  }
  weights_var = "weights___"

  #data.frame for results
  df_ret = data.frame(matrix(nrow=length(vars), ncol=0)) #df for storing results
  rownames(df_ret) = vars


  #Moran's I
  if ("Morans" %in% measures) {
    morans = get_Morans_I_multi(df=df, vars=vars, dists=dists, lat_var=lat_var, lon_var=lon_var, distance_method=distance_method)
    df_ret$Morans_I = morans
  }

  #correlation of distances
  if ("CD" %in% measures) {

    df_dist = get_distances(df=df, dists=dists, lat_var=lat_var, lon_var=lon_var, distance_method=distance_method, weights_var=weights_var, weight_method=weight_method)

    cd = suppressWarnings(cor(df_dist)["spatial", vars])
    df_ret$cd = cd
    df_ret$cd_sqrt = suppressWarnings(sqrt(cd))

    #if any values were NaN, convert to 0
    if (CD_convert_NaN_to_zero) {
      df_ret[is.na(df_ret)] = 0
    }
  }


  #KNSNR
  if ("KNSNR" %in% measures) {
    for (k_ in k) {
      for (var in vars) {
        knsnr = SAC_knsn_reg(df=df, dependent=var, k=k_, dists=dists, lat_var=lat_var, lon_var=lon_var, weights_var=weights_var, distance_method=distance_method, output = "cor")
        df_ret[var, str_c("knsn_", k_)] = knsnr
      }
    }
  }

  return(df_ret)
}

get_SAC_measures = SAC_measures #old name

#' Perform spatial local regression.
#'
#' Returns either summary statistics for each predictor or a data.frame with the betas from each local regression.
#' @param df A data.frame with variables.
#' @param dependent A character vector with the name of the dependent variable.
#' @param predictors A character vector with the names of the predictor variables.
#' @param k The number of neighbors to use in the regressions. Defaults to 3.
#' @param output Which meta-analysis measure to use. Defaults to trim10, a 10 percent trimmed mean. Other options are: mean, median, and trim followed by any desired number. Can also use 'values' to get the values which returns a data.frame.
#' @param dists A matrix of distances between cases.
#' @param lat_var A string with the name of the variable which has the latitude/east-west data.
#' @param lon_var A string with the name of the variable which has the longitude/north-south data.
#' @param distance_method Which geometric system to use to calculate distances. Defaults to spherical. Can be either spherical or euclidean. If using euclidean it doesn't matter which variable is coded as lat or lon.
#' @param weights_method A string with the name of the weights method to use. Defaults to "inverse", i.e. weighing cases by their inverse distance to the case being considered. Can also be "none".
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @keywords spatial autocorrelation, regression, spatial local regression, local regression
#' @export
SAC_slr = function(df, dependent, predictors, k=3, output = "trim10", dists, lat_var, lon_var, distance_method, auto_detect_dist_method=T, weights_method="inverse", weights_var = "", include_self = F, verbose = T) {
  library(stringr)

  #check input
  if (missing("df")) stop("df input missing!")
  if (missing("dependent")) stop("Dependent variable not given!")
  if (missing("predictors")) stop("Dependent variable not given!")
  if (anyNA(df)) stop("Missing values present. Remove/impute and try again.")
  if (include_self) weights_method = "none" #otherwise an error happens!
  if (missing("output")) output = "trim10" #if not given, use trim 10

  #check spatial input
  check_results = check_spatial_input(df=df, dists=dists, lat_var=lat_var, lon_var=lon_var, distance_method=distance_method, auto_detect_dist_method=auto_detect_dist_method)

  #weights
  if (weights_var == "") {
    df$weights___ = rep(1, nrow(df)) #fill in 1's
  } else {
    df$weights___ = df[[weights_var]] #use chosen var
  }
  weights_var = "weights___"


  #if no spatial info
  if(check_results$setting == "na") {
    stop("No spatial information detected!")
  }

  #coords
  if(check_results$setting == "coords") {
    distance_method = check_results$distance_method
    lat_var = check_results$lat_var
    lon_var = check_results$lon_var

    #get distances
    dists = get_distances_mat(df[c(lat_var, lon_var)], lat_var = lat_var, lon_var = lon_var, distance_method = distance_method)[[1]]
  }

  #find k neighbors
  if (include_self) {
    neighbor_list = find_neighbors(df=df, dists=dists, k=k-1, distance_method=distance_method, lat_var=lat_var, lon_var=lon_var)
    #only finds k-1 neighbors because we want to include the case itself as well
  } else {
    neighbor_list = find_neighbors(df=df, dists=dists, k=k, distance_method=distance_method, lat_var=lat_var, lon_var=lon_var)
  }


  #get their distances
  neighbor_dists = llply(1:length(neighbor_list), function(x) {
    tmp_neighbors = neighbor_list[[x]]
    tmp_dists = dists[x, tmp_neighbors]
    tmp_dists
  })

  #for storing output
  betas = data.frame(matrix(nrow = nrow(df), ncol = length(predictors)))
  colnames(betas) = predictors

  #loop over cases
  for (case in 1:nrow(df)) {
    #neighbors
    case_neighbors = neighbor_list[[case]]

    #subset the case plus the k nearest neighbors
    if (include_self) { #include self
      df_sub = df[c(case, case_neighbors), ]
    } else { #dont include self
      df_sub = df[c(case_neighbors), ]
    }

    #standardize
    df_sub_std = std_df(df_sub, exclude = "weights___") #exclude weights

    #check if NAs were created
    #this happens if sd=0 and gives mysterious errors
    if (anyNA(df_sub_std[c(dependent, predictors)])) {
      if (verbose) {
        print("One or more values were NA for this cluster of data.")
        print(df_sub_std[c(dependent, predictors)])
      }
      next
    }

    #make model
    model = str_c(dependent, " ~ ", str_c(predictors, collapse = " + "))

    #which weights? and then fit
    #this creates a weights vector based both on distance (if desired) and given case weights
    if (weights_method == "none") {
      df_sub_std$weights__ = rep(1, nrow(df_sub_std)) * df_sub_std$weights___ #fill with 1's
    } else if (weights_method == "inverse") {
      case_neighbor_dists = neighbor_dists[[case]]
      df_sub_std$weights__ = 1/case_neighbor_dists * df_sub_std$weights___
    } else {
      stop("weights_method not recognized!")
    }

    #fit
    fit = lm(model, df_sub_std, weights = weights__)

    #save betas
    betas[case, ] = coef(fit)[-1]
  }


  #all values NA?
  if (sum(!is.na(betas)) == 0) return(NaN)

  #output form
  if (output == "mean") {
    mean_beta = apply(betas, 2, mean, na.rm=T)
    return(mean_beta)
  }

  if(str_detect(output, "trim")) {
    tmp_trim_val = str_extract(output, "\\d+") %>% as.numeric %>% `/`(., 100)
    mean_beta = apply(betas, 2, mean, trim = tmp_trim_val, na.rm=T)
    return(mean_beta)
  }

  if(output == "median") {
    mean_beta = apply(betas, 2, median, na.rm=T)
    return(mean_beta)
  }

  if(output == "values" | output == "vector") {
    return(betas)
  }
}



#' Control for spatial autocorrelation with multiple methods
#'
#' Returns a data.frame with predictors for linear regression with and without controls for SAC.
#' @param df A data.frame with variables.
#' @param dependent A character vector with the name of the dependent variable.
#' @param predictors A character vector with the name of the predictor variables.
#' @param knsn_k The number of neighbors to use with k nearest spatial regression. Defaults to 3.
#' @param slr_k The number of neighbors to use with spatial local regression. Defaults to 3.
#' @param dists A matrix of distances between cases.
#' @param lat_var A string with the name of the variable which has the latitude/east-west data.
#' @param lon_var A string with the name of the variable which has the longitude/north-south data.
#' @param distance_method Which geometric system to use to calculate distances. Defaults to spherical. Can be either spherical or euclidean. If using euclidean it doesn't matter which variable is coded as lat or lon.
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @param SLR_weights_method A string with the name of the weights method to use for spatial local regression. Defaults to "inverse", i.e. weighing cases by their inverse distance to the case being considered. Can also be "none".
#' @param SLR_include_self Whether to include the self node or not in SLR. Defaults to F.
#' @param SLR_central_measure Which central tendency measure to meta-analyze SLR results with. Defaults to trim10, which is a 10 percent trimmed mean. For other options, see help for SAC_slr.
#' @param CD_weight_method A string indicating which averaging method to use when combining weights for cases. Defaults to harmonic mean. Other options are arithmic and geometric.
#' @param weights_var A character vector of the name of the weight variable to use. If none given, defaults to equal weights.
#' @param methods Which control methods to use. Options are CD, KNSNR, SLR. Defaults to all of them.
#' @param standardize Whether to standardize the data to get standardized betas. Defaults to T.
#' @param control_approach Which control approaches to use. Options are partial and mr. Defaults to partial.
#' @export SAC_control SAC_control_all_methods
#' @aliases SAC_control_all_methods
SAC_control = function(df, dependent, predictors, knsn_k=3, slr_k = 3, dists, lat_var, lon_var, distance_method, auto_detect_dist_method=T, SLR_weights_method="inverse", SLR_include_self = F, SLR_central_measure, CD_weight_method = "harmonic", weights_var="", methods = c("KNSNR", "SLR"), standardize = T, control_approach = c("partial")) {
  library(stringr)

  #check input
  #is df
  if (missing("df")) stop("df input missing!")

  #are variables even there?
  if (any(!(c(dependent, predictors) %in% colnames(df))))

  if (missing("dependent")) stop("Dependent variable not given!")
  if (missing("predictors")) stop("Dependent variable not given!")
  if (anyNA(df[c(dependent, predictors)])) stop("Missing values present. Remove/impute and try again.")
  if (!all(control_approach %in% c("partial", "mr"))) {
    stop("Unrecognized control approaches included!")
  }



  #weights
  if (weights_var == "") {
    df$weights___ = rep(1, nrow(df)) #fill in 1's
  } else {
    df$weights___ = df[[weights_var]] #use chosen var
  }
  weights_var = "weights___"

  #check spatial input
  check_results = check_spatial_input(df=df, dists=dists, lat_var=lat_var, lon_var=lon_var, distance_method=distance_method, auto_detect_dist_method=auto_detect_dist_method)

  #if no spatial info
  if(check_results$setting == "na") {
    stop("No spatial information detected!")
  }

  #coords
  if(check_results$setting == "coords") {
    distance_method = check_results$distance_method
    lat_var = check_results$lat_var
    lon_var = check_results$lon_var

    #get distances
    dists = get_distances_mat(df[c(lat_var, lon_var)], lat_var=lat_var, lon_var=lon_var, distance_method=distance_method)[[1]]

    #calculate only if needed
    if ("CD" %in% methods) {
      dists_vec = get_distances(df=df[c(lat_var, lon_var, dependent, predictors, weights_var)], lat_var=lat_var, lon_var=lon_var, distance_method=distance_method, weights_var = weights_var)

      #standardize?
      if (standardize) {
        dists_vec = std_df(dists_vec, exclude = "weights___", messages = F)
      }
    }
  }

  #add KNSNR predictor scores var
  #must be done before standardization if that is done
  if ("KNSNR" %in% methods) {
    df$spatial = SAC_knsn_reg(df=df, dists = dists, dependent = dependent, k = knsn_k, output = "scores", weights_var = "weights___")
  }

  #subset otherwise some of the below will waste time calculating on unused variables
  df = df[colnames(df) %in% c(dependent, predictors, "weights___", "spatial")]

  #standardize?
  if (standardize) {
    df = std_df(df, exclude = "weights___", messages = F)
  }


  #for results
  if ("mr" %in% control_approach) {
    d_betas = as.data.frame(matrix(nrow = length(predictors)+1, ncol = 0))
    rownames(d_betas) = c(predictors, "spatial")
  } else {
    d_betas = as.data.frame(matrix(nrow = length(predictors), ncol = 0))
    rownames(d_betas) = c(predictors)
  }

  #which rows?
  if ("mr" %in% control_approach) {
    rows_ = 1:(nrow(d_betas)-1)
  } else {
    rows_ = 1:nrow(d_betas)
  }


  #make formulas
  tmp_form_unc = str_c(dependent, " ~ ", str_c(predictors, collapse = " + "))
  tmp_form_o = str_c(str_c(dependent, "_res"), " ~ ", str_c(predictors, collapse = " + "))
  tmp_form_p = str_c(dependent, " ~ ", str_c(str_c(predictors, "_res"), collapse = " + "))
  tmp_form_b = str_c(str_c(dependent, "_res"), " ~ ", str_c(str_c(predictors, "_res"), collapse = " + "))
  tmp_form_mr = str_c(dependent, " ~ ", str_c(c(predictors, "spatial"), collapse = " + "))


  #uncontrolled results
  d_betas[rows_, "Uncorrected"] = lm(formula = tmp_form_unc, data = df, weights = weights___) %>% coef %>% `[`(-1)


  #CD
  if ("CD" %in% methods) {
    #get distances if using dists input
    if (check_results$setting == "dists") {
      dists_vec = get_distances(df=df[c(dependent, predictors)], auto_detect_dist_method = F, weights_var = "weights___")
      dists_vec$spatial = dists[lower.tri(dists)]

      #standardize?
      if (standardize) {
        dists_vec = std_df(dists_vec, exclude = "weights___", messages = F)
      }
    }

    #partials
    #res. vars
    dists_vec_res = residualize_DF(dists_vec, "spatial", suffix = "_res", return.resid.vars = F, print.models = F)
    #merge to create a larger df with variables both in res and normal form
    dists_vec = cbind(dists_vec, dists_vec_res)

    if ("partial" %in% control_approach) {

      d_betas[rows_, "CD_o"] = lm(tmp_form_o, dists_vec, weights = weights___) %>% coef %>% `[`(-1) %>% sqrt

      d_betas[rows_, "CD_p"] = lm(tmp_form_p, dists_vec, weights = weights___) %>% coef %>% `[`(-1) %>% sqrt

      d_betas[rows_, "CD_b"] = lm(tmp_form_b, dists_vec, weights = weights___) %>% coef %>% `[`(-1) %>% sqrt
    }

    if ("mr" %in% control_approach) {
      d_betas["CD_mr"] = lm(tmp_form_mr, dists_vec, weights = weights___) %>% coef %>% `[`(-1) %>% sqrt
    }

  }


  #KNSNR
  if ("KNSNR" %in% methods) {

    if ("partial" %in% control_approach) {
      #res. df
      df_res = lapply(colnames(df), function(x) {
        SAC_knsn_reg(df=df, dependent=x, k=knsn_k, dists=dists, output = "resids", weights_var = "weights___")
      }) %>% as.data.frame
      colnames(df_res) = str_c(colnames(df), "_res") #add identifier to resids version
      df_tmp = cbind(df, df_res) #merge

      #standardize, if standardized values are desired
      if (standardize) {
        df_tmp = std_df(df_tmp, exclude = "weights___", messages = F)
      }

      #regress
      d_betas[rows_, str_c("KNSNR_o_k", knsn_k)] = lm(tmp_form_o, df_tmp, weights = weights___) %>% coef %>% `[`(-1)

      d_betas[rows_, str_c("KNSNR_p_k", knsn_k)] = lm(tmp_form_p, df_tmp, weights = weights___) %>% coef %>% `[`(-1)

      d_betas[rows_, str_c("KNSNR_b_k", knsn_k)] = lm(tmp_form_b, df_tmp, weights = weights___) %>% coef %>% `[`(-1)
    }

    if ("mr" %in% control_approach) {
      d_betas[str_c("KNSNR_mr_k", knsn_k)] = lm(tmp_form_mr, df, weights = weights___) %>% coef %>% `[`(-1)
    }

  }


  #SLR
  if ("SLR" %in% methods) {
    d_betas[rows_, str_c("SLR_k", slr_k)] = SAC_slr(df=df, dependent=dependent, predictors=predictors, k=slr_k, weights_method = SLR_weights_method, dists=dists, include_self = SLR_include_self, output = SLR_central_measure)
  }

  #return
  return(d_betas)
}

SAC_control_all_methods = SAC_control #old name



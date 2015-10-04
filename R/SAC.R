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
#' @examples
#' get_spherical_dists()
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
#' @examples
#' get_euclidean_dists()
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
#' @examples
#' get_pairwise_means()
get_pairwise_means = function(x, weight_method = "harmonic") {
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
#' @examples
#' get_distances()
get_distances = function(df, dists, lat_var, lon_var, distance_method, weights_var = "", weight_method = "harmonic", auto_detect_dist_method=T) {
  library(plyr) #for llply
  library(stringr) #for str_detect
  library(gtools) #for combinations

  #autodetect distance method
  if (!missing("dists")) auto_detect_dist_method=F
  if(auto_detect_dist_method) {
    #spherical
    if (all(c("lat", "lon") %in% colnames(df))) {
      distance_method = "spherical"
      lat_var = "lat"
      lon_var = "lon"
    }

    #euclidean
    if (all(c("x", "y") %in% colnames(df))) {
      distance_method = "euclidean"
      lat_var = "x"
      lon_var = "y"
    }
  }

  #coerce to df
  df = as.data.frame(df)

  #is weights var there?
  if (!weights_var %in% colnames(df) & weights_var != "") stop("Weights variable isn't in the data.frame!")

  #remove missing data
  if (any(is.na(df))) message("Warning, data.frame contained cases with missing values. These cases were excluded!")
  df = na.omit(df)

  #check sizes after NA removal
  if(!missing("dists")) if(!any(dim(dists) == nrow(df))) stop("Data.frame and distance matrix do not match in size! This can happen if there is missing data.")

  #calcualte distances if needed
  #needed if dists not given AND no spatial vars present
  if (missing("dists") & !missing("lat_var") & !missing("lon_var")) {
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

    geo = T

  }


  #weights?
  if (weights_var != "") {
    #extract weights var and remove from main df
    df_weights = df[weights_var]
    df = df[!str_detect(colnames(df), weights_var)]

    #get case pair weights
    df_pair_weights = get_pairwise_means(df_weights, weight_method = weight_method)
  }

  #get absolute differences for all other variables
  if (ncol(df) != 0) {
    df_dist = as.data.frame(llply(df, function(x) dist(x) %>% as.vector))} else { #unless they are none
    df_dist = data.frame(matrix(nrow = choose(nrow(df), 2), ncol=0))
    #in which case we make an empty df to merge dists with
  }


  #add spatial dists if they exist
  if (exists("geo")) df_dist["spatial"] = df_latlon_dist
  if (!missing("dists")) df_dist["spatial"] = dists[lower.tri(dists)] %>% as.vector

  #add weights if they exist
  if (weights_var != "") df_dist["weight"] = df_pair_weights

  #return
  return(df_dist)
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
#' @examples
#' find_neighbors()
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
#' @examples
#' get_Morans_I()
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
#' @examples
#' get_Morans_I_multi()
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
#' @examples
#' add_SAC()
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
#' @param k The number of neighbors taken into account. Defaults to 3.
#' @param dists A matrix of distances between cases.
#' @param lat_var A string with the name of the variable which has the latitude data.
#' @param lon_var A string with the name of the variable which has the longitude data.
#' @param weights_var A string with the name of the variable that contains case weights.
#' @param distance_method Which geometric system to use to calculate distances. Can be either spherical or euclidean. If using euclidean it doesn't matter which variable is coded as lat or lon.
#' @param outout Which type of output to return. Can be (predicted) scores, cor or resids. Defaults to scores.
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @keywords spatial autocorrelation, latitude, longitude, distance, knn, knsn
#' @export
#' @examples
#' knsn_reg()
knsn_reg = function(df, dependent, k = 3, dists, lat_var, lon_var, weights_var = "", distance_method, output = "scores", auto_detect_dist_method=T) {
  library(fields) #for rdist

  #autodetect distance method
  if(!missing("dists")) auto_detect_dist_method=F
  if(auto_detect_dist_method) {
    auto = distance_method_detector(df)
    distance_method = auto[1]
    lat_var = auto[2]
    lon_var = auto[3]
  }

  #options
  if (!output %in% c("scores", "cor", "resids")) stop("Desired output unrecognized!")

  #keep orig names and length
  orig_names = rownames(df)
  orig_nrow = nrow(df)
  df_return = as.data.frame(matrix(nrow=orig_nrow, ncol=1))
  rownames(df_return) = orig_names
  colnames(df_return) = "y_hat"
  #all this stuff is needed to return a vector of the same length as the original input with NAs in the correct positions. Otherwise, it can be difficult to use the predicted values. It is the same functionality as lm() provides with na.exclude.

  #weights
  if (weights_var == "") {
    df$weights___ = rep(1, nrow(df)) #fill in 1's
  } else {
    df$weights___ = df[[weights_var]] #use chosen var
  }

  #subset data
  #this depends on whether the dists are given are not.
  if (missing("dists")) {df = df[c(dependent, lat_var, lon_var, "weights___")]}
  else {df = df[c(dependent, "weights___")]}
  df = na.omit(df) #remove missing

  #check if dists and df match in size
  if (!missing("dists")) {
    if (!all(nrow(df) == dim(dists))) stop("Size of the distance matrix does not match the data.frame's number of cases. This could be due to missing values.")
  }

  #check k
  if (!k < nrow(df)) stop("k must be smaller than the number of complete cases!")

  #distances
  #spherical
  if (missing("dists")) {
    #check method
    if (!distance_method %in% c("spherical", "euclidean")) stop("Distance method unrecognized!")

    #calculate
    if (distance_method == "spherical") dists = get_spherical_dists(df, lat_var, lon_var, output = "matrix")
    if (distance_method == "euclidean") dists = get_euclidean_dists(df[c(lat_var, lon_var)], output = "matrix")
  }


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
  if (output == "cor") return(cor(df_return[["y_hat"]], df_return[[dependent]], use = "p"))
  if (output == "resids"){
    model = str_c(dependent, " ~ y_hat")
    fit = lm(model, df_return, weights = weights___, na.action = na.exclude)
    return(resid(fit))
  }

}


#' Calculate multiple spatial autocorrelation measures.
#'
#' Returns a data.frame with measures of SAC using Moran's I, CD, CD_sqrt and KNSNR.
#' @param df A data.frame with variables.
#' @param vars A character vector with the names of the variables for which SAC measures should be calculated.
#' @param dists A matrix of distances between cases.
#' @param lat_var A string with the name of the variable which has the latitude/east-west data.
#' @param lon_var A string with the name of the variable which has the longitude/north-south data.
#' @param distance_method Which geometric system to use to calculate distances. Defaults to spherical. Can be either spherical or euclidean. If using euclidean it doesn't matter which variable is coded as lat or lon.
#' @param k A vector of k values to use for knsnr. Defaults to 3.
#' @param weights_var A string with the name of the variable which has the case weights. Optional.
#' @param weight_method A string with the weighing method to use. Defaults to harmonic.
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @keywords spatial autocorrelation, latitude, longitude, distance, Moran's I, wrapper
#' @export
#' @examples
#' get_SAC_measures()
get_SAC_measures = function(df, vars, dists, lat_var, lon_var, distance_method, k = 3, weights_var="", weight_method="harmonic", auto_detect_dist_method=T) {

  #autodetect distance method
  if (!missing("dists")) auto_detect_dist_method=F
  if(auto_detect_dist_method) {
    auto = distance_method_detector(df)
    distance_method = auto[1]
    lat_var = auto[2]
    lon_var = auto[3]
  }

  #weights
  if (weights_var=="") df$weights___ = rep(1, nrow(df)) #if none, use 1's

  #data.frame for results
  df_ret = data.frame(matrix(nrow=length(vars), ncol=0)) #df for storing results
  rownames(df_ret) = vars

  #subset
  if (missing("dists")) {
    df = df[c(vars, lat_var, lon_var, "weights___")]
  }
  df = na.omit(df) #remove missing

  #Moran's I
  morans = get_Morans_I_multi(df=df, vars=vars, dists=dists, lat_var=lat_var, lon_var=lon_var, distance_method=distance_method)
  df_ret$Morans_I = morans

  #correlation of distances
  df_dist = get_distances(df=df, dists=dists, lat_var=lat_var, lon_var=lon_var, distance_method=distance_method, weights_var=weights_var, weight_method=weight_method)

  cd = cor(df_dist)["spatial", vars]
  df_ret$cd = cd
  df_ret$cd_sqrt = cd %>% sqrt

  #knsnr
  for (k_ in k) {
    for (var in vars) {
      knsnr = knsn_reg(df=df, dependent=var, k=k_, dists=dists, lat_var=lat_var, lon_var=lon_var, weights_var=weights_var, distance_method=distance_method, output = "cor")
      df_ret[var, str_c("knsn_", k_)] = knsnr
    }
  }

  return(df_ret)
}



#' Autodetect distance method based on variable names.
#'
#' Returns a vector of the autodetected values or raises an error if it fails.
#' @param df A data.frame with variables.
#' @keywords latitude, longitude, distance
#' @export
#' @examples
#' get_SAC_measures()
distance_method_detector = function(df) {
  #autodetect distance method

  #spherical
  if (all(c("lat", "lon") %in% colnames(df))) {
    distance_method = "spherical"
    lat_var = "lat"
    lon_var = "lon"
    return(c(distance_method, lat_var, lon_var))
  }

  #euclidean
  if (all(c("x", "y") %in% colnames(df))) {
    distance_method = "euclidean"
    lat_var = "x"
    lon_var = "y"
    return(c(distance_method, lat_var, lon_var))
  }

  stop("Could not detect the distance method!")
}

### This has some functions related to spatial autocorrelation analysis

#' Calculate pairwise spherical distances for all pairs.
#'
#' Returns a vector with the pairwise spherical distances for all pairs.
#' @param df A data.frame with variables.
#' @param lat_var A numeric vector with the latitudes.
#' @param lon_var A numeric vector with the longitudes.
#' @keywords spatial autocorrelation, latitude, longitude, distance
#' @export
#' @examples
#' get_spherical_dists()
get_spherical_dists = function(df, lat_var = "lat", lon_var = "lon") {
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

  #find combinations
  combs = combinations(nrow(df), 2) #pick 2 out of nrow without order

  #find distances
  geo_dist = as.vector(distCosine(df[combs[, 1], 2:1], df[combs[, 2], 2:1]))
  #bizarrely, the authors of the distCosine has swapped the order of lat and lon which made me waste 30 mins bug hunting!

  #return
  return(geo_dist)
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
#' @param df A data.frame with variables.
#' @param lat_var A numeric vector with the latitudes. Defaults to "lat".
#' @param lon_var A numeric vector with the longitudes. Defaults to "lon".
#' @param weights_var A numeric vector with the weights.
#' @param weight_method A character string indicating which averaging method to use when combining weights for cases. Defaults to harmonic mean. Other options are arithmic and geometric.
#' @keywords spatial autocorrelation, latitude, longitude, distance, weights
#' @export
#' @examples
#' get_distances()
get_distances = function(df, lat_var = "lat", lon_var = "lon", weights_var = "", weight_method = "harmonic") {
  #purpose is to input a data.frame and then automatically get the distances back for every combination of cases, both for standard variables and for spatial variables (latitude, longitude)

  #is weights var there?
  if (!weights_var %in% colnames(df) & weights_var != "") stop("Weights variable isn't in the data.frame!")

  #remove missing data
  if (any(is.na(df))) message("Warning, data.frame contained cases with missing values. These cases were excluded!")
  df = na.omit(df)

  library(plyr)
  #for llply

  library(stringr)
  #for str_detect

  library(gtools)
  #for combinations

  #geo dist
  geo = F

  #are there any spatial variables?
  if (lat_var %in% colnames(df) & lon_var %in% colnames(df)) {
    #subset latlon vars
    df_latlon = df[c(lat_var, lon_var)]

    #get spherical distances
    df_latlon_dist = get_spherical_dists(df_latlon, lat_var = lat_var, lon_var = lon_var)

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

  #get dists for all other variables
  df_dist = as.data.frame(llply(df, function(x) dist(x) %>% as.vector))

  #add spatial dists if they exist
  if (geo) df_dist["spatial"] = df_latlon_dist

  #add weights if they exist
  if (weights_var != "") df_dist["weight"] = df_pair_weights

  #return
  return(df_dist)
}



### This file has various helper

#' Check for input for spatial data.
#'
#' Returns a list with the results of the check. This is generally only used as a helper function for other spatial statistics functions.
#' @param df A data.frame with variables.
#' @param dists A matrix of distances between cases.
#' @param lat_var A string with the name of the variable which has the latitude/east-west data.
#' @param lon_var A string with the name of the variable which has the longitude/north-south data.
#' @param distance_method Which geometric system to use to calculate distances. Defaults to spherical. Can be either spherical or euclidean. If using euclidean it doesn't matter which variable is coded as lat or lon.
#' @param auto_detect_dist_method Whether to try to autodetect the distance method. If the dataset contains variables with the names "lat" and "lon", it will be detected as spherical. If it contains "x" and "y", it will be detected as euclidean. Defaults to true.
#' @export
#' @examples
#' SAC_slr()
check_spatial_input = function(df, dists, lat_var, lon_var, distance_method, auto_detect_dist_method) {
  # browser()
  #dists missing?
  if (missing("dists")) no_dists=T

  #can spatial be autodetected?
  if (auto_detect_dist_method) {
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

  #both coords missing?
  if (missing("lat_var") & missing("lon_var")) no_latlon=T

  ##Actions

  #none present
  if (exists("no_dists") & exists("no_latlon")) {
    return(list(setting = "na"))
  }

  #dists found
  if (!exists("no_dists")) {
    return(list(setting = "dists"))
  }

  #spatial found
  if (exists("no_dists") & !exists("no_latlon")) {
    #check if they exist
    if (!all(c(lat_var, lon_var) %in% colnames(df))) {
      stop("Spatial variables given were not in the data.frame!")
    }

    return(list(setting = "coords",
                distance_method=distance_method,
                lat_var=lat_var,
                lon_var=lon_var))
  }
}

### This file has various helper functions for SAC


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
check_spatial_input = function(df, dists = NULL, lat_var = NULL, lon_var = NULL, distance_method = NULL, auto_detect_dist_method = T) {

  #input
  df

  #dists
  if (!is.null(dists)) {
    return(list(setting = "dists"))
  }

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
  if (is.null(lat_var) && is.null(lon_var)) return(list(setting = "na"))

  #found coords
  return(list(setting = "coords",
              distance_method=distance_method,
              lat_var=lat_var,
              lon_var=lon_var))
}



#' Autodetect spatial distance method based on variable names.
#'
#' Returns a vector of the autodetected values or raises an error if it fails.
#' @param df A data frame.
#' @export
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

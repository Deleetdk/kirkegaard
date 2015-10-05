### This file has various internal functions

check_spatial_input = function(df, dists, lat_var, lon_var, distance_method, auto_detect_dist_method) {
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
  if (!exists("no_dists") & exists("no_latlon")) {
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

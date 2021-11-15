
#' Add spatial k nearest neighbor (sknn) values
#'
#' @param x A data frame with variables for lon and lat
#' @param vars A vector of variables to regress with sknn
#' @param k The value for 3, default 3
#' @param lon The lon variable
#' @param lat The lat variable
#' @param suffix The suffix to use, default "_lag"
#'
#' @return A data frame with added variables
#' @export
#'
#' @examples
spatial_knn = function(x, vars, k = 3, lon = "lon", lat = "lat", suffix = "_lag") {
  #fail on bad input data
  if (!is.data.frame(x)) stop("`x` must be a data frame")
  if (!lon %in% names(x)) stop("`lon` ({lon}) not in data frame", call. = F)
  if (!lat %in% names(x)) stop("`lat` ({lat}) not in data frame", call. = F)
  for (v in vars) {
    if (!v %in% names(x)) {
      stop(str_glue("`{v}` not in data frame"), call. = F)
    }
  }

  #make the distance matrix
  #drop sf class if need be
  if ("sf" %in% class(x)) {
    x2 = x %>% sf::st_drop_geometry()
  } else {
    x2 = x
  }
  dist_mat = x2 %>% select(!!lon, !!lat) %>% as.matrix()
  spatial_dists = terra::distance(x = dist_mat, y = dist_mat, lonlat = T)

  #loop variables
  for (v in vars) {

    #subset distance matrix to subset without missing data
    v_nomiss = x[[v]] %>% is.na() %>% `!`()
    lonlat_nomiss = (miss_by_case(dist_mat) == 0)
    combined_nomiss = v_nomiss & lonlat_nomiss
    spatial_dists_subset = spatial_dists[combined_nomiss, combined_nomiss]

    #get the neighbors
    neighbors <- apply(spatial_dists_subset, 1, order)
    # we need to transpose the result
    neighbors <- t(neighbors)

    #get k neighbors, columns
    k_neighbors = neighbors[, 2:(k+1)]

    #average values of the neighbors
    #make a matrix of values to average
    #then get those values and average and insert in the right spot
    x[[v + suffix]][combined_nomiss] = x[[v]][combined_nomiss][k_neighbors] %>% matrix(ncol = k) %>% rowMeans()
  }

  x
}


#' Compute correlatoins for lag variables
#'
#' @param x A data frame
#' @param suffix The suffix on the lag variables
#'
#' @return A named vector
#' @export
#'
#' @examples
spatial_lag_cors = function(x, suffix = "_lag", long_output = F) {
  #find pairs of variables with _lag suffix and without
  vars_to_cor = map_lgl(names(x), function(v) {
    (v %in% names(x)) & ((v + suffix) %in% names(x))
  })
  vars_to_cor = names(x)[vars_to_cor]

  if (!long_output) {
    #loop and cor
    map_dbl(vars_to_cor, function(v) {
      cor(x[[v]], x[[v + suffix]], use = "pairwise")
    }) -> y

    names(y) = vars_to_cor

    return(y)
  } else {
    #loop and cor
    # browser()
    map_df(vars_to_cor, function(v) {
      cor.test(x[[v]], x[[v + suffix]], na.action = na.omit) %>%
        broom::tidy() %>%
        mutate(var = v, n = parameter + 2) %>%
        select(var, everything()) %>%
        select(-method, -alternative, -parameter) %>%
        rename(r = estimate)
    }) -> y

    return(y)
  }

}


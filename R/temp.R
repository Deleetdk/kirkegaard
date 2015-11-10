# SAC_SEVM = function(df, vars, dists, lat_var, lon_var, distance_method, auto_detect_dist_method=T) {
#   library(psych)
#   library(nFactors)
#   library(stringr)
#   library(ade4)
#
#   #check input
#   if (missing("df")) {
#     message("No variables given, assuming all variables.")
#     vars = colnames(df)
#   }
#   if (missing("vars")) stop("No variables given!")
#   if (anyNA(df)) stop("Missing values present. Remove/impute and try again.")
#
#   #check spatial input
#   check_results = check_spatial_input(df=df, dists=dists, lat_var=lat_var, lon_var=lon_var, distance_method=distance_method, auto_detect_dist_method=auto_detect_dist_method)
#
#   #if no spatial info
#   if(check_results$setting == "na") {
#     stop("No spatial information detected!")
#   }
#
#   #coords
#   if(check_results$setting == "coords") {
#     distance_method = check_results$distance_method
#     lat_var = check_results$lat_var
#     lon_var = check_results$lon_var
#
#     #get distances
#     dists = get_distances_mat(df[c(lat_var, lon_var)], lat_var = lat_var, lon_var = lon_var, distance_method = distance_method)[[1]]
#   }
#
#   browser()
#   #how many factors
#   dists = 1/dists
#   diag(dists) = 0
#   dists = as.data.frame(dists)
#   n_fact = nScree(dists)[[1]] %>% unlist %>% max
#   dists_r = cor(dists)
#
#   #extract
#   spatial_fa = fa(dists_r, nfactors = n_fact)
#   #spatial_fa = fa(dists, nfactors = n_fact)
#   fa_loadings = spatial_fa$loadings
#
#   #score cases uses loadings
#   fa_scores_df = as.data.frame(matrix(ncol = n_fact, nrow = nrow(df)))
#   for (i in 1:nrow(fa_scores_df)) {
#     fa_scores_df[i, ] = fa_loadings[i, ] * dists[, i] %>% sum
#   }
#
#   colnames(fa_scores_df) = str_c("PC", 1:ncol(fa_scores_df), "___")
#   fa_names = colnames(fa_scores_df)
#
#   #make temp df
#   tmp_df = merge_datasets(df[vars], fa_scores_df)
#
#   #residualize
#   tmp_df_res = residualize_DF(tmp_df, resid.vars = fa_names, return.resid.vars = F, print.models = F)
#   cor(tmp_df_res)
#   return(tmp_df_res)
# }
#
# SAC_SEVM(d_ex5, c("predictor", "outcome")) %>% cor
# SAC_SEVM(d_ex6, c("predictor", "outcome")) %>% cor

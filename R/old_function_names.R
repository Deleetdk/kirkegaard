### old names for functions
#these are just small redirection functions
subset_by_pattern = function(...) stop("subset_by_pattern changed name to df_subset_by_pattern")
remove_NA_vars = function(...) stop("remove_NA_vars changed name to df_remove_NA_vars")
plot_loadings = function(...) stop("This function is depreciated. Use FA_plot_loadings.")
plot_loadings_multi = function(...) stop("This function is depreciated. Use FA_plot_loadings.")
merge_datasets2 = function(...) stop("This function has been moved to merge_datasets")
merge_datasets2_multi = function(...) stop("This function has been moved to merge_datasets_multi")
suppressor = function(...) stop("This function has been moved to silence")
plot_miss = function(...) stop("This function has been moved to miss_plot")
df_addNA = function(...) stop("This function has been moved to miss_add_random")
filter_by_missing_values = function(...) stop("This function has been moved to miss_filter")
MOD_repeat_cv_glmnet = function(...) stop("This function has been moved to MOD_LASSO")
SAC_knsn_reg = function(...) stop("This function has been moved to SAC_knsnr")
get_SAC_measures = function(...) stop("This function has been moved to SAC_measures")
rcorr2 = function(...) stop("Don't use this function.")
SAC_control_all_methods = function(...) stop("This function has been moved to SAC_control")
plot_kmeans = function(...) stop("This function has been moved to GG_kmeans")
Jensen_plot = function(...) stop("This function has been deleted, use fa_Jensens_method")
Jensens_method = function(...) stop("This function has been moved to fa_Jensens_method")



# old FA_ -----------------------------------------------------------------
#these all changed a prefix
FA_MAR = function(...) stop("All FA_ function have been changed to fa_")
FA_residuals = FA_MAR
FA_CFS = FA_MAR
FA_all_methods = FA_MAR
FA_mixedness = FA_MAR
FA_rank_fa = FA_MAR
FA_robust_cormatrix = FA_MAR
FA_robust_fa = FA_MAR
FA_CAFL = FA_MAR
FA_splitsample_repeat = FA_MAR
FA_congruence_matrix = FA_MAR
FA_plot_loadings = FA_MAR
FA_Jensens_method = FA_MAR


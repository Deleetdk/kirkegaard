### old names for functions
#these are just small redirection functions

#function generator
defunct = function(msg = "This function is depreciated") function(...) return(stop(msg))


# other stuff -------------------------------------------------------------

#' @export
suppressor = defunct("This function has been moved to silence")

#' @export
rcorr2 = defunct("Don't use this function.")

#' @export
plot_kmeans = defunct("This function has been moved to GG_kmeans")

#' @export
as_abbrev = defunct("Depreciated. use pu_translate")

#' @export
as_long = defunct("Depreciated. use pu_translate")

#' @export
check_missing = defunct("Depreciated. Don't rely on missingness.")


# spatial -----------------------------------------------------------------
#' @export
SAC_control_all_methods = defunct("This function has been moved to SAC_control")

#' @export
SAC_knsn_reg = defunct("This function has been moved to SAC_knsnr")

#' @export
get_SAC_measures = defunct("This function has been moved to SAC_measures")

# missing related ---------------------------------------------------------

#' @export
plot_miss = defunct("This function has been moved to miss_plot")

#' @export
filter_by_missing_values = defunct("This function has been moved to miss_filter")


# df_ related ---------------------------------------------------------------------

#' @export
std_df = defunct("This function has been moved to df_standardize")

#' @export
merge_datasets2 = defunct("This function has been moved to merge_datasets")

#' @export
merge_datasets2_multi = defunct("This function has been moved to merge_datasets_multi")

#' @export
as_num_df = defunct("Renamed to df_as_num")

#' @export
df_func = defunct("Renamed to df_rowFunc")

#' @export
sort_df = defunct("Renamed to df_sort")

#' @export
residualize_DF = defunct("Renamed to df_residualize")

#' @export
merge_rows = defunct("Renamed to df_merge_rows")

#' @export
round_df = defunct("Renamed to df_round")

#' @export
rank_df = defunct("Renamed to df_rank")

#' @export
merge_rows_by_name = defunct("Depreciated. Use df_merge_rows")

#' @export
t_df = defunct("Renamed to df_t")

#' @export
reorder_columns = defunct("Renamed to df_reorder_columns")

#' @export
df_remove_vars = defunct("Renamed to df_remove")

#' @export
df_rename_vars = defunct("Renamed to df_rename")

#' @export
add_id = defunct("Renamed to df_add_id")

#' @export
copy_columns = defunct("Function has been deleted. Use cbind. If you need the pattern, then use df_subset_by_pattern and cbind.")
df_copy_columns = defunct("Function has been deleted. Use cbind. If you need the pattern, then use df_subset_by_pattern and cbind.")

#' @export
subset_by_pattern = defunct("subset_by_pattern changed name to df_subset_by_pattern")

#' @export
remove_NA_vars = defunct("remove_NA_vars changed name to df_remove_NA_vars")

#' @export
df_addNA = defunct("This function has been moved to miss_add_random")

# MOD_ --------------------------------------------------------------------

#' @export
lm_beta_matrix = defunct("This function has been moved to MOD_APSLM")

#' @export
MOD_repeat_cv_glmnet = defunct("This function has been moved to MOD_LASSO")

#' @export
lm_CI = defunct("This function has been moved to MOD_summary")


# old FA_ -----------------------------------------------------------------
#these all changed a prefix

#' @export
FA_MAR = function(...) stop("All FA_ function have been changed to fa_")

#' @export
FA_residuals = FA_MAR

#' @export
FA_CFS = FA_MAR

#' @export
FA_all_methods = FA_MAR

#' @export
FA_mixedness = FA_MAR

#' @export
FA_rank_fa = FA_MAR

#' @export
FA_robust_cormatrix = FA_MAR

#' @export
FA_robust_fa = FA_MAR

#' @export
FA_CAFL = FA_MAR

#' @export
FA_splitsample_repeat = FA_MAR

#' @export
FA_congruence_matrix = FA_MAR

#' @export
FA_plot_loadings = FA_MAR

#' @export
FA_Jensens_method = FA_MAR

#' @export
plot_loadings = defunct("This function is depreciated. Use fa_plot_loadings.")

#' @export
plot_loadings_multi = defunct("This function is depreciated. Use fa_plot_loadings.")

#' @export
Jensen_plot = defunct("This function has been deleted, use fa_Jensens_method")

#' @export
Jensens_method = defunct("This function has been moved to fa_Jensens_method")

#' @export
remove_redundant_vars2 = defunct("This function has been moved to remove_redundant_vars, and the old function deleted")

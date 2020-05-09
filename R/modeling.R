## VARIOUS MODELING FUNCTIONS



# p_to_asterisk -----------------------------------------------------------

#' Add asterisks to p values
#'
#' @param x p values vector
#' @param asterisks Thresholds
#' @param digits Number of digits
#' @param asterisks_only Only print asterisks
#'
#' @return character vctor
#' @export
#'
#' @examples
#' c(.123, .009, .004, .0009, .0001) %>% p_to_asterisk()
#' c(.123, .009, .004, .0009, .0001) %>% p_to_asterisk(asterisks = c(.05, .01))
#' c(.123, .009, .004, .0009, .0001) %>% p_to_asterisk(asterisks = c(.05))
#' c(.123, .009, .004, .0009, .0001) %>% p_to_asterisk(asterisks = c())
#' c(.123, .009, .004, .0009, .0001) %>% p_to_asterisk(asterisks = NULL)
#' c(.123, .009, .004, .0009, .0001) %>% p_to_asterisk(asterisks_only = T)
p_to_asterisk = function(x, asterisks = c(.01, .005, .001), digits = max(count_decimals(asterisks)), asterisks_only = F) {
  # browser()
  #do nothing if not desired
  if (is.null(asterisks)) return(x)
  x2 = round(x, digits = digits)
  y = x2 %>% as.character()
  min_val = 1/10^digits



  #add asterisks
  for (i in seq_along(asterisks)) {
    idx = x < asterisks[i]
    idx_zero = (x < min_val)
    asts = str_c(rep('*', i), collapse = '')

    #if rounded to 0, then use less than minimal value
    y[idx] = str_glue("{x2[idx]}{asts}")
    y[idx_zero] = str_glue("<{min_val}{asts}")
  }

  #asterisk only
  if (asterisks_only) {
    y = str_match(y, "(\\*+)")[, 2]
    y[is.na(y)] = ""
  }

  y
}


# summarize_models -------------------------------------------------------------------------

#internal helper functions
get_adj_r2 = function(x) {
  map_dbl(x, function(m) {
    summary.lm(m)$adj.r.squared
  })
}

get_n = function(x) {
  map_dbl(x, function(m) {
    length(m$residuals)
  })
}

#' Concise model comparison summary table
#'
#' @param x List of models
#' @param asterisks Thresholds for asterisks
#' @param asterisks_only Asterisks only? If false, then print rounded p values as well
#' @param beta_digits Digits for betas
#' @param beta_se_digits Digits for beta SEs
#' @param p_digits Digits for p values
#' @param collapse_nonlinear Hide uninterpretable betas for nonlinear terms
#' @param nonlinear_text If `collapse_nonlinear`, then what text to use
#'
#' @return Data frame
#' @export
#'
#' @examples
#' models = list(
#' lm(Sepal.Width ~ Sepal.Length, data = iris),
#' lm(Sepal.Width ~ Sepal.Length + Petal.Width, data = iris),
#' lm(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris)
#' )
#' summarize_models(models)
#' summarize_models(models, asterisks = c(.05))
#' summarize_models(models, asterisks_only = F)
summarize_models = function(x, asterisks = c(.01, .005, .001), asterisks_only = T, beta_digits = 2, beta_se_digits = beta_digits + 1, p_digits = 3, collapse_nonlinear = T, nonlinear_text = "(nonlinear)") {

  #names?
  if (is.null(names(x))) names(x) = 1:length(x) %>% factor()

  #loop and make consise table
  y = map2_df(x, names(x), function(m, name) {
    #get tidy output
    y = m %>%
      summary.lm() %>%
      broom::tidy() %>%
      mutate(model = name)

    y
  })

  #order of variable appearance
  term_order = unique(y$term)

  #nice numeric values
  #if asterisks wanted, add them
  if (!is.null(asterisks)) {
    y = y %>% mutate(
      beta = str_glue("{format_digits(estimate, beta_digits)} ({format_digits(std.error, beta_se_digits)}, {p_to_asterisk(p.value, asterisks = asterisks, asterisks_only = asterisks_only)})")
    )

    #if no p values, remove comma whitespace
    if (asterisks_only) y$beta = str_replace(y$beta, ", ", "")

  } else { #just round p values
    y = y %>% mutate(
      beta = str_glue("{format_digits(estimate, beta_digits)} ({format_digits(std.error, beta_se_digits)}, {str_zero_to_lt(p.value, digits = p_digits)})")
    )
  }

  #subset to wanted cols
  y = y %>% dplyr::select(term, beta, model)

  #factor levels, so that we spread in the same order as input
  y$model = factor(y$model, levels = names(x))

  #collapse nonlinear terms?
  if (collapse_nonlinear && any(str_detect(term_order, "'$"))) {
    y = plyr::ddply(y, "model", function(dd) {
      #find nonlinear terms
      nonlinear_clean_terms = dd$term %>% str_subset("'$") %>% str_replace("'+", "") %>% unique()

      #which rows are the non-1st? remove them
      y = dd
      y = y[!str_detect(y$term, "'$"), ]

      #then replace the clean one with dummy
      y[y$term %in% nonlinear_clean_terms, "beta"] = nonlinear_text

      y
    })
  }

  #spread
  y2 = y %>%
    #spread
    tidyr::spread(key = model, value = beta) %>%
    #put variables in right order, same as their order of appearance in models
    dplyr::mutate(term = factor(term, levels = term_order)) %>%
    dplyr::arrange(term)

  #reset to chr so we can add more rows
  y2$term %<>% as.character()

  #fix top left column name
  names(y2)[1] = "Predictor/Model"

  #add summary stats to end
  y2 = rbind(
    y2,
    c("R2 adj.", get_adj_r2(x) %>% format_digits(3)),
    c("N", get_n(x))
  )

  #attribute for asterisks
  attr(y2, "asterisks") = map2_chr(asterisks, seq_along(asterisks), function(v, i) {
    str_glue("<{format(v, digits = count_decimals(v))}{str_c(rep('*', i), collapse = '')}")
  })

  #to normal df for printing the attribute
  y2
}


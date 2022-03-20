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
    y[idx] = str_glue("{x2[idx]}{asts}") %>% as.character()
    y[idx_zero] = str_glue("<{min_val}{asts}") %>% as.character()
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
#' @param collapse_factors Whether to collapse factors or not. Can be a logical, or a string of factor names to collapse.
#' @param add_ref_level Whether to add reference levels for factors
#' @param ref_class_text In case of adding reference levels, which text to fill in
#'
#' @return Data frame
#' @export
#'
#' @examples
#' #default lm fits
#' models = list(
#' lm(Sepal.Width ~ Sepal.Length, data = iris),
#' lm(Sepal.Width ~ Sepal.Length + Petal.Width, data = iris),
#' lm(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris)
#' )
#' summarize_models(models)
#' summarize_models(models, asterisks = c(.05))
#' summarize_models(models, asterisks_only = F)
summarize_models = function(x, asterisks = c(.01, .005, .001), asterisks_only = T, beta_digits = 2, beta_se_digits = beta_digits + 1, p_digits = 3, collapse_nonlinear = T, nonlinear_text = "(nonlinear)", collapse_factors = F, add_ref_level = T, ref_class_text = "(ref)") {

  #names?
  if (is.null(names(x))) names(x) = 1:length(x) %>% factor()

  #ensure model names are factor
  if (is.character(names(x))) names(x) = factor(names(x), levels = names(x))

  #loop and make consise table
  y = map2_df(x, names(x), function(m, name) {
    # browser()
    #get tidy output
    #if its lm or rms
    class_m = class(m)
    if ("rms" %in% class_m) {

      y = m %>%
        summary.lm() %>%
        broom::tidy() %>%
        mutate(model = name)
    } else if ("rlm" %in% class_m) {

      #need to make the p value manually
      y = m %>%
        broom::tidy() %>%
        mutate(
          p.value = abs(statistic) %>% pt(df = summary(m)$df[2], lower.tail = F),
          model = name
        )

    } else if ("lm" %in% class_m) {
      y = m %>%
        broom::tidy() %>%
        mutate(model = name)
    } else { #YOLO
      y = m %>%
        broom::tidy() %>%
        mutate(model = name)
    }

    y
  })


  #add reference levels?
  #add clean version
  y$clean_term = y$term %>% str_replace("=.+", "") %>% factor(levels = unique(.))
  y$factor = y$term %>% str_detect("=")
  y$ref = F #prefill with F
  if (add_ref_level) {
    y = plyr::ddply(y, c("model", "clean_term"), function(dd) {
      #is it a factor? if not, return as is
      if (!dd$factor[1]) return(dd)

      #if factor, then get the ref level from model fit

      fct_levels = x[[dd$model[1]]]$Design$parms[[dd$clean_term[1] %>% as.character()]]

      #add ref level
      bind_rows(
        tibble(
          term = dd$clean_term[1] + "=" + fct_levels[1],
          estimate = 0, #we will overwrite this later
          p.value = 0,
          clean_term = dd$clean_term[1],
          model = dd$model[1],
          ref = T
        ),
        dd
      )
    })
  }



  #order of variable appearance
  term_order = y$term %>% unique()

  #deal with factors that have been reduced
  tibble(
    term = term_order,
    term_clean = term %>% str_replace("=.+", "") %>% factor(levels = unique(.)),
    factor = term %>% str_detect("=")
  ) %>%
    plyr::ddply("term_clean", function(dd) {
      # browser()
      #any factors?
      #if not, we don't need to do anything
      if (!any(dd$factor)) return(dd)

      #where are factors?
      factors = dd$factor %>% which()

      #if yes, add a dummy row in front
      bind_rows(
        dd[0:(min(factors)-1), ],
        dd[min(factors), ] %>% mutate(term = term_clean),
        dd[min(factors):nrow(dd), ]
      ) ->
        y

      y
    }) %>%
    pull(term) ->
    term_order




  #nice numeric values
  #if asterisks wanted, add them
  if (!is.null(asterisks)) {
    y = y %>% mutate(
      beta = str_glue("{str_round(estimate, beta_digits)} ({str_round(std.error, beta_se_digits)}, {p_to_asterisk(p.value, asterisks = asterisks, asterisks_only = asterisks_only)})") %>% as.character()
    )

    #if no p values, remove comma whitespace
    if (asterisks_only) y$beta = str_replace(y$beta, ", ", "")

  } else { #just round p values
    y = y %>% mutate(
      beta = str_glue("{str_round(estimate, beta_digits)} ({str_round(std.error, beta_se_digits)}, {str_round(p.value, digits = p_digits)})") %>% as.character()
    )
  }




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

  #replace beta with ref class text
  y$beta = case_when(
    y$ref ~ ref_class_text,
    T ~ y$beta
  )

  #collapse factors?
  if (collapse_factors == T || is.character(collapse_factors)) {
    #which rows to collapse?
    #find factor terms to collapse
    if (is.logical(collapse_factors)) {
      factor_clean_terms = y$term %>% str_subset("=") %>% str_replace("=.+", "") %>% unique()
    } else {
      factor_clean_terms = collapse_factors
    }

    #loop across models
    y = plyr::ddply(y, "model", function(dd) {

      #if no factors, return what we have
      if (!any(str_detect(dd$term, "="))) return(dd)

      #if no factors in this model to clean, return what we have

      if (!any(dd$clean_term %in% factor_clean_terms)) return(dd)

      #which rows are the non-1st? remove them
      non_first_rows = map(factor_clean_terms, function(term) {
        #which rows have it?
        term_rows = dd$term %>% str_detect("^" + term + "=") %>% which()
        term_rows[-1]
      }) %>% c(recursive = T)
      y = dd[-c(non_first_rows), ]



      #replace beta with placeholder for the desired rows
      y[y$term %>% str_detect("=") & y$clean_term %in% factor_clean_terms, "beta"] = "(yes)"

      #remove levels when need to
      y$term = case_when(
        y$clean_term %in% factor_clean_terms ~ y$term %>% str_replace("=.+", ""),
        TRUE ~ y$term
      )

      y
    })
  }

  #subset to wanted cols
  y = y %>% dplyr::select(term, beta, model)

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
    c("R2 adj.", get_adj_r2(x) %>% str_round(3)),
    c("N", get_n(x))
  )

  #attribute for asterisks
  attr(y2, "asterisks") = map2_chr(asterisks, seq_along(asterisks), function(v, i) {
    str_glue("<{format(v, digits = count_decimals(v))}{str_c(rep('*', i), collapse = '')}") %>% as.character()
  })

  #to normal df for printing the attribute
  y2
}




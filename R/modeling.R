## VARIOUS MODELING FUNCTIONS




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
#' p_to_asterisk(NA)
p_to_asterisk = function(x, asterisks = c(.01, .005, .001), digits = max(count_decimals(asterisks)), asterisks_only = F) {

  #do nothing if not desired
  if (is.null(asterisks)) return(x)
  x2 = round(x, digits = digits)
  y = x2 %>% as.character()
  min_val = 1/10^digits



  #add asterisks
  for (i in seq_along(asterisks)) {
    idx = (x < asterisks[i]) %>% NA_to_F() #NA as F here
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



#internal helper functions
get_adj_r2 = function(x) {
  map_dbl(x, function(m) {
    suppressWarnings(summary.lm(m)$adj.r.squared)
  })
}

get_n = function(x) {
  map_dbl(x, function(m) {
    length(m$residuals)
  })
}

clean_term = function(x) {
  map_chr(x, function(z) {
    #split it if we need to
    z_split = str_split(z, pattern = " \\* ", simplify = T)

    #any splits? if no
    if (length(z_split) == 1) {
      #clean and return
      z2 = z %>% str_replace("=.+", "")
      return(z2)

    } else {
      #clean, concat, return
      z2 = z_split %>% str_replace("=.+", "") %>% str_c(collapse = " * ")
      return(z2)
    }
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

  #ensure no duplicates
  if (any(duplicated(names(x)))) {
    names(x) = str_uniquify(names(x), suffix = "%d")
  }

  #ensure model names are factor
  if (is.character(names(x))) names(x) = factor(names(x), levels = names(x))

  #loop and make concise table
  y = map2_df(x, names(x), function(m, name) {

    #get tidy output
    #if its lm or rms
    class_m = class(m)
    if ("rms" %in% class_m) {

      y = m %>%
        {
          #prevent spurious warnings
          suppressWarnings(summary.lm(.))
        } %>%
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

  y$clean_term = y$term %>%
    clean_term() %>%
    factor(levels = unique(.))


  y$factor = y$term %>% str_detect("=")

  #add ref levels
  y$ref = F #prefill with F
  if (add_ref_level) {

    y = plyr::ddply(y, c("model", "clean_term"), function(dd) {
      #is it a factor? if not, return as is
      if (!dd$factor[1]) return(dd)

      #how many factors are there? for each factor, we have to find the ref level
      #we can find factors by splitting variables by *
      vars = str_split(dd$clean_term[1], pattern = " \\* ", simplify = T)

      vars_ref_level = map_chr(vars, function(v) {
        #get from model terms
        terms_sub = y %>%
          filter(!!v == clean_term, factor == T)

        #if non-0 length, find ref level in model design object
        #if length 0, return NA
        if (nrow(terms_sub) == 0) return(NA_character_)

        #this model's design object
        terms_model = x[[terms_sub$model[1]]]$Design

        terms_model$parms[[as.character(terms_sub$clean_term[1])]][1]
      })
      #for each variable, check if it is a factor, and if so, get the base level

      #add ref level if this is a simple factor, not an interaction
      fct_count = length(na.omit(vars_ref_level))
      if (fct_count > 1) return(dd)

      #add it
      bind_rows(
        tibble(
          term = dd$clean_term[1] + "=" + str_c(na.omit(vars_ref_level), collapse = ", "),
          estimate = 0, #we will overwrite this later
          p.value = NA_real_,
          clean_term = dd$clean_term[1],
          model = dd$model[1],
          ref = T,
          factor = T
        ),

        dd
      )

    }) #end add ref levels loop
  } #end add ref levels chuck



  #order of variable appearance
  term_order = y$term %>% unique()

  #add extra preceding levels for factors without levels (factor collapsed)
  # browser()
  tibble(
    term = term_order,
    term_clean = term %>% clean_term() %>% factor(levels = unique(.)),
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

    #if no p values, remove comma white space
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

  #add spacing to = for factors
  y2$term %<>% str_replace_all("=", " = ")

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
  y2 %>% as_tibble()
}


#get coefficients from list of models
#' Extract model coefficients from a list of models
#'
#' Uses `broom::tidy` to extract coefficients from models. If the model doesn't support this, it will fail. There is a special case for `rlm` models, where the p value is calculated manually. If the model is of class `rms`, then the `summary.lm` function is used to extract coefficients. There is some special handling for factor levels, where the levels are made nicer, but this is not done for `rms` models.
#'
#' @param models A list of models
#' @param conf.level Which confidence level to use. Default is .95.
#' @param nicer_factor_levels Whether to make factor levels nicer. Default is TRUE.
#'
#' @return Data frame
#' @export
#'
#' @examples
#' list(
#' a = lm(Sepal.Width ~ Sepal.Length, data = iris),
#' b = lm(Sepal.Width ~ Sepal.Length + Petal.Width, data = iris)
#' ) %>% get_model_coefs()
get_model_coefs = function(models, conf.level = .95, nicer_factor_levels = T) {
  if (is.null(names(models))) {
    names(models) = 1:length(models)
  }

  #extract data from models
  model_coefs = map2(
    models,
    names(models),
    function(m, name) {
      # browser()
      #get tidy output
      if ("rms" %in% class(m)) {
        y = m %>%
          {
            suppressWarnings(summary.lm(.))
          } %>%
          broom::tidy(conf.int = T, conf.level = conf.level) %>%
          mutate(model = name)
      } else if ("rlm" %in% class(m)) {
      #need to make the p value manually
      y = m %>%
        broom::tidy(conf.int = T, conf.level = conf.level) %>%
        mutate(
          p.value = abs(statistic) %>% pt(df = summary(m)$df[2], lower.tail = F),
          model = name
        ) %>%
        #reorder columns into consistent order
        dplyr::select(term, estimate, std.error, statistic, p.value, conf.low, conf.high, model)
      } else if ("lm" %in% class(m)) {
        y = m %>%
          broom::tidy(conf.int = T, conf.level = conf.level) %>%
          mutate(model = name)
      } else { #YOLO
        y = m %>%
          broom::tidy(conf.int = T, conf.level = conf.level) %>%
          mutate(model = name)
      }

      y
    }
    ) %>%
    bind_rows()

  #nicer factor levels
  if (nicer_factor_levels) {
    #skip if models are rms
    if ("rms" %in% class(models[[1]])) {
      #do nothing
    } else {
      # browser()
      #find factors
      all_terms = map_df(models, ~.x %>% terms() %>% attributes() %>% .[["dataClasses"]] %>% named_vector_to_df())
      factor_terms = all_terms %>% filter(value %in% c("factor", "ordered", "character", "logical")) %>% pull(name) %>% unique()

      #loop and make names nicer
      for (fctr in factor_terms) {
        model_coefs %<>%
          mutate(
            term = term %>% str_replace("^" + fctr, fctr + ": ")
          )
      } #end nicer factor levels

    } #end rms check

  } #end nicer factor levels

  model_coefs
}


#' Fit linear models for a set of predictors to compare their effects alone and together
#'
#' This function fits a set of models to the data, where each model includes only one predictor. It then fits a full model with all predictors. The function returns a data frame with the coefficients for each model, as well as the full model. Thus, if given p predictors, it will return p+1 model results and 2 betas for each predictor.
#'
#' @param data The data frame to use
#' @param outcome The name of the outcome variable. Must be numeric.
#' @param predictors A character vector of predictors to use.
#' @param conf.level The confidence level to use. Default is .95.
#' @param family The family to use. Default is "gaussian" (OLS), see `?glm` for more information.
#' @param additional_models A list of additional models to fit. Each element of the list should be a character vector of predictors to use. The names of the list will be used as the model names in the output.
#' @param controls A character vector of control variables to use. These will be added to all models.
#' @param keep_controls Whether to keep the values for control variables in the output. Default is TRUE.
#'
#' @return A data frame with the coefficients for each model
#' @export
#'
#' @examples
#' compare_predictors(iris, names(iris)[1], names(iris)[-1])
#' compare_predictors(mpg, names(mpg)[3], names(mpg)[-3])
#' #with additional models
#' compare_predictors(iris, names(iris)[1], names(iris)[-1], additional_models = list(petal = c("Petal.Length", "Petal.Width")))
#' #with controls
#' compare_predictors(iris, names(iris)[1], names(iris)[-c(1, 5)], controls = c("Species"))
compare_predictors = function(data, outcome, predictors, additional_models = NULL, conf.level = .95, family = gaussian, controls = NULL, keep_controls = T) {
  # browser()
  #run singular regression models
  models = predictors %>% map(function(.x) {
    #if controls are given, add them to the model
    if (!is.null(controls)) {
      fit = glm(str_glue("{outcome} ~ {.x} + {controls %>% str_c(collapse = ' + ')}"), data = data, family = family)
    } else {
      fit = glm(str_glue("{outcome} ~ {.x}"), data = data, family = family)
    }

  })
  names(models) = 1:length(models)

  #full model
  #if controls are given, add them to the model
  if (!is.null(controls)) {
    #fit full model
    models[["full"]] = glm(str_glue("{outcome} ~ {predictors %>% str_c(collapse = ' + ')} + {controls %>% str_c(collapse = ' + ')}"), data = data, family = family)
  } else {
    #fit full model
    models[["full"]] = glm(str_glue("{outcome} ~ {predictors %>% str_c(collapse = ' + ')}"), data = data, family = family)
  }

  #any additional models?
  if (!is.null(additional_models)) {
    #do they have names already?
    if (is.null(names(additional_models))) {
      names(additional_models) = "Extra model " + seq_along(additional_models)
    }

    #loop and add them
    for (i in seq_along(additional_models)) {
      models[[names(additional_models)[i]]] = glm(str_glue("{outcome} ~ {str_c(additional_models[[i]], collapse = ' + ')}"), data = data, family = family)
    }
  }


  #get all coefficients for all models, tag appropriately
  all_coefs = get_model_coefs(models, conf.level = conf.level)

  #rename the singular models
  all_coefs$model = mapvalues(
    all_coefs$model,
    from = 1:length(models),
    to = rep("singular", length(models)),
    warn_missing = F
  )

  #set factor levels to keep them consistent
  all_coefs$term = factor(all_coefs$term, levels = all_coefs$term %>% unique() %>% rev())

  #mark control variables if we have any
  if (!is.null(controls)) {
    all_coefs$control = F
    all_coefs$control[all_coefs$term %>% str_detect(str_c(controls, collapse = "|"))] = T
  }

  #remove control variables
  if (!keep_controls && !is.null(controls)) {
    all_coefs = all_coefs %>% filter(!control)
  }

  all_coefs
}




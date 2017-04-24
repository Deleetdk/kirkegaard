### META-ANALYSIS FUNCTIONS


#' Extract data from `rma` object
#'
#' Extract data from fitted meta-analysis of type `rma`.
#'
#' Extract effect size, sampling variance/standard error from `rma` object. Then adds a couple of more useful columns with z scores, p values (two-tailed).
#' @param .rma (rma object) The rma analysis from metafor.
#' @export
#' @return A data frame.
#' @examples
#' library(metafor); data(european_ancestry)
#' meta = rma(european_ancestry$r, sei = european_ancestry$SE_r)
#' meta_extract_data(meta)
meta_extract_data = function(.rma) {
  d = data_frame(
    es = .rma$yi,
    var = .rma$vi,
    se = .rma$vi %>% sqrt,
    z = es/se,
    p = pnorm(z, lower.tail = F)
  )

  #study name
  d$name = sprintf("Study %d", 1:nrow(d))

  d
}


#' Plot a contingency table with ggplot2
#'
#' Makes a pretty contingency table with ggplot2 using geom_tile.
#' @param .analysis (rma object) The rma analysis from metafor.
#' @param .names (chr vector) An optional vector of names to use.
#' @param .alphabetic_sort_names (lgl sclar) Alphabetically sort names? Default yes.
#' @export
#' @examples
#' library(metafor); data(european_ancestry)
#' meta = rma(european_ancestry$r, sei = european_ancestry$SE_r)
#' GG_forest(meta, .names = european_ancestry$Author_sample)
GG_forest = function(.analysis, .names = NULL, .alphabetic_sort_names = T) {
  if (!inherits(.analysis, "rma")) stop("This function only works for rma objects from the metafor package.")

  #extract effect sizes, SEs and names
  d = meta_extract_data(.analysis)[c("es", "se", "name")]
  d$meta = "study"

  #names if given
  if (!is.null(.names)) {
    d$name = .names
  }

  #make names unique if necessary
  if (any(duplicated(d$name))) {
    d$name %<>% str_uniquify
  }

  #sort?
  if (.alphabetic_sort_names) {
    d$name %<>% factor %>% forcats::fct_rev()
  }

  #extract main effect
  d_meta = tibble::data_frame(es = .analysis$b %>% as.vector,
                              se = .analysis$se,
                              name = "Main effect",
                              meta = "meta"
                              )

  #horizontal space case
  d_hline = tibble::data_frame(es = .analysis$b %>% as.vector,
                               se = .analysis$se,
                               name = "",
                               meta = "invis"
                               )

  #add main effect to d
  d = rbind(d, d_meta, d_hline)

  #make sure meta effect is in the bottom
  d$name %<>% factor() %>% forcats::fct_relevel(c("Main effect", ""))

  #plot
  ggplot2::ggplot(d, aes(es, name, color = meta)) +
    geom_point() +
    geom_errorbarh(aes(xmin = es - se * 1.96,
                       xmax = es + se * 1.96)) +
    geom_hline(yintercept = 2, linetype = "dashed") +
    theme_bw() +
    scale_y_discrete(name = NULL) +
    scale_colour_manual(values = c("white", "black", "black"), guide = F) +
    xlab("Effect size")
}


#' Funnel plot with ggplot2
#'
#' Makes a pretty funnel plot using [ggplot2].
#' @param .analysis (rma object) The rma analysis from [metafor].
#' @param .CI (chr vector) Confidence interval to use.
#' @param .study_CI (lgl vector) Whether to plot confidence intervals for individual studies.
#' @export
#' @examples
#' library(metafor); data(european_ancestry)
#' meta = rma(european_ancestry$r, sei = european_ancestry$SE_r)
#' GG_funnel(meta)
#' GG_funnel(meta, .study_CI = T)
GG_funnel = function(.analysis, .CI = .95, .study_CI = F) {
  if (!inherits(.analysis, "rma")) stop("This function only works for rma objects from the metafor package.")

  #convert CI to se z
  se_z = qnorm(1 - (1-.CI)/2)

  #extract main effect
  d_meta = tibble::data_frame(es = .analysis$b %>% as.vector,
                              se = .analysis$se
  )

  #extract effect sizes and SEs
  d = tibble::data_frame(es = .analysis$yi,
                         se = sqrt(.analysis$vi),
                         upper = d_meta$es + se_z * se,
                         lower = d_meta$es - se_z * se,
                         outlier = !is_between(es, lower, upper)
  )

  #calculate funnel
  d_funnel = tibble::data_frame(se = seq(0, max(d$se)*1.1, length.out = 1000),
                                upper = d_meta$es + se * se_z,
                                lower = d_meta$es - se * se_z)

  d_polygon = tibble::data_frame(x = c(min(d_funnel$lower), d_meta$es, max(d_funnel$upper)),
                                 y = c(max(d_funnel$se), 0, max(d_funnel$se)))

  #plot
  gg = ggplot2::ggplot() +
    geom_line(data = d_funnel, aes(upper, se)) +
    geom_line(data = d_funnel, aes(lower, se)) +
    geom_polygon(data = d_polygon, aes(x, y), fill = "grey") +
    geom_vline(linetype = "dashed", xintercept = d_meta$es) +
    geom_point(data = d, aes(es, se, color = outlier)) +
    scale_color_manual(guide = F, values = c("black", "red")) +
    scale_y_reverse() +
    theme_bw() +
    xlab("Effect size")

  #study CIs
  if (.study_CI) {
    gg = gg +
      geom_errorbarh(data = d, aes(xmin = es - se_z * se,
                                   xmax = es + se_z * se,
                                   x = es,
                                   y = se))
  }

  gg
}



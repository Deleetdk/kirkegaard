#' Translate between names of units and abbreviations
#'
#' Convert names of political units to standardized abbreviations or convert standardized abbreviations to names.
#' @param x (chr vectr) A chr vector of names to abbreviate.
#' @param superunit (chr vectr) Optional. A superunit whose subunits are the only ones being considered. Set to "world" if only sovereign countries are desired.
#' @param fuzzy (lgl scalr) Whether to use fuzzy matching if no exact match exists.
#' @param reverse (lgl scalr) Whether to translate from abbreviations to names.
#' @param lang (chr scalr) If translating back to names, which language to use. Default en=English.
#' @param superunit_recursive (lgl scalr) Whether to also include subunits of subunits. Default no.
#' @param messages (num scalr) Whether to give helpful messages. 0 = none, 1 = some, 2 = lots. Default 1.
#' @param stringdist_params (list) If using fuzzy matching, a list of parameters to pass to stringdist function.
#' @param standardize_name (lgl scalr) If true, will translate names to abbreviations, and then back to names. This converts the names to the standard version in the dataset.
#' @export
#' @return Returns a character vector.
#' @examples
#' pu_translate("DNK", reverse = T)
pu_translate = function(x, superunit, fuzzy = T, reverse=F, lang = "en", superunit_recursive = F, messages = 1, stringdist_params, standardize_name = F) {
  library(stringr); library(magrittr); library(XLConnect)
  #check input
  is_(x, class = "character", error_on_false = T)
  is_(fuzzy, class = "logical", error_on_false = T, size = 1)
  is_(reverse, class = "logical", error_on_false = T, size = 1)
  is_(superunit_recursive, class = "logical", error_on_false = T, size = 1)
  is_(messages, class = "numeric", error_on_false = T, size = 1)
  if (!is_between(messages, 0, 2)) stop(sprintf("messages was %f but must be 0>=x<=2. You probably made a mistake.", messages), call. = F)
  is_(standardize_name, class = "logical", error_on_false = T, size = 1)
  if (!missing("stringdist_params")) is_(stringdist_params, class = "list", error_on_false = T)
  if (superunit_recursive) stop("Not implemented yet. But the idea is that one can pass e.g. 'USA' and be able to match county names as well as states.")

  #impossible settings
  if (standardize_name & reverse) stop("Cannot both standardize names and reverse the translations!")

  #read polunits
  data_file_location = system.file("extdata", "political_units.xlsx", package = "kirkegaard")
  units = XLConnect::readWorksheetFromFile(data_file_location, sheet = "Abbreviations")

  #remove removed units
  units = dplyr::filter(units, is.na(Removed))

  #subset to subunits if desired
  if (!missing("superunit")) {
    #get name of superunit
    name_superunit = pu_translate(x = superunit, reverse = T, messages = 0)

    if (messages > 1) message(sprintf("Subsetting to subunits of %s [%s].", name_superunit, superunit))
    units = dplyr::filter(units, Superunit == superunit)

    #no members? give useful feedback
    if (nrow(units) == 0) stop(sprintf("There were no subunits of this superunit! %s", superunit))
  }

  #forward translation
  if (!reverse) {
    #loop over names
    x_exact = sapply(x, FUN = function(s) {
      #check if there is exactly one match
      if (sum(s == units$Names) == 1) {
        s_match = units$Abbreviation[which(s == units$Names)]
        if (messages > 1) message(sprintf("Exact match: %s -> %s", s, s_match))
        return(s_match)
      }

      #check if multiple matches
      if (sum(s == units$Names) > 1) {
        s_match = units$Abbreviation[which(s == units$Names)]
        str = sprintf("More than one exact match for %s. Perhaps you want to use a superunit to limit the options? Matches were: ", s)
        str = str + str_c(s_match, collapse = " | ")
        stop(str, call. = F)
      }

      #if zero
      if (messages>0) message(sprintf("No exact match: %s", s))
      return(NA)
    })
  }

  #backwards translation
  if (reverse) {
    #subset by lang?
    if (lang != "en") {
      #subset
      units_sub_by_lang = dplyr::filter(units, str_detect(units$Lang, pattern = lang))

      #check length
      if (nrow(units_sub_by_lang) == 0) stop("There were no translations at all in this language!", call. = F)
    }

    #loop over abbreviations
    x_exact = sapply(x, FUN = function(s) {
      # browser()

      #look in specific language?
      if (lang != "en") {
        #any matches?
        abbrev_matches = dplyr::filter(units_sub_by_lang, Abbreviation == s)
        if (nrow(abbrev_matches) == 0) {
          if (messages > 0) message(sprintf("There was no match in language %s for %s", lang, s))
        }

        #use the first match if any
        if (nrow(abbrev_matches) > 0) return(abbrev_matches$Name[1])
      }

      #any matches?
      abbrev_matches = dplyr::filter(units, Abbreviation == s)
      if (nrow(abbrev_matches) > 0) {
        return(abbrev_matches$Name[1])
      }

      #if zero
      message(sprintf("No match: %s", s))
      return(NA)
    })
  }

  #standardize name and not fuzzy?
  if (standardize_name & !fuzzy) return(pu_translate(x_exact %>% as.character(), reverse = T, lang = lang, messages = messages))

  #return?
  #if either not using fuzzy matching or doing back translations or no missing matches, return
  if (!fuzzy | reverse) return(x_exact)

  #fuzzy matching the remaining names
  x_noexact = is.na(x_exact)

  #loop over names and their exactnessstatus
  x_inexact = mapply(s = x, exact = !x_noexact, FUN = function(s, exact) {

    #skip if already have exact match
    if (exact) return(NA)
    #measure distances
    dst = stringdist::stringdist(s, units$Names)

    #make into df, and sort
    d_dst = data.frame(name = units$Names, abbrev = units$Abbreviation, dst = dst, stringsAsFactors = F) %>%
      df_sort("dst")

    #are there multiple best with different results?
    min_dst = min(dst)
    if (sum(min_dst == dst) > 1) {
      #filter to data with the min dst
      d_dst_min = dplyr::filter(d_dst, dst == min_dst)

      #are there disagreements?
      if (!all_the_same(d_dst_min$abbrev)) {
        str = sprintf("There were more than one equally good matches that did not agree for %s: ", s)
        str = str + str_c(d_dst_min$name, collapse = " | ")
        str = str + sprintf(". All with distance %.2f", min_dst)
        stop(str, call. = F)
      }
    }

    #pick the first
    x_best = d_dst[1, "name"]
    if (messages > 0) {
      message(sprintf("Best fuzzy match found: %s -> %s with distance %.2f", s, x_best, d_dst[1, "dst"]))
    }

    return(d_dst[1, "abbrev"])
  })

  #return merged vector
  x_combined = merge_vectors(x_exact, x_inexact, overwrite_NA = F)

  #standardize name?
  if (standardize_name) {
    return(pu_translate(x_combined, reverse = T, lang = lang, messages = messages))
  }

  #else return
  x_combined
}


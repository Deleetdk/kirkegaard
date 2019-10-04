#' Translate between names of units and abbreviations
#'
#' Convert names of political units to standardized abbreviations or convert standardized abbreviations to names.
#' @details
#' **Superunits**
#'
#' Superunits are the abbreviations of the units one level above in the hierarchy. Countries have "world" as their superunit. One can supply multiple superunits meaning that all available translations will be used. They are not used in any order of preference and coalitions will result in an error as usual. if you have complex data of mixed level units, it is probably easier to to use a *split-apply-combine* approach. I.e., split the names into those that belong to the world, and to each different country, then translate each subset and combine the results. This can be done e.g. using [plyr::ddply()].
#'
#' **Languages**
#'
#' The list of translations is being slowly extended as I find the need to do so. Currently, there is good support for Danish and English. There is reasonable support for German and Italian. There is some support for Norwegian and Swedish, but mostly from the name->ISO direction.
#'
#' **ISO codes and dependencies**
#'
#' In many cases, it is clear whether a given unit belongs to some other unit. Florida clearly belongs to the USA, and Vietnam clearly belongs to the world. But what about Hong Kong and the US Virgin Islands? These are not ordinary first-level administrative units as the usual [Chinese provinces](https://simple.wikipedia.org/wiki/Political_divisions_of_China) or [US states](https://en.wikipedia.org/wiki/Political_divisions_of_the_United_States), but neither are they independent in the same as Norway and Mexico are. Sometimes, these grey zone units have official ISO codes and sometimes not. I made the consistent call to place them under their respective sovereign countries, which in some cases can cause problems. Hong Kong (HKG), for instance, is listed under China but has it's own ISO code. This means that it will *not* be translated when one uses the 'world' superunit. Unfortunately, there doesn't seem to be any obvious solution to these problems, so one will simply have to be careful when using lists of names that contain mixed-level units. Fortunately, the function throws useful messages to warn the user of these problems.
#' @param x (chr vectr) A chr vector of names to abbreviate.
#' @param superunit (chr vectr) Optional. A superunit whose subunits are the only ones being considered. Set to "world" if only sovereign countries are desired.
#' @param fuzzy (lgl scalr) Whether to use fuzzy matching if no exact match exists.
#' @param reverse (lgl scalr) Whether to translate from abbreviations to names.
#' @param lang (chr scalr) If translating back to names, which language to use.
#' @param superunit_recursive (lgl scalr) Whether to also include subunits of subunits.
#' @param messages (num scalr) Whether to give helpful messages. 0 = none, 1 = some, 2 = lots.
#' @param stringdist_params (list) If using fuzzy matching, a list of parameters to pass to stringdist function.
#' @param standardize_name (lgl scalr) If true, will translate names to abbreviations, and then back to names. This converts the names to the standard version in the dataset.
#' @param add_parens (chr or NULL) Adds parenthesized versions of units. Useful for diambiguation.
#' @export
#' @return A character vector.
#' @examples
#' pu_translate("Denmark")
#' pu_translate("DNK", reverse = T)
#' pu_translate("DNK", reverse = T, lang = "de")
#' #throws an error due to multiple Georgias
#' pu_translate("Georgia")
#' #solve by subsetting to specific superunits
#' pu_translate("Georgia", superunit = "world")
#' #complex problems can happen when one has mixed level units, e.g Georgia (country) and Hong Kong (quasi-country under Chinese rule)
#' pu_translate(c("Hong Kong", "Georgia"), superunit = "world") #clearly wrong!
#' pu_translate(c("Hong Kong", "Georgia"), superunit = c("world", "CHN")) #right!
#' #duplicated names in Latin America
#' pu_translate("Cordoba") #bad, multiple matches
#' pu_translate("Cordoba (ARG)") #works
pu_translate = function(x,
                        superunit = NULL,
                        fuzzy = T,
                        reverse = F,
                        lang = "en",
                        superunit_recursive = F,
                        messages = 1,
                        stringdist_params = NULL,
                        standardize_name = F,
                        add_parens = c("ISO3", "ISO2", "name")) {

  #uniq encode
  x_encoded = uniq_encoding(x)

  #translate unique values
  x_translated = pu_translate_inner(x = x_encoded$levels,
                              superunit = superunit,
                              fuzzy = fuzzy,
                              reverse = reverse,
                              lang = lang,
                              superunit_recursive = superunit_recursive,
                              messages = messages,
                              stringdist_params = stringdist_params,
                              standardize_name = standardize_name,
                              add_parens = add_parens)

  #reverse uniq
  x_encoded$levels = x_translated
  rev_uniq_encoding(x_encoded)
}

#inner function
pu_translate_inner = function(x,
                              superunit = NULL,
                              fuzzy = T,
                              reverse = F,
                              lang = "en",
                              superunit_recursive = F,
                              messages = 1,
                              stringdist_params = NULL,
                              standardize_name = F,
                              add_parens = c("ISO3", "ISO2", "name")) {
  #check input
  x = as.character(x) #forces to right type from whatever input was
  is_(fuzzy, class = "logical", error_on_false = T, size = 1)
  is_(reverse, class = "logical", error_on_false = T, size = 1)
  is_(superunit_recursive, class = "logical", error_on_false = T, size = 1)
  is_(messages, class = c("numeric", "logical"), error_on_false = T, size = 1)
  if (!is_between(messages, 0, 2)) stop(sprintf("messages was %f but must be 0>=x<=2. You probably made a mistake.", messages), call. = F)
  is_(standardize_name, class = "logical", error_on_false = T, size = 1)
  if (!is.null(stringdist_params)) is_(stringdist_params, class = "list", error_on_false = T)
  if (superunit_recursive) stop("Not implemented yet. But the idea is that one can pass e.g. 'USA' and be able to match county names as well as states.")

  #NAs
  na_pos = which(is.na(x))
  x = na.omit(x)
  #if empty, we just need to return the NAs
  if (length(x) == 0) return(restore_NAs(x, na_pos))

  #impossible settings
  if (standardize_name & reverse) stop("Cannot both standardize names and reverse the translations!")

  #read polunits
  data_file_location = system.file("extdata", "political_units.xlsx", package = "kirkegaard")
  units = readxl::read_xlsx(data_file_location, sheet = "Abbreviations", guess_max = 10000)

  #fill in ISOs
  units$Abbreviation = units$Abbreviation %>% miss_locf()

  #remove removed units
  units_all = units #keep orig
  units = dplyr::filter(units, is.na(Removed))

  #subset to subunits if desired
  if (!is.null(superunit)) {

    #fill in 'world'
    units$Superunit %<>% plyr::mapvalues(from = NA, to = "world")

    #get names of superunits
    name_superunit = pu_translate(x = superunit, reverse = T, messages = 0)

    if (messages > 1) message(sprintf("Subsetting to subunits of: %s [%s].", stringr::str_c(name_superunit, collapse = " | "), stringr::str_c(superunit, collapse = " | ")))
    units = dplyr::filter(units, Superunit %in% superunit)

    #no members? give useful feedback
    if (nrow(units) == 0) stop(sprintf("There were no subunits of this superunit! %s", superunit))
  }

  #add duplicates with country ISO in parens
  #this is so we dont need to manually specify these
  if (!is.null(add_parens) & !reverse) {

    sub_units_clean = units %>% filter(!is.na(Superunit))

    #add the country ISO3 in parens
    if ("ISO3" %in% add_parens) {
      sub_units = sub_units_clean
      sub_units$Name = sprintf("%s (%s)", sub_units$Name, sub_units$Superunit)
      units = rbind(units, sub_units)
    }

    #ISO2
    #TODO: add ISO2
    # if ("ISO2" %in% add_parens) {
    #   sub_units$Name = sprintf("%s (%s)", sub_units$Name, sub_units$Superunit)
    #   units = rbind(units, sub_units)
    # }

    #full names
    if ("name" %in% add_parens) {
      sub_units = sub_units_clean
      superunit_names = pu_translate(sub_units$Superunit, reverse = T)
      sub_units$Name = sprintf("%s (%s)", sub_units$Name, superunit_names)
      units = rbind(units, sub_units)
    }

    #get rid of any duplicates that might have arisen
    #these happen if country names are the same as ISO (e.g. USA)
    units %<>% filter(!duplicated(Name))

  }

  #forward translation
  if (!reverse) {
    #loop over names
    x_exact = sapply(x, FUN = function(s) {
      #check if there is exactly one match
      if (sum(s == units$Name) == 1) {
        s_match = units$Abbreviation[which(s == units$Name)]
        if (messages > 1) message(sprintf("Exact match: %s -> %s", s, s_match))
        return(s_match)
      }

      #check if multiple matches
      if (sum(s == units$Name) > 1) {
        s_match = units$Abbreviation[which(s == units$Name)]
        str = sprintf("More than one exact match for %s. Perhaps you want to use a superunit to limit the options? Matches were: ", s)
        str = str + stringr::str_c(s_match, collapse = " | ")
        stop(str, call. = F)
      }

      #if zero
      if (messages>0) message(sprintf("No exact match: %s", s))
      return(NA_character_)
    })
  }

  #backwards translation
  if (reverse) {
    #subset by lang?
    if (lang != "en") {
      #subset
      units_sub_by_lang = dplyr::filter(units_all, Lang == lang)
      #include legacy abbreviations for reverse translations

      #check length
      if (nrow(units_sub_by_lang) == 0) stop(sprintf("There are no translations for this language: %s", lang), call. = F)
    }

    #loop over abbreviations
    x_exact = purrr::map_chr(x, function(s) {
      #look in specific language?
      if (lang != "en") {
        #any matches?
        abbrev_matches = dplyr::filter(units_sub_by_lang, Abbreviation == s)
        if (nrow(abbrev_matches) == 0) {
          if (messages > 0) message(sprintf("There was no match in language %s for country %s", lang, s))
        }

        #use the first match if any
        if (nrow(abbrev_matches) > 0) return(abbrev_matches$Name[1])
      }

      #any matches?
      abbrev_matches = dplyr::filter(units_all, Abbreviation == s)
      if (nrow(abbrev_matches) > 0) {
        return(abbrev_matches$Name[1])
      }

      #if zero
      message(sprintf("No match: %s", s))
      return(NA)
    })
  }

  #standardize name and not fuzzy?
  if (standardize_name & !fuzzy) return(pu_translate(x_exact %>% as.character, reverse = T, lang = lang, messages = messages))

  #return?
  #if either not using fuzzy matching or doing back translations or no missing matches, return
  if (!fuzzy | reverse) return(restore_NAs(x_exact, na_pos))

  #fuzzy matching the remaining names
  x_noexact = is.na(x_exact)

  #loop over names and their exactnessstatus
  x_inexact = mapply(s = x, exact = !x_noexact, FUN = function(s, exact) {

    #skip if already have exact match
    if (exact) return(NA)
    #measure distances
    dst = stringdist::stringdist(s, units$Name)

    #make into df, and sort
    d_dst = data.frame(name = units$Name, abbrev = units$Abbreviation, dst = dst, stringsAsFactors = F) %>%
      df_sort("dst")

    #are there multiple best with different results?
    min_dst = min(dst)
    if (sum(min_dst == dst) > 1) {
      #filter to data with the min dst
      d_dst_min = dplyr::filter(d_dst, dst == min_dst)

      #are there disagreements?
      if (!all_the_same(d_dst_min$abbrev)) {
        str = sprintf("There were multiple equally good matches for %s: ", s)
        str = str + str_c(d_dst_min$name, collapse = " | ")
        str = str + sprintf(". All with distance %.2f", min_dst)
        warning(str, call. = F)
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
  restore_NAs(x_combined, na_pos)
}

# pu_translate(NA)
# pu_translate(rep(NA, 5))
# pu_translate(c("Denmark", NA, "Sweden"))
# pu_translate(c("Dennmark", NA, "Sueden"))
# pu_translate(c("Dennmark", NA, "Sueden")) %>% pu_translate(reverse=T)
# pu_translate(c("Dennmark", NA, "Sueden", "Sweden"))

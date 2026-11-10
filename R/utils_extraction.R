# Utility functions for feature extraction

#' Normalize feature counts to per-1,000-word rates
#'
#' Metrics (f_43, f_44) and the internal denominator are not rates and are
#' left unchanged.
#'
#' @param counts A data frame with feature counts and a `tot_counts` column.
#' @return A data frame with normalized counts (`tot_counts` removed).
#' @keywords internal
normalize_counts <- function(counts) {
  no_norm <- c("f_43_type_token", "f_44_mean_word_length", "n_tokens", "tot_counts")
  counts %>%
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric) & !dplyr::any_of(no_norm),
      ~ 1000 * . / tot_counts
    )) %>%
    dplyr::select(-"tot_counts")
}

#' Replace NAs with zeros in numeric columns of a data frame
#'
#' @param x A data frame
#' @return Data frame with NAs in numeric columns replaced by 0
#' @keywords internal
replace_nas <- function(x) {
  dplyr::mutate(x, dplyr::across(dplyr::where(is.numeric), ~ dplyr::coalesce(., 0L)))
}

#' Extract a morphological feature value from a UD feats string
#'
#' @param feats Character vector of UD morphological features
#' @param key The feature name to extract (e.g., "Tense", "VerbForm")
#' @return Character vector of extracted values
#' @keywords internal
extract_morph_value <- function(feats, key) {
  value <- stringr::str_match(feats, paste0("(?:^|\\|)", key, "=([^|]+)"))[, 2]
  as.character(value)
}

#' Get a named word list from the word_lists data
#'
#' @param word_lists_lookup The word_lists object
#' @param name Name of the list to retrieve
#' @return Character vector of terms
#' @keywords internal
get_word_list <- function(word_lists_lookup, name) {
  if (!name %in% names(word_lists_lookup)) {
    warning("Word list '", name, "' not found", call. = FALSE)
    return(character(0))
  }
  word_lists_lookup[[name]]
}

#' Normalize terms for matching
#'
#' Lower-cases and maps typographic apostrophes (U+2019, U+2018, U+02BC) to
#' the ASCII apostrophe.
#'
#' @param values Character vector of terms
#' @return Normalized character vector
#' @keywords internal
normalize_terms <- function(values) {
  stringi::stri_trans_tolower(values) %>%
    stringr::str_replace_all("[\u2019\u2018\u02bc]", "'")
}

#' Extract lemmas from a dictionary entry
#'
#' Only single-word entries are returned, so that multiword phrases do not
#' contribute their first word (a preposition or article) as a lemma. With
#' `head_word = TRUE`, the first word of every entry is returned instead.
#'
#' @param dict_lookup The dict object
#' @param feature Feature name
#' @param head_word Return the first word of every entry.
#' @return Character vector of lemmas
#' @keywords internal
dictionary_to_lemmas <- function(dict_lookup, feature, head_word = FALSE) {
  if (!feature %in% names(dict_lookup)) return(character(0))
  patterns <- normalize_terms(unlist(dict_lookup[[feature]], use.names = FALSE))
  if (head_word) return(unique(stringr::str_extract(patterns, "^[^_]+")))
  unique(patterns[!stringr::str_detect(patterns, "_")])
}

# Utility functions for feature extraction

#' Normalize feature counts to per-1000-word rates
#'
#' @param counts A data frame with feature counts and a tot_counts column
#' @return A data frame with normalized counts (tot_counts column removed)
#' @keywords internal
normalize_counts <- function(counts) {
  counts %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ 1000 * . / tot_counts)) %>%
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

#' Extract a specific morphological feature value from UD feats string
#'
#' @param feats Character vector of UD morphological features
#' @param key The feature name to extract (e.g., "Tense", "VerbForm")
#' @return Character vector of extracted values
#' @keywords internal
extract_morph_value <- function(feats, key) {
  purrr::map_chr(feats, function(f) {
    if (is.na(f) || f == "") return(NA_character_)
    parts <- stringr::str_split(f, "\\|")[[1]]
    match <- parts[stringr::str_detect(parts, paste0("^", key, "="))]
    if (length(match) == 0) return(NA_character_)
    stringr::str_remove(match[1], paste0("^", key, "="))
  })
}

#' Get a named word list from the word_lists data
#'
#' @param word_lists_lookup The word_lists object
#' @param name Name of the list to retrieve
#' @return Character vector of terms
#' @keywords internal
get_word_list <- function(word_lists_lookup, name) {
  if (!name %in% names(word_lists_lookup)) {
    warning(paste0("Word list '", name, "' not found"))
    return(character(0))
  }
  word_lists_lookup[[name]]
}

#' Normalize terms by converting to lowercase and replacing Unicode apostrophes
#'
#' @param values Character vector of terms
#' @return Normalized character vector
#' @keywords internal
normalize_terms <- function(values) {
  stringr::str_to_lower(values) %>%
    stringr::str_replace_all("\u2019", "'")
}

#' Extract lemmas from a dictionary entry
#'
#' @param dict_lookup The dict object
#' @param feature Feature name
#' @return Character vector of lemmas
#' @keywords internal
dictionary_to_lemmas <- function(dict_lookup, feature) {
  if (!feature %in% names(dict_lookup)) {
    return(character(0))
  }
  
  patterns <- dict_lookup[[feature]]
  
  lemmas <- patterns %>%
    stringr::str_remove_all("_") %>%
    stringr::str_to_lower() %>%
    unique()
  
  lemmas
}

#' Prepare parsed tokens for downstream feature extraction
#'
#' @param tokens Parsed token data frame
#' @return Normalized token tibble with harmonized morphology columns
#' @keywords internal
prepare_parsed_tokens <- function(tokens) {
  tokens <- tokens %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(token = stringr::str_to_lower(.data$token))

  if (!"sentence_id" %in% colnames(tokens)) {
    tokens <- dplyr::mutate(tokens, sentence_id = 1L)
  }

  tokens <- tokens %>%
    dplyr::mutate(pos = dplyr::if_else(.data$token == "\n", "PUNCT", .data$pos)) %>%
    dplyr::filter(.data$pos != "SPACE")

  if (nrow(tokens) == 0) {
    stop("No valid tokens found after filtering. Document may contain only whitespace.", call. = FALSE)
  }

  if ("morph" %in% colnames(tokens)) {
    tokens <- tokens %>%
      dplyr::mutate(morph = purrr::map_chr(.data$morph, function(x) {
        if (inherits(x, "python.builtin.object")) {
          if (requireNamespace("reticulate", quietly = TRUE)) {
            value <- reticulate::py_to_r(x)
            if (is.null(value) || length(value) == 0) {
              return("")
            }
            value <- as.character(value)
            if (length(value) == 1) {
              return(value)
            }
            return(paste(value, collapse = "|"))
          }
          return(as.character(x))
        }
        if (is.null(x) || length(x) == 0) {
          return("")
        }
        if (length(x) == 1) {
          return(as.character(x))
        }
        paste(as.character(x), collapse = "|")
      })) %>%
      dplyr::mutate(morph = dplyr::na_if(.data$morph, ""))

    if ("feats" %in% colnames(tokens)) {
      tokens <- tokens %>%
        dplyr::mutate(feats = dplyr::coalesce(.data$feats, .data$morph))
    } else {
      tokens <- tokens %>%
        dplyr::mutate(feats = .data$morph)
    }
  }

  if (!"feats" %in% colnames(tokens)) {
    tokens <- dplyr::mutate(tokens, feats = NA_character_)
  }

  tokens %>%
    dplyr::mutate(
      token_id_int = suppressWarnings(as.integer(.data$token_id)),
      head_token_id_int = suppressWarnings(as.integer(.data$head_token_id)),
      morph_tense = extract_morph_value(.data$feats, "Tense"),
      morph_verbform = extract_morph_value(.data$feats, "VerbForm"),
      morph_mood = extract_morph_value(.data$feats, "Mood"),
      morph_prontype = extract_morph_value(.data$feats, "PronType"),
      morph_voice = extract_morph_value(.data$feats, "Voice"),
      morph_number = extract_morph_value(.data$feats, "Number"),
      morph_person = extract_morph_value(.data$feats, "Person")
    ) %>%
    dplyr::arrange(.data$doc_id, .data$sentence_id, .data$token_id_int)
}

#' Coerce UDPipe annotations to the shared spacy-like token schema
#'
#' @param tokens A `udpipe_connlu` object
#' @return A data frame with the shared token columns used internally
#' @keywords internal
coerce_udpipe_to_spacy_tokens <- function(tokens) {
  udpipe_as_df <- if (requireNamespace("udpipe", quietly = TRUE)) {
    utils::getFromNamespace("as.data.frame.udpipe_connlu", "udpipe")
  } else {
    NULL
  }

  if (is.null(udpipe_as_df)) {
    stop("The 'udpipe' package is required to coerce udpipe_connlu objects.", call. = FALSE)
  }

  udpipe_tks <- udpipe_as_df(tokens, stringsAsFactors = FALSE)

  udpipe_tks <- udpipe_tks %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id", "token", "lemma", "upos",
      "xpos", "feats", "head_token_id", "dep_rel"
    ) %>%
    dplyr::rename(pos = "upos", tag = "xpos") %>%
    dplyr::mutate(tag = dplyr::if_else(is.na(.data$tag) | .data$tag == "", .data$pos, .data$tag))

  structure(udpipe_tks, class = c("spacyr_parsed", "data.frame"))
}

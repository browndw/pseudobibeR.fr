# Lexical diversity, word length and token-count metadata

PUNCT_UPOS <- c("PUNCT", "SYM", "SPACE")

#' Normalization denominator per document
#'
#' UD word rows excluding punctuation, symbols, spaces and digit-final numeric
#' tokens (`42`, `3,5`), the same numeric rule as the English package and the
#' lexical-diversity stream. Spelled-out numerals and function words count.
#'
#' @param tokens Prepared tokens.
#' @param doc_ids Document IDs in output order.
#' @return Integer vector aligned with `doc_ids`.
#' @keywords internal
#' @noRd
document_word_counts <- function(tokens, doc_ids) {
  words <- tokens[!tokens$pos %in% PUNCT_UPOS & !stringi::stri_detect_regex(tokens$token, "\\d$"), ]
  tabulate(match(words$doc_id, doc_ids$doc_id), nbins = nrow(doc_ids))
}

#' Lexical-diversity token stream
#'
#' Lower-case surface forms with their fine-grained tag appended (UPOS when
#' the tag is missing), excluding punctuation, symbols and spaces, and
#' removing digit-final numeric forms. Dictionary phrases are not compounded.
#'
#' @param tokens Prepared tokens.
#' @param doc_ids Document IDs in output order.
#' @return A quanteda tokens object with one document per `doc_ids` row.
#' @keywords internal
lexical_diversity_tokens <- function(tokens, doc_ids) {
  words <- tokens[!tokens$pos %in% PUNCT_UPOS, ]
  tags <- dplyr::coalesce(dplyr::na_if(words$tag, "_"), words$pos)
  tagged <- paste(words$token, stringi::stri_trans_tolower(tags), sep = "_")
  documents <- factor(match(words$doc_id, doc_ids$doc_id), levels = seq_len(nrow(doc_ids)))
  quanteda::as.tokens(split(tagged, documents)) %>%
    quanteda::tokens_remove("\\d_", valuetype = "regex")
}

#' Normalization denominator and mean word length (French)
#'
#' `n_tokens` is the internal normalization denominator (see
#' `document_word_counts()`); it is not part of the output. f_44 is the mean
#' length in characters of purely alphabetic tokens (any Unicode letter),
#' unrounded; `NA` for a document without any (D-51).
#'
#' @param tokens Prepared tokens.
#' @param doc_ids Document IDs.
#' @return Data frame with `n_tokens` and f_44.
#' @keywords internal
block_lexical_complexity_fr <- function(tokens, doc_ids) {
  alphabetic <- tokens[stringi::stri_detect_regex(tokens$token, "^\\p{L}+$"), ]
  lengths <- alphabetic %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(f_44_mean_word_length = mean(nchar(.data$token, type = "chars")),
                     .groups = "drop")
  doc_ids %>%
    dplyr::left_join(lengths, by = "doc_id") %>%
    dplyr::mutate(n_tokens = document_word_counts(tokens, doc_ids))
}

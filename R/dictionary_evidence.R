# Sentence-local dictionary matching on supplied word rows

#' Convert dictionary entries to word sequences
#'
#' Underscores separate words; an apostrophe ends a word (`à_l_'_est` becomes
#' `à l' est`). Entries with an internal apostrophe (`quelqu'un`) also match
#' the split elision tokenization (`quelqu' un`) used by some parsers.
#'
#' @param patterns Dictionary entries.
#' @return Character vector of space-separated phrases.
#' @keywords internal
dictionary_phrases <- function(patterns) {
  patterns <- normalize_terms(unlist(patterns, use.names = FALSE))
  joined <- gsub("_'(_|$)", "'\\1", patterns)
  phrases <- gsub("_", " ", joined, fixed = TRUE)
  split <- gsub("([^ ])'([^ ])", "\\1' \\2", phrases)
  unique(c(phrases, split))
}

#' Find maximal sentence-local phrase matches
#'
#' @param tokens Prepared tokens.
#' @param patterns Dictionary entries.
#' @return Data frame of `start_row`/`end_row` indexes into `tokens`; matches
#'   contained in a longer match are dropped.
#' @keywords internal
surface_matches <- function(tokens, patterns) {
  empty <- data.frame(start_row = integer(), end_row = integer())
  if (!length(patterns) || !nrow(tokens)) return(empty)
  groups <- dplyr::group_indices(dplyr::group_by(tokens, .data$doc_id, .data$sentence_id))
  rows <- split(seq_len(nrow(tokens)), groups)
  words <- lapply(rows, function(i) tokens$match_token[i])
  matches <- quanteda::kwic(
    quanteda::as.tokens(words),
    pattern = quanteda::phrase(dictionary_phrases(patterns)),
    valuetype = "fixed", case_insensitive = TRUE, window = 0
  )
  if (!nrow(matches)) return(empty)
  found <- unique(data.frame(
    start_row = vapply(seq_len(nrow(matches)), function(i) {
      rows[[as.character(matches$docname[i])]][matches$from[i]]
    }, integer(1)),
    end_row = vapply(seq_len(nrow(matches)), function(i) {
      rows[[as.character(matches$docname[i])]][matches$to[i]]
    }, integer(1))
  ))
  keep <- vapply(seq_len(nrow(found)), function(i) {
    !any(found$start_row <= found$start_row[i] & found$end_row >= found$end_row[i] &
           (found$start_row < found$start_row[i] | found$end_row > found$end_row[i]))
  }, logical(1))
  found[keep, , drop = FALSE]
}

#' Dictionary evidence for surface-inventory features
#'
#' One evidence row per maximal phrase match, anchored on its first word,
#' with the full matched phrase in `token`.
#'
#' With `cross_feature_precedence = TRUE`, a match strictly contained in a
#' longer match of another listed feature is dropped, so that the words of a
#' phrase (`en quelque sorte`) are not also counted individually (`quelque`).
#'
#' @param tokens Prepared tokens.
#' @param features Dictionary feature names.
#' @param cross_feature_precedence Apply longest-match precedence across features.
#' @return List with `evidence` and phrase `spans`.
#' @keywords internal
dictionary_evidence <- function(tokens, features, cross_feature_precedence = FALSE) {
  dict <- pseudobibeR.fr::dict
  evidence <- list()
  spans <- list()
  for (feature in features) {
    hits <- surface_matches(tokens, dict[[feature]])
    if (!nrow(hits)) next
    rows <- tokens[hits$start_row, ]
    rows$token <- vapply(seq_len(nrow(hits)), function(i) {
      paste(tokens$token[seq.int(hits$start_row[i], hits$end_row[i])], collapse = " ")
    }, character(1))
    evidence[[feature]] <- evidence_from_rows(rows, feature)
    spans[[feature]] <- data.frame(
      feature = feature, doc_id = rows$doc_id, sentence_id = rows$sentence_id,
      start = tokens$token_id_int[hits$start_row], end = tokens$token_id_int[hits$end_row]
    )
  }
  if (cross_feature_precedence && length(spans) > 1L) {
    all_spans <- dplyr::bind_rows(spans)
    contained <- vapply(seq_len(nrow(all_spans)), function(i) {
      s <- all_spans[i, ]
      any(all_spans$doc_id == s$doc_id & all_spans$sentence_id == s$sentence_id &
            all_spans$feature != s$feature & all_spans$start <= s$start &
            all_spans$end >= s$end & (all_spans$end - all_spans$start) > (s$end - s$start))
    }, logical(1))
    dropped <- all_spans[contained, ]
    for (feature in names(evidence)) {
      gone <- dropped[dropped$feature == feature, ]
      if (!nrow(gone)) next
      key <- paste(evidence[[feature]]$doc_id, evidence[[feature]]$sentence_id,
                   evidence[[feature]]$token_id)
      evidence[[feature]] <- evidence[[feature]][
        !key %in% paste(gone$doc_id, gone$sentence_id, gone$start), ]
      spans[[feature]] <- spans[[feature]][
        !paste(spans[[feature]]$doc_id, spans[[feature]]$sentence_id, spans[[feature]]$start) %in%
          paste(gone$doc_id, gone$sentence_id, gone$start), ]
    }
  }
  list(
    evidence = do.call(bind_evidence, unname(evidence)),
    spans = if (length(spans)) dplyr::bind_rows(spans) else
      data.frame(feature = character(), doc_id = character(), sentence_id = integer(),
                 start = integer(), end = integer())
  )
}

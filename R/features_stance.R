# Occurrence-level disambiguation of words listed in two lexical classes

#' Move selected occurrences from one feature to another
#'
#' Evidence rows of `from` anchored on `rows` are removed and replaced by
#' rows of `to`, so that each word belongs to one of the two classes.
#'
#' @param evidence Combined evidence.
#' @param rows Token rows to reassign.
#' @param from,to Feature names.
#' @return Updated evidence.
#' @keywords internal
reassign_evidence <- function(evidence, rows, from, to) {
  if (!nrow(rows)) return(evidence)
  keys <- c("doc_id", "sentence_id", "token_id")
  targets <- evidence_from_rows(rows, to)
  moved <- evidence[evidence$feature == from, ] %>%
    dplyr::semi_join(targets, by = keys)
  if (!nrow(moved)) return(evidence)
  remaining <- evidence[!(evidence$feature == from &
                            paste(evidence$doc_id, evidence$sentence_id, evidence$token_id) %in%
                            paste(moved$doc_id, moved$sentence_id, moved$token_id)), ]
  added <- dplyr::mutate(moved, feature = to) %>%
    dplyr::anti_join(remaining[remaining$feature == to, ], by = keys)
  bind_evidence(remaining, added)
}

#' Disambiguate donc, ensuite and vraiment
#'
#' `donc` that is sentence-initial, sentence-final or in a question is a
#' discourse particle (f_50), not a conjunct (f_45). `ensuite` after a comma
#' or semicolon, not sentence-initial, is a conjunct (f_45), not a time
#' adverbial (f_05). `vraiment` next to an adjective or adverb is an
#' amplifier (f_48), not an emphatic (f_49).
#'
#' @param evidence Combined evidence.
#' @param tokens Prepared tokens with context columns.
#' @return Updated evidence.
#' @keywords internal
disambiguate_stance_fr <- function(evidence, tokens) {
  question <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::mutate(has_question = any(.data$token == "?" & .data$pos == "PUNCT")) %>%
    dplyr::ungroup()
  donc <- question[question$lemma == "donc" &
                     (question$position_in_sentence <= 2 |
                        question$position_in_sentence > question$sentence_length - 2 |
                        question$has_question), ]
  ensuite <- tokens[tokens$lemma == "ensuite" & tokens$prev_token %in% c(",", ";") &
                      tokens$position_in_sentence > 2, ]
  vraiment <- tokens[tokens$lemma == "vraiment" &
                       (tokens$next_pos %in% c("ADJ", "ADV") |
                          tokens$prev_pos %in% c("ADJ", "ADV")), ]
  evidence <- reassign_evidence(evidence, donc, "f_45_conjuncts", "f_50_discourse_particles")
  evidence <- reassign_evidence(evidence, ensuite, "f_05_time_adverbials", "f_45_conjuncts")
  reassign_evidence(evidence, vraiment, "f_49_emphatics", "f_48_amplifiers")
}

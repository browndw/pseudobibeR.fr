# Passive voice and stative features

#' Passive voice and stative features (French)
#'
#' f_17/f_18: one occurrence per passive predicate (head of a passive
#' auxiliary), recorded as the span from the auxiliary to the predicate
#' (`été rédigé`). A predicate with an agent
#' (`obl:agent`/`agent`, or an oblique introduced by `par`) is f_18;
#' otherwise f_17. f_19: être not used as an auxiliary. f_20: each
#' `il y a` (avoir preceded by `y` and the pronoun `il`).
#'
#' @param tokens Prepared tokens with sentence-local context columns.
#' @param doc_ids Document IDs.
#' @param head_lookup Head token attributes.
#' @param passive_rel_values Dependency labels of passive auxiliaries.
#' @return Block result with f_17-f_20 evidence.
#' @keywords internal
block_passive_voice_fr <- function(tokens, doc_ids, head_lookup, passive_rel_values) {
  passive_aux <- tokens %>%
    dplyr::filter(.data$dep_rel %in% passive_rel_values) %>%
    dplyr::left_join(head_lookup,
                     by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")) %>%
    dplyr::filter(.data$head_pos == "VERB") %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$head_token_id_int, .keep_all = TRUE)

  par_obliques <- tokens %>%
    dplyr::filter(.data$lemma == "par", .data$dep_rel == "case") %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int)
  # par attached as case directly to the passive predicate is also accepted.
  agents <- dplyr::bind_rows(
    tokens %>%
      dplyr::filter(
        .data$dep_rel %in% c("obl:agent", "agent") |
          (stringr::str_detect(.data$dep_rel, "^(obl|nmod)") &
             paste(.data$doc_id, .data$sentence_id, .data$token_id_int) %in%
             paste(par_obliques$doc_id, par_obliques$sentence_id, par_obliques$token_id_int))
      ) %>%
      dplyr::distinct(.data$doc_id, .data$sentence_id, head_token_id_int = .data$head_token_id_int),
    dplyr::rename(par_obliques, head_token_id_int = "token_id_int")
  ) %>%
    dplyr::distinct()

  passive_spans <- function(rows) {
    dplyr::transmute(rows, .data$doc_id, .data$sentence_id,
                     start = pmin(.data$token_id_int, .data$head_token_id_int),
                     end = pmax(.data$token_id_int, .data$head_token_id_int))
  }
  keys <- c("doc_id", "sentence_id", "head_token_id_int")
  f18 <- span_evidence(tokens, passive_spans(dplyr::semi_join(passive_aux, agents, by = keys)),
                       "f_18_by_passives")
  f17 <- span_evidence(tokens, passive_spans(dplyr::anti_join(passive_aux, agents, by = keys)),
                       "f_17_agentless_passives")

  f19 <- tokens %>%
    dplyr::filter(.data$lemma == "\u00eatre", !stringr::str_detect(.data$dep_rel, "aux")) %>%
    count_feature_traced("f_19_be_main_verb")

  f20 <- tokens %>%
    dplyr::filter(
      .data$lemma == "avoir",
      .data$pos %in% c("VERB", "AUX"),
      dplyr::coalesce(.data$prev_lemma, "") == "y",
      dplyr::coalesce(.data$prev2_lemma, "") == "il",
      dplyr::coalesce(.data$prev2_pos, "") == "PRON"
    ) %>%
    count_feature_traced("f_20_existential_there")

  make_block_result(doc_ids, bind_evidence(f17, f18, f19$evidence, f20$evidence))
}

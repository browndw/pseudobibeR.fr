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

  # Existential il y a (D-55): y (+ en) + avoir, with il, a verb (il va y
  # avoir) or nothing (colloquial y a, y a-t-il) before y, skipping ne. Surface
  # forms are used because UDPipe lemmatizes elle/je as il. avoir as the
  # auxiliary of another verb (il y a mangé, "he ate there") is excluded;
  # il y a eu remains.
  y_pos <- ifelse(tokens$prev_token %in% "en" & normalize_terms(dplyr::coalesce(tokens$prev2_token, "")) == "y", 2L,
                  ifelse(normalize_terms(dplyr::coalesce(tokens$prev_token, "")) == "y", 1L, NA_integer_))
  lag_token <- function(k) {
    out <- rep(NA_character_, nrow(tokens))
    cols <- list(tokens$prev_token, tokens$prev2_token, tokens$prev3_token, tokens$prev4_token)
    for (i in seq_along(k)) if (!is.na(k[i]) && k[i] <= 4) out[i] <- cols[[k[i]]][i]
    normalize_terms(out)
  }
  lag_pos <- function(k) {
    out <- rep(NA_character_, nrow(tokens))
    cols <- list(tokens$prev_pos, tokens$prev2_pos, tokens$prev3_pos, tokens$prev4_pos)
    for (i in seq_along(k)) if (!is.na(k[i]) && k[i] <= 4) out[i] <- cols[[k[i]]][i]
    out
  }
  before <- lag_token(y_pos + 1L)
  skip_ne <- before %in% c("ne", "n'")
  before <- ifelse(skip_ne, lag_token(y_pos + 2L), before)
  before_pos <- ifelse(skip_ne, lag_pos(y_pos + 2L), lag_pos(y_pos + 1L))
  subject_ok <- is.na(before) | before %in% c("il", "qu'il") |
    before_pos %in% c("VERB", "AUX", "PUNCT", "SCONJ", "CCONJ", "ADV")
  participle_heads <- tokens %>%
    dplyr::filter(.data$morph_verbform %in% "Part", .data$match_lemma != "avoir") %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int)
  aux_of_participle <- stringr::str_detect(tokens$dep_rel, "^aux") &
    paste(tokens$doc_id, tokens$sentence_id, tokens$head_token_id_int) %in%
    paste(participle_heads$doc_id, participle_heads$sentence_id, participle_heads$token_id_int)
  f20 <- tokens[tokens$match_lemma == "avoir" & tokens$pos %in% c("VERB", "AUX") &
                  !is.na(y_pos) & subject_ok & !aux_of_participle, ] %>%
    count_feature_traced("f_20_existential_there")

  make_block_result(doc_ids, bind_evidence(f17, f18, f19$evidence, f20$evidence))
}

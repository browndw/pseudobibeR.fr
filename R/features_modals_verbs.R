# Adjective, preposition, adverb, verb class, and modal features

#' Add adjective conjuncts that inherit a counted adjective's role
#'
#' @param tokens Prepared tokens.
#' @param rows Counted adjective rows.
#' @return `rows` plus coordinated ADJ conjuncts, transitively.
#' @keywords internal
expand_adjective_conjuncts <- function(tokens, rows) {
  conjuncts <- tokens[tokens$pos == "ADJ" & stringr::str_detect(tokens$dep_rel, "^conj($|:)"), ]
  repeat {
    added <- conjuncts %>%
      dplyr::semi_join(rows, by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")) %>%
      dplyr::anti_join(rows, by = c("doc_id", "sentence_id", "token_id_int"))
    if (!nrow(added)) break
    rows <- dplyr::bind_rows(rows, added)
  }
  rows
}

#' Adjective, preposition, and adverb features (French)
#'
#' f_39: one occurrence per prepositional `case` marker; a multiword
#' preposition (`à cause de`, `grâce à`) counts once, on its first word.
#' f_40: adjectives in an `amod` relation and their conjuncts, excluding
#' hyphenated forms and the quantifier tout (`tous les jours`). f_41: adjectives heading a copula or in an
#' `xcomp`/`acomp` relation to a linking verb, and their conjuncts.
#' f_42: adverb candidates other than the concord clitic ne; the pipeline then
#' removes adverbs inside f_04,
#' f_05, f_46, f_47 and f_48 occurrences (`exclude_specialized_adverbs()`).
#'
#' @param tokens Prepared tokens.
#' @param doc_ids Document IDs.
#' @param dict_lookup Dictionary.
#' @param word_lists_lookup Word lists.
#' @return Block result with f_39-f_42 evidence.
#' @keywords internal
block_adj_prep_adv_fr <- function(tokens, doc_ids, dict_lookup, word_lists_lookup) {
  keys <- c("doc_id", "sentence_id", "token_id_int")
  fixed_adp_heads <- tokens %>%
    dplyr::filter(.data$pos == "ADP", .data$dep_rel == "fixed") %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int)
  f39 <- tokens %>%
    dplyr::filter(.data$dep_rel == "case") %>%
    dplyr::filter(
      .data$pos == "ADP" |
        paste(.data$doc_id, .data$sentence_id, .data$token_id_int) %in%
        paste(fixed_adp_heads$doc_id, fixed_adp_heads$sentence_id, fixed_adp_heads$token_id_int)
    ) %>%
    count_feature_traced("f_39_prepositions")

  plain <- !stringr::str_detect(tokens$token, "-")
  f40_rows <- tokens[tokens$pos == "ADJ" & tokens$dep_rel == "amod" & plain &
                       tokens$lemma != "tout", ]
  f40_rows <- expand_adjective_conjuncts(tokens, f40_rows)
  f40 <- count_feature_traced(f40_rows[!stringr::str_detect(f40_rows$token, "-"), ],
                              "f_40_adj_attr")

  linking_verbs <- get_word_list(word_lists_lookup, "linking_matchlist")
  cop_heads <- tokens %>%
    dplyr::filter(.data$dep_rel == "cop") %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int)
  linking_ids <- tokens %>%
    dplyr::filter(.data$lemma %in% linking_verbs) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, head_token_id_int = .data$token_id_int)
  f41_rows <- dplyr::bind_rows(
    tokens[tokens$pos == "ADJ", ] %>% dplyr::semi_join(cop_heads, by = keys),
    tokens[tokens$pos == "ADJ" & tokens$dep_rel %in% c("xcomp", "acomp"), ] %>%
      dplyr::semi_join(linking_ids, by = c("doc_id", "sentence_id", "head_token_id_int"))
  ) %>%
    dplyr::anti_join(f40_rows, by = keys)
  f41 <- expand_adjective_conjuncts(tokens, f41_rows) %>%
    dplyr::anti_join(f40_rows, by = keys) %>%
    count_feature_traced("f_41_adj_pred")

  # The concord clitic ne is not an adverb in its own right: ne ... pas
  # contributes one adverb, like English not and Spanish no (C3).
  f42 <- count_feature_traced(tokens[tokens$pos == "ADV" & !tokens$match_token %in% c("ne", "n'"), ],
                              "f_42_adverbs")

  make_block_result(doc_ids, bind_evidence(f39$evidence, f40$evidence,
                                           f41$evidence, f42$evidence))
}

#' Residual adverbs (D-15)
#'
#' Removes f_42 occurrences that fall inside an occurrence of a place or time
#' adverbial, downtoner, hedge or amplifier (including every word of a phrase).
#' Emphatics, conjuncts, discourse particles and negation are not excluded.
#'
#' @param evidence Combined evidence after disambiguation.
#' @return Evidence without the excluded f_42 rows.
#' @keywords internal
exclude_specialized_adverbs <- function(evidence) {
  excluded <- c("f_04_place_adverbials", "f_05_time_adverbials", "f_46_downtoners",
                "f_47_hedges", "f_48_amplifiers")
  spans <- evidence[evidence$feature %in% excluded, ] %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id, start = .data$token_id,
                     end = .data$token_id + lengths(strsplit(.data$token, " ", fixed = TRUE)) - 1L)
  adverbs <- evidence[evidence$feature == "f_42_adverbs", ]
  inside <- dplyr::semi_join(
    adverbs, spans,
    by = dplyr::join_by("doc_id", "sentence_id", "token_id" >= "start", "token_id" <= "end")
  )
  dplyr::anti_join(evidence, dplyr::mutate(inside, .drop = TRUE),
                   by = c("doc_id", "feature", "sentence_id", "token_id"))
}

#' Specialized verb class features (French)
#'
#' One occurrence per VERB/AUX whose lemma is a single-word entry of the
#' corresponding dictionary class.
#'
#' @param tokens Prepared tokens.
#' @param doc_ids Document IDs.
#' @param dict_lookup Dictionary.
#' @return Block result with f_55-f_58 evidence.
#' @keywords internal
block_specialized_verbs_fr <- function(tokens, doc_ids, dict_lookup) {
  features <- c("f_55_verb_public", "f_56_verb_private", "f_57_verb_suasive", "f_58_verb_seem")
  verbs <- tokens[tokens$pos %in% c("VERB", "AUX"), ]
  evidence <- lapply(features, function(feature) {
    lemmas <- dictionary_to_lemmas(dict_lookup, feature)
    count_feature_traced(verbs[verbs$match_lemma %in% lemmas, ], feature)$evidence
  })
  make_block_result(doc_ids, do.call(bind_evidence, evidence))
}

#' Modal verb features (French)
#'
#' Modal features count modal verbs and periphrases grammaticalized as modals,
#' as in the English and Spanish packages; adjectival or nominal constructions
#' (`être obligé de`, `être susceptible de`, `avoir la possibilité de`) and
#' `risquer de` are not counted.
#' f_52: possibility lemmas (pouvoir). f_53: necessity lemmas (devoir,
#' falloir). f_54: finite future and conditional verbs or auxiliaries
#' (`viendra`, `aurait aimé`) and aller in the present or imperfect governing
#' an infinitive (`va partir`, recorded as the span from aller to the
#' infinitive). Each word counts once.
#'
#' @param tokens Prepared tokens with context columns.
#' @param doc_ids Document IDs.
#' @param dict_lookup Dictionary.
#' @return Block result with f_52-f_54 evidence.
#' @keywords internal
block_modals_fr <- function(tokens, doc_ids, dict_lookup) {
  verbal <- tokens$pos %in% c("VERB", "AUX")
  lemma_rows <- function(feature) {
    verbal & tokens$match_lemma %in% dictionary_to_lemmas(dict_lookup, feature)
  }

  f52 <- tokens[lemma_rows("f_52_modal_possibility"), ] %>%
    count_feature_traced("f_52_modal_possibility")
  f53 <- tokens[lemma_rows("f_53_modal_necessity"), ] %>%
    count_feature_traced("f_53_modal_necessity")
  finite <- verbal & (is.na(tokens$morph_verbform) | tokens$morph_verbform == "Fin")
  infinitive_complements <- tokens %>%
    dplyr::filter(.data$dep_rel == "xcomp", .data$pos %in% c("VERB", "AUX"),
                  .data$morph_verbform %in% "Inf") %>%
    dplyr::arrange(.data$token_id_int) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$head_token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id, .data$head_token_id_int,
                     infinitive_id = .data$token_id_int)
  aller_rows <- tokens[tokens$lemma == "aller" & finite &
                         tokens$morph_tense %in% c("Pres", "Imp"), ] %>%
    dplyr::inner_join(infinitive_complements,
                      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")) %>%
    dplyr::filter(.data$infinitive_id > .data$token_id_int)
  aller_ids <- paste(aller_rows$doc_id, aller_rows$sentence_id, aller_rows$token_id_int)
  f54_single <- tokens[!paste(tokens$doc_id, tokens$sentence_id, tokens$token_id_int) %in% aller_ids &
                         (lemma_rows("f_54_modal_predictive") |
                            (finite & (tokens$morph_tense %in% "Fut" | tokens$morph_mood %in% "Cnd"))), ] %>%
    count_feature_traced("f_54_modal_predictive")
  f54_aller <- span_evidence(
    tokens,
    data.frame(doc_id = aller_rows$doc_id, sentence_id = aller_rows$sentence_id,
               start = aller_rows$token_id_int, end = aller_rows$infinitive_id),
    "f_54_modal_predictive"
  )
  make_block_result(doc_ids, bind_evidence(f52$evidence, f53$evidence,
                                           f54_single$evidence, f54_aller))
}

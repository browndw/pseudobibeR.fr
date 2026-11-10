# Feature extraction pipeline

#' Extract French features from prepared tokens
#'
#' Every count is the number of evidence rows for its feature; each word or
#' construction counts at most once per feature, and identity respects
#' document and sentence boundaries.
#'
#' @param tokens Prepared tokens (see `prepare_parsed_tokens()`).
#' @param measure Lexical-diversity measure.
#' @param normalize Normalize counts per 1,000 word tokens.
#' @param traced Return the evidence alongside the counts.
#' @return Feature data frame, or a list with `counts` and `evidence`.
#' @importFrom rlang .data :=
#' @keywords internal
parse_biber_features <- function(tokens, measure, normalize, traced = FALSE) {
  dict <- pseudobibeR.fr::dict
  word_lists <- pseudobibeR.fr::word_lists
  doc_order <- attr(tokens, "doc_order")
  doc_ids <- tibble::tibble(doc_id = doc_order)

  relative_pronoun_candidates <- c(
    "qui", "que", "quoi", "o\u00f9", "dont",
    "lequel", "laquelle", "lesquel", "lesquelle", "lesquels", "lesquelles",
    "auquel", "auxquels", "auxquelles", "duquel", "desquels", "desquelles"
  )
  tokens <- add_token_context(tokens)
  tokens <- add_clause_flags(tokens, relative_pronoun_candidates)

  proverb_pronouns <- normalize_terms(get_word_list(word_lists, "proverb_object_pronouns"))
  neg_synthetic_terms <- normalize_terms(get_word_list(word_lists, "neg_synthetic_determiners"))
  negation_part_lemmas <- unique(c(
    normalize_terms(get_word_list(word_lists, "negation_particles")), "n'"
  ))
  negation_adverbs <- normalize_terms(get_word_list(word_lists, "neg_analytic_adverbs"))
  # faire is meteorological only with a weather complement (il fait froid).
  weather_lemmas <- setdiff(unique(c(
    normalize_terms(get_word_list(word_lists, "impersonal_verbs")), "bruiner", "tonner"
  )), "faire")
  weather_faire_terms <- c("froid", "chaud", "beau", "mauvais", "frais", "doux", "nuit",
                           "jour", "soleil", "vent", "gris", "humide", "sec")
  raising_verbs <- c("sembler", "para\u00eetre", "demeurer", "rester", "suffire", "convenir")
  wh_question_lemmas <- c(
    "qui", "que", "quoi", "o\u00f9", "quand", "comment", "pourquoi",
    "lequel", "laquelle", "lesquels", "lesquelles",
    "quel", "quelle", "quels", "quelles", "combien"
  )

  impersonal_clause_verbs <- c("arriver", "importer", "valoir", "agir", "advenir", "manquer")
  # Clausal complements of a predicate: que-clauses (ccomp/csubj) or
  # infinitival clauses introduced by de/à/que (il est facile de partir).
  marked_clauses <- tokens %>%
    dplyr::filter(.data$dep_rel == "mark", .data$match_lemma %in% c("de", "\u00e0", "que", "qu'")) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int)
  # Parsers often attach the de/que introducer directly to the adjective.
  adjective_ids <- paste(tokens$doc_id, tokens$sentence_id, tokens$token_id_int)[tokens$pos == "ADJ"]
  clause_complements <- tokens %>%
    dplyr::filter(
      stringr::str_detect(.data$dep_rel, "^(ccomp|csubj)") |
        (stringr::str_detect(.data$dep_rel, "^xcomp") &
           paste(.data$doc_id, .data$sentence_id, .data$token_id_int) %in%
           paste(marked_clauses$doc_id, marked_clauses$sentence_id, marked_clauses$token_id_int)) |
        (.data$match_lemma %in% c("de", "que", "qu'") & .data$dep_rel %in% c("mark", "advmod") &
           paste(.data$doc_id, .data$sentence_id, .data$head_token_id_int) %in% adjective_ids),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$head_token_id_int) %>%
    dplyr::mutate(has_clause_comp = TRUE)
  head_lookup <- tokens %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int",
      head_pos = "pos", head_lemma = "lemma", head_token = "token",
      head_feats = "feats", head_morph_verbform = "morph_verbform",
      head_morph_voice = "morph_voice", head_morph_tense = "morph_tense"
    )
  token_lookup <- tokens %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int",
      token_pos = "pos", token_dep_rel = "dep_rel",
      token_head_token_id_int = "head_token_id_int",
      token_morph_verbform = "morph_verbform"
    )
  subject_pronouns <- c("je", "tu", "il", "elle", "on", "nous", "vous", "ils", "elles",
                        "lui", "leur")
  subject_heads <- tokens %>%
    dplyr::filter(
      stringr::str_detect(.data$dep_rel, "^nsubj") |
        (stringr::str_detect(.data$dep_rel, "^(obj|iobj)") & .data$pos == "PRON" &
           .data$lemma %in% subject_pronouns)
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id,
                    clause_head_token_id_int = .data$head_token_id_int) %>%
    dplyr::mutate(has_subject = TRUE)

  pronouns <- block_personal_pronouns_fr(
    tokens, doc_ids, head_lookup, dict, clause_complements, weather_lemmas,
    weather_faire_terms, raising_verbs, impersonal_clause_verbs, wh_question_lemmas
  )
  pronoun_it <- pronouns$evidence[pronouns$evidence$feature == "f_09_pronoun_it", ]
  nominal_infinitives <- nominal_infinitive_rows_fr(tokens, word_lists, dict)
  gerondif_nouns <- gerondif_noun_rows_fr(tokens, word_lists)
  resumptive <- resumptive_relative_rows_fr(tokens, word_lists)
  blocks <- list(
    block_tense_fr(tokens, doc_ids),
    block_aux_tense_fr(tokens, doc_ids, head_lookup, proverb_pronouns),
    pronouns,
    block_lexical_membership_fr(tokens, doc_ids, word_lists, pronoun_it, nominal_infinitives,
                                gerondif_nouns),
    block_passive_voice_fr(tokens, doc_ids, head_lookup, c("aux:pass", "auxpass")),
    block_clause_embedding_fr(tokens, doc_ids, head_lookup),
    block_reduced_complements_fr(
      tokens, doc_ids, head_lookup,
      normalize_terms(get_word_list(word_lists, "reduced_complement_predicates")),
      nominal_infinitives
    ),
    block_participial_clauses_fr(tokens, doc_ids, head_lookup, nominal_infinitives,
                                 gerondif_nouns),
    block_relatives_fr(tokens, doc_ids, head_lookup, resumptive),
    block_contractions_fr(tokens, doc_ids, word_lists),
    block_adj_prep_adv_fr(tokens, doc_ids, dict, word_lists),
    block_specialized_verbs_fr(tokens, doc_ids, dict),
    block_modals_fr(tokens, doc_ids, dict),
    block_stranded_split_fr(tokens, doc_ids, resumptive, word_lists, nominal_infinitives),
    block_split_coordination_fr(tokens, doc_ids, token_lookup, subject_heads,
                                head_lookup, negation_part_lemmas),
    block_negation_fr(tokens, doc_ids, neg_synthetic_terms, negation_part_lemmas,
                      negation_adverbs)
  )
  evidence <- do.call(bind_evidence, lapply(blocks, extract_evidence))
  lexical <- dictionary_evidence(tokens, .dict_only_features, cross_feature_precedence = TRUE)
  evidence <- bind_evidence(evidence, lexical$evidence)
  evidence <- dplyr::distinct(evidence, .data$doc_id, .data$feature, .data$sentence_id,
                              .data$token_id, .keep_all = TRUE)
  evidence <- disambiguate_stance_fr(evidence, tokens)
  evidence <- gate_indefinite_pronouns_fr(evidence, tokens)
  evidence <- exclude_specialized_adverbs(evidence)
  evidence <- evidence[evidence$feature %in% .evidence_features, ]

  counts <- doc_ids
  for (feature in .evidence_features) {
    rows <- evidence$doc_id[evidence$feature == feature]
    counts[[feature]] <- tabulate(match(rows, counts$doc_id), nbins = nrow(counts))
  }
  for (feature in .zero_output_features) counts[[feature]] <- 0L
  counts <- dplyr::left_join(counts, block_lexical_complexity_fr(tokens, doc_ids), by = "doc_id")

  if (normalize) {
    if (any(counts$n_tokens == 0L)) {
      warning("Documents with no word tokens have undefined normalized frequencies.",
              call. = FALSE)
    }
    counts$tot_counts <- ifelse(counts$n_tokens > 0L, counts$n_tokens, NA_real_)
    counts <- normalize_counts(counts)
  }
  if (measure != "none") {
    counts$f_43_type_token <- lexical_diversity(tokens, doc_ids, measure)
  }
  counts <- counts[, c("doc_id", intersect(names(.feature_categories), names(counts)))]
  counts <- as.data.frame(counts, stringsAsFactors = FALSE)
  if (traced) list(counts = counts, evidence = evidence) else counts
}

#' Lexical diversity for a corpus (French)
#'
#' One measure for the whole corpus: default MATTR falls back to TTR for every
#' document when any document has fewer than 200 retained tokens; explicitly
#' selected TTR, CTTR and MSTTR are honored. Documents with no retained tokens
#' receive `NA` with a warning.
#'
#' @param tokens Prepared tokens.
#' @param doc_ids Document IDs.
#' @param measure Requested measure.
#' @return Numeric vector aligned with `doc_ids`.
#' @keywords internal
lexical_diversity <- function(tokens, doc_ids, measure) {
  document_tokens <- lexical_diversity_tokens(tokens, doc_ids)
  lengths <- quanteda::ntoken(document_tokens)
  nonempty <- lengths > 0L
  if (any(!nonempty)) {
    warning("Documents with no lexical-diversity tokens have undefined lexical diversity: ",
            paste(doc_ids$doc_id[!nonempty], collapse = ", "), ".", call. = FALSE)
  }
  actual <- measure
  if (measure == "MATTR" && any(lengths < 200L)) {
    message("Setting type-to-token ratio to TTR")
    actual <- "TTR"
  }
  values <- rep(NA_real_, nrow(doc_ids))
  if (any(nonempty)) {
    lexdiv <- quanteda.textstats::textstat_lexdiv(document_tokens[nonempty], measure = actual)
    values[nonempty] <- lexdiv[[actual]][match(as.character(which(nonempty)), lexdiv$document)]
  }
  values
}

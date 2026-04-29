#' Audit feature-bearing sentence chunks
#'
#' Returns sentence-level chunks whose parsed tokens trigger one or more
#' `pseudobibeR.fr` feature counts. This is intended as an audit aid for
#' collaborators who want to inspect representative examples without reviewing
#' every matching sentence in a large corpus.
#'
#' The function reuses the same parser-specific feature extraction logic as
#' [biber()] by temporarily treating each sentence as its own document. This
#' keeps auditing aligned with the package's counting heuristics for both
#' lexical and syntactic features.
#'
#' @param tokens A dataset of tokens created by `spacyr::spacy_parse()` or
#'   `udpipe::udpipe_annotate()`.
#' @param feature Optional feature identifier such as `"f_19_be_main_verb"`.
#' @param category Optional category code or label such as `"G"`,
#'   `"Passives"`, or `"Modals"`. Expands to all features in the category.
#' @param sample_n Optional maximum number of rows to return after filtering.
#'   When supplied, rows are sampled at random.
#' @param seed Optional random seed used when `sample_n` or `max_per_doc` is
#'   supplied.
#' @param max_per_doc Optional maximum number of rows to keep per source
#'   document before global sampling.
#' @param window Number of tokens to show on the left and right side of an
#'   exact match when KWIC-style anchors are available.
#' @return A tibble with one row per matching sentence chunk and columns for the
#'   source document, sentence, feature, category, feature count within the
#'   chunk, reconstructed sentence text, and `left`/`keyword`/`right` context
#'   columns when exact token anchors are available. The return value has class
#'   `audit_features_result`, which provides a compact print method.
#' @examples
#' audit_features(spacy_samples, feature = "f_19_be_main_verb", sample_n = 5)
#' if (requireNamespace("udpipe", quietly = TRUE)) {
#'   audit_features(udpipe_samples, category = "Modals", sample_n = 10, seed = 1)
#' }
#' @export
audit_features <- function(tokens,
                           feature = NULL,
                           category = NULL,
                           sample_n = NULL,
                           seed = NULL,
                           max_per_doc = NULL,
                           window = 5) {
  UseMethod("audit_features")
}

.audit_feature_categories <- list(
  A = c("f_01_past_tense", "f_02_perfect_aspect", "f_03_present_tense"),
  B = c("f_04_place_adverbials", "f_05_time_adverbials"),
  C = c(
    "f_06_first_person_pronouns", "f_07_second_person_pronouns",
    "f_08_third_person_pronouns", "f_09_pronoun_it",
    "f_10_demonstrative_pronoun", "f_11_indefinite_pronouns",
    "f_12_proverb_do"
  ),
  D = c("f_13_wh_question"),
  E = c("f_14_nominalizations", "f_15_gerunds", "f_16_other_nouns"),
  F = c("f_17_agentless_passives", "f_18_by_passives"),
  G = c("f_19_be_main_verb", "f_20_existential_there"),
  H = c(
    "f_21_that_verb_comp", "f_22_that_adj_comp", "f_23_wh_clause",
    "f_24_infinitives", "f_25_present_participle", "f_26_past_participle",
    "f_27_past_participle_whiz", "f_28_present_participle_whiz",
    "f_29_that_subj", "f_30_that_obj", "f_31_wh_subj", "f_32_wh_obj",
    "f_33_pied_piping", "f_34_sentence_relatives", "f_35_because",
    "f_36_though", "f_37_if", "f_38_other_adv_sub"
  ),
  I = c("f_39_prepositions", "f_40_adj_attr", "f_41_adj_pred", "f_42_adverbs"),
  J = c("f_43_type_token", "f_44_mean_word_length"),
  K = c(
    "f_45_conjuncts", "f_46_downtoners", "f_47_hedges",
    "f_48_amplifiers", "f_49_emphatics", "f_50_discourse_particles",
    "f_51_demonstratives"
  ),
  L = c("f_52_modal_possibility", "f_53_modal_necessity", "f_54_modal_predictive"),
  M = c("f_55_verb_public", "f_56_verb_private", "f_57_verb_suasive", "f_58_verb_seem"),
  N = c(
    "f_59_contractions", "f_60_that_deletion", "f_61_stranded_preposition",
    "f_62_split_infinitive", "f_63_split_auxiliary"
  ),
  O = c("f_64_phrasal_coordination", "f_65_clausal_coordination"),
  P = c("f_66_neg_synthetic", "f_67_neg_analytic")
)

.audit_category_aliases <- c(
  "tense and aspect markers" = "A",
  "place and time adverbials" = "B",
  "pronouns and pro-verbs" = "C",
  "questions" = "D",
  "nominal forms" = "E",
  "passives" = "F",
  "stative forms" = "G",
  "subordination features" = "H",
  "prepositional phrases, adjectives and adverbs" = "I",
  "lexical specificity" = "J",
  "lexical classes" = "K",
  "modals" = "L",
  "specialized verb classes" = "M",
  "reduced forms and dispreferred structures" = "N",
  "co-ordination" = "O",
  "coordination" = "O",
  "negation" = "P"
)

.audit_unsupported_features <- c("f_43_type_token", "f_44_mean_word_length")

new_audit_features_result <- function(x) {
  structure(x, class = c("audit_features_result", class(x)))
}

resolve_audit_feature_set <- function(feature = NULL, category = NULL) {
  if (is.null(feature) == is.null(category)) {
    stop("Supply exactly one of 'feature' or 'category'.", call. = FALSE)
  }

  if (!is.null(feature)) {
    if (!is.character(feature) || length(feature) < 1) {
      stop("'feature' must be a character vector of feature identifiers.", call. = FALSE)
    }
    selected <- unique(feature)
  } else {
    if (!is.character(category) || length(category) != 1) {
      stop("'category' must be a single character string.", call. = FALSE)
    }
    category_key <- stringr::str_trim(stringr::str_to_lower(category))
    category_code <- if (nchar(category_key) == 1) {
      stringr::str_to_upper(category_key)
    } else {
      .audit_category_aliases[[category_key]]
    }

    if (is.null(category_code) || !category_code %in% names(.audit_feature_categories)) {
      stop("Unknown category '", category, "'.", call. = FALSE)
    }

    selected <- .audit_feature_categories[[category_code]]
  }

  unknown <- setdiff(selected, unlist(.audit_feature_categories, use.names = FALSE))
  if (length(unknown) > 0) {
    stop("Unknown feature(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  unsupported <- intersect(selected, .audit_unsupported_features)
  if (length(unsupported) > 0) {
    stop(
      "Audit chunks are not supported for lexical summary feature(s): ",
      paste(unsupported, collapse = ", "),
      ". Choose a tagged/count feature instead.",
      call. = FALSE
    )
  }

  selected
}

feature_to_category_code <- function(feature) {
  purrr::map_chr(feature, function(feature_id) {
    matches <- names(.audit_feature_categories)[purrr::map_lgl(
      .audit_feature_categories,
      ~ feature_id %in% .x
    )]
    if (length(matches) == 0) NA_character_ else matches[[1]]
  })
}

validate_audit_sampling <- function(sample_n = NULL, max_per_doc = NULL, seed = NULL) {
  if (!is.null(sample_n) && (!is.numeric(sample_n) || length(sample_n) != 1 || sample_n < 1)) {
    stop("'sample_n' must be a single positive number.", call. = FALSE)
  }

  if (!is.null(max_per_doc) && (!is.numeric(max_per_doc) || length(max_per_doc) != 1 || max_per_doc < 1)) {
    stop("'max_per_doc' must be a single positive number.", call. = FALSE)
  }

  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1)) {
    stop("'seed' must be a single numeric value.", call. = FALSE)
  }
}

normalize_window <- function(window) {
  if (!is.numeric(window) || length(window) != 1 || window < 0) {
    stop("'window' must be a single non-negative number.", call. = FALSE)
  }
  as.integer(window)
}

build_sentence_audit_index <- function(tokens) {
  tokens %>%
    dplyr::mutate(
      sentence_id = dplyr::coalesce(.data$sentence_id, 1L),
      audit_doc_id = paste(.data$doc_id, .data$sentence_id, sep = "::")
    ) %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id, .data$audit_doc_id) %>%
    dplyr::summarise(
      chunk = paste(.data$token, collapse = " "),
      token_count = dplyr::n(),
      .groups = "drop"
    )
}

counts_to_audit_rows <- function(counts, selected_features) {
  purrr::map_dfr(selected_features, function(feature_id) {
    if (!feature_id %in% colnames(counts)) {
      return(tibble::tibble())
    }

    counts %>%
      dplyr::filter(.data[[feature_id]] > 0) %>%
      dplyr::transmute(
        audit_doc_id = .data$doc_id,
        feature = feature_id,
        count = .data[[feature_id]]
      )
  })
}

sample_audit_rows <- function(rows, sample_n = NULL, max_per_doc = NULL, seed = NULL) {
  if (nrow(rows) == 0) {
    return(rows)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  sampled <- rows

  if (!is.null(max_per_doc)) {
    sampled <- sampled %>%
      dplyr::group_by(.data$doc_id) %>%
      dplyr::group_modify(~ dplyr::slice_sample(.x, n = min(max_per_doc, nrow(.x)))) %>%
      dplyr::ungroup()
  }

  if (!is.null(sample_n) && nrow(sampled) > sample_n) {
    sampled <- sampled %>%
      dplyr::slice_sample(n = sample_n)
  }

  sampled
}

split_surface_pattern <- function(pattern) {
  stringr::str_split(pattern, "_", simplify = FALSE)[[1]]
}

locate_surface_pattern_matches <- function(tokens, feature, patterns) {
  if (length(patterns) == 0) {
    return(tibble::tibble())
  }

  sentence_slices <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::group_split()

  matches <- purrr::map_dfr(sentence_slices, function(sentence_tokens) {
    sentence_tokens <- sentence_tokens %>%
      dplyr::arrange(.data$token_id_int)

    surface_tokens <- sentence_tokens$token
    if (length(surface_tokens) == 0) {
      return(tibble::tibble())
    }

    purrr::map_dfr(patterns, function(pattern) {
      pattern_tokens <- split_surface_pattern(pattern)
      pattern_length <- length(pattern_tokens)
      if (pattern_length == 0 || pattern_length > length(surface_tokens)) {
        return(tibble::tibble())
      }

      starts <- seq_len(length(surface_tokens) - pattern_length + 1L)
      matched_starts <- starts[purrr::map_lgl(starts, function(start_idx) {
        end_idx <- start_idx + pattern_length - 1L
        identical(surface_tokens[start_idx:end_idx], pattern_tokens)
      })]

      if (length(matched_starts) == 0) {
        return(tibble::tibble())
      }

      purrr::map_dfr(matched_starts, function(start_idx) {
        end_idx <- start_idx + pattern_length - 1L
        tibble::tibble(
          feature = feature,
          doc_id = sentence_tokens$doc_id[[1]],
          sentence_id = sentence_tokens$sentence_id[[1]],
          start_token_id_int = sentence_tokens$token_id_int[[start_idx]],
          end_token_id_int = sentence_tokens$token_id_int[[end_idx]],
          keyword_token = paste(sentence_tokens$token[start_idx:end_idx], collapse = " "),
          count = 1L,
          match_type = "exact"
        )
      })
    })
  })

  if (nrow(matches) == 0) {
    return(matches)
  }

  contained_keys <- matches %>%
    dplyr::mutate(match_id = dplyr::row_number()) %>%
    dplyr::inner_join(
      matches %>%
        dplyr::mutate(other_id = dplyr::row_number()),
      by = c("feature", "doc_id", "sentence_id"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(
      .data$match_id != .data$other_id,
      .data$start_token_id_int.y <= .data$start_token_id_int.x,
      .data$end_token_id_int.y >= .data$end_token_id_int.x,
      (
        .data$start_token_id_int.y < .data$start_token_id_int.x |
          .data$end_token_id_int.y > .data$end_token_id_int.x
      )
    ) %>%
    dplyr::distinct(.data$match_id) %>%
    dplyr::pull(.data$match_id)

  matches %>%
    dplyr::mutate(match_id = dplyr::row_number()) %>%
    dplyr::filter(!.data$match_id %in% contained_keys) %>%
    dplyr::select(-"match_id")
}

prepare_exact_locator_context <- function(tokens) {
  relative_pronoun_candidates <- c(
    "qui", "que", "quoi", "\u00f9", "ou", "dont",
    "lequel", "laquelle", "lesquel", "lesquelle", "lesquels", "lesquelles",
    "auquel", "auxquels", "auxquelles",
    "duquel", "desquels", "desquelles"
  )

  tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::arrange(.data$token_id_int, .by_group = TRUE) %>%
    dplyr::mutate(
      position_in_sentence = dplyr::row_number(),
      sentence_length = dplyr::n(),
      is_sentence_initial = .data$position_in_sentence <= 2,
      is_sentence_final = .data$position_in_sentence > (.data$sentence_length - 2),
      sentence_has_question = any(.data$token == "?" & .data$pos == "PUNCT"),
      prev_token = dplyr::lag(.data$token),
      prev_lemma = dplyr::lag(.data$lemma),
      prev_pos = dplyr::lag(.data$pos),
      prev_token_id_int = dplyr::lag(.data$token_id_int),
      prev2_token = dplyr::lag(.data$token, 2),
      prev2_lemma = dplyr::lag(.data$lemma, 2),
      prev2_pos = dplyr::lag(.data$pos, 2),
      prev2_token_id_int = dplyr::lag(.data$token_id_int, 2),
      prev3_token = dplyr::lag(.data$token, 3),
      prev3_lemma = dplyr::lag(.data$lemma, 3),
      prev3_pos = dplyr::lag(.data$pos, 3),
      prev3_token_id_int = dplyr::lag(.data$token_id_int, 3),
      prev4_token = dplyr::lag(.data$token, 4),
      prev4_lemma = dplyr::lag(.data$lemma, 4),
      prev4_pos = dplyr::lag(.data$pos, 4),
      prev4_token_id_int = dplyr::lag(.data$token_id_int, 4),
      next_token = dplyr::lead(.data$token),
      next_lemma = dplyr::lead(.data$lemma),
      next_pos = dplyr::lead(.data$pos),
      next_token_id_int = dplyr::lead(.data$token_id_int),
      next_prontype = dplyr::lead(.data$morph_prontype),
      next2_token = dplyr::lead(.data$token, 2),
      next2_lemma = dplyr::lead(.data$lemma, 2),
      next2_pos = dplyr::lead(.data$pos, 2),
      next2_token_id_int = dplyr::lead(.data$token_id_int, 2),
      next_morph_verbform = dplyr::lead(.data$morph_verbform),
      next2_morph_verbform = dplyr::lead(.data$morph_verbform, 2),
      next3_token_id_int = dplyr::lead(.data$token_id_int, 3),
      passive_agent_next2 = dplyr::lead(.data$token %in% c("par"), 2, default = FALSE),
      passive_agent_next3 = dplyr::lead(.data$token %in% c("par"), 3, default = FALSE),
      is_relative_pronoun = dplyr::if_else(
        (!is.na(.data$morph_prontype) & stringr::str_detect(.data$morph_prontype, "Rel")) |
          (
            .data$lemma %in% relative_pronoun_candidates &
              stringr::str_detect(
                dplyr::coalesce(.data$dep_rel, ""),
                "^(nsubj|obj|obl|iobj|expl|mark|acl)"
              )
          ),
        TRUE,
        FALSE
      )
    ) %>%
    dplyr::mutate(
      is_relative_subject = .data$is_relative_pronoun &
        stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "nsubj"),
      is_relative_object = .data$is_relative_pronoun &
        (
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "obj") |
            (
              stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^obl") &
                .data$lemma %in% c(
                  "lequel", "laquelle", "lesquel", "lesquelle", "lesquels", "lesquelles",
                  "auquel", "auxquels", "auxquelles",
                  "duquel", "desquels", "desquelles",
                  "dont"
                ) &
                !dplyr::lag(.data$pos == "ADP", default = FALSE)
            ) |
            (
              .data$lemma %in% c("que") &
                stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^mark")
            )
        )
    ) %>%
    dplyr::ungroup()
}

locate_nominal_feature_matches <- function(tokens, selected_features, word_lists_lookup) {
  requested <- intersect(selected_features, c("f_14_nominalizations", "f_15_gerunds", "f_16_other_nouns"))
  if (length(requested) == 0) {
    return(tibble::tibble())
  }

  nominalization_suffixes <- get_word_list(word_lists_lookup, "nominalization_suffixes")
  nominalization_pattern <- if (length(nominalization_suffixes) > 0) {
    escaped <- stringr::str_replace_all(nominalization_suffixes, "([\\W])", "\\\\\\1")
    paste0("(", paste(escaped, collapse = "|"), ")$")
  } else {
    "^$"
  }
  nominal_stoplist <- normalize_terms(get_word_list(word_lists_lookup, "nominalization_stoplist"))
  gerund_stoplist <- normalize_terms(get_word_list(word_lists_lookup, "gerund_stoplist"))

  f14_tokens <- tokens %>%
    dplyr::filter(
      .data$pos == "NOUN",
      stringr::str_detect(.data$lemma, nominalization_pattern),
      !.data$lemma %in% nominal_stoplist
    ) %>%
    dplyr::transmute(
      feature = "f_14_nominalizations",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  f15_tokens <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      !is.na(.data$morph_verbform),
      (
        .data$morph_verbform == "Ger" |
          (.data$morph_verbform == "Part" & .data$morph_tense == "Pres")
      ),
      !.data$lemma %in% gerund_stoplist
    )

  fallback_gerunds <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::arrange(.data$sentence_id, .data$token_id_int, .by_group = TRUE) %>%
    dplyr::filter(
      .data$pos %in% c("NOUN", "PROPN"),
      stringr::str_detect(.data$lemma, "ant$"),
      dplyr::lag(.data$token, default = "") == "en",
      dplyr::lag(.data$pos, default = "") == "ADP",
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(nmod|obl|advcl)")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!.data$lemma %in% gerund_stoplist)

  if (nrow(fallback_gerunds) > 0) {
    f15_tokens <- dplyr::bind_rows(f15_tokens, fallback_gerunds) %>%
      dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE)
  }

  f15_rows <- f15_tokens %>%
    dplyr::transmute(
      feature = "f_15_gerunds",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  excluded_noun_ids <- dplyr::bind_rows(
    f14_tokens %>%
      dplyr::transmute(.data$doc_id, .data$sentence_id, token_id_int = .data$start_token_id_int),
    f15_rows %>%
      dplyr::left_join(
        tokens %>% dplyr::select("doc_id", "sentence_id", "token_id_int", "pos"),
        by = c("doc_id", "sentence_id", "start_token_id_int" = "token_id_int")
      ) %>%
      dplyr::filter(.data$pos %in% c("NOUN", "PROPN")) %>%
      dplyr::transmute(.data$doc_id, .data$sentence_id, token_id_int = .data$start_token_id_int)
  ) %>%
    dplyr::distinct()

  f16_rows <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("NOUN", "PROPN"),
      !stringr::str_detect(.data$token, "-")
    ) %>%
    dplyr::anti_join(excluded_noun_ids, by = c("doc_id", "sentence_id", "token_id_int")) %>%
    dplyr::transmute(
      feature = "f_16_other_nouns",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  dplyr::bind_rows(f14_tokens, f15_rows, f16_rows) %>%
    dplyr::filter(.data$feature %in% requested)
}

locate_stative_feature_matches <- function(tokens, selected_features) {
  requested <- intersect(selected_features, c("f_19_be_main_verb", "f_20_existential_there"))
  if (length(requested) == 0) {
    return(tibble::tibble())
  }

  tokens_with_context <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::arrange(.data$token_id_int, .by_group = TRUE) %>%
    dplyr::mutate(
      lag1_lemma = dplyr::lag(.data$lemma),
      lag2_lemma = dplyr::lag(.data$lemma, 2),
      lag2_pos = dplyr::lag(.data$pos, 2)
    ) %>%
    dplyr::ungroup()

  f19_rows <- tokens_with_context %>%
    dplyr::filter(
      .data$lemma == "\u00eatre",
      !stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "aux")
    ) %>%
    dplyr::transmute(
      feature = "f_19_be_main_verb",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  f20_rows <- tokens_with_context %>%
    dplyr::filter(
      .data$lemma == "avoir",
      .data$pos %in% c("VERB", "AUX"),
      .data$lag1_lemma == "y",
      .data$lag2_lemma == "il",
      .data$lag2_pos == "PRON"
    ) %>%
    dplyr::transmute(
      feature = "f_20_existential_there",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = pmax(.data$token_id_int - 2L, 1L),
      end_token_id_int = .data$token_id_int,
      keyword_token = "il y a",
      count = 1L,
      match_type = "exact"
    )

  dplyr::bind_rows(f19_rows, f20_rows) %>%
    dplyr::filter(.data$feature %in% requested)
}

locate_lexical_class_matches <- function(tokens, selected_features, dict_lookup) {
  requested <- intersect(
    selected_features,
    c("f_45_conjuncts", "f_47_hedges", "f_48_amplifiers", "f_50_discourse_particles")
  )
  if (length(requested) == 0) {
    return(tibble::tibble())
  }

  tokens_with_context <- prepare_exact_locator_context(tokens)

  rows <- list()

  if ("f_45_conjuncts" %in% requested) {
    conjunct_patterns <- setdiff(dict_lookup[["f_45_conjuncts"]], "donc")
    conjunct_surface <- locate_surface_pattern_matches(tokens, "f_45_conjuncts", conjunct_patterns)

    donc_conjunct <- tokens_with_context %>%
      dplyr::filter(
        .data$lemma == "donc",
        !.data$is_sentence_initial,
        !.data$is_sentence_final,
        !.data$sentence_has_question
      ) %>%
      dplyr::transmute(
        feature = "f_45_conjuncts",
        doc_id = .data$doc_id,
        sentence_id = .data$sentence_id,
        start_token_id_int = .data$token_id_int,
        end_token_id_int = .data$token_id_int,
        keyword_token = .data$token,
        count = 1L,
        match_type = "exact"
      )

    ensuite_conjunct <- tokens_with_context %>%
      dplyr::filter(
        .data$lemma == "ensuite",
        .data$prev_token %in% c(",", ";"),
        !.data$is_sentence_initial
      ) %>%
      dplyr::transmute(
        feature = "f_45_conjuncts",
        doc_id = .data$doc_id,
        sentence_id = .data$sentence_id,
        start_token_id_int = .data$token_id_int,
        end_token_id_int = .data$token_id_int,
        keyword_token = .data$token,
        count = 1L,
        match_type = "exact"
      )

    rows[["f45"]] <- dplyr::bind_rows(conjunct_surface, donc_conjunct, ensuite_conjunct) %>%
      dplyr::distinct(.data$feature, .data$doc_id, .data$sentence_id, .data$start_token_id_int, .keep_all = TRUE)
  }

  if ("f_47_hedges" %in% requested) {
    rows[["f47"]] <- locate_surface_pattern_matches(tokens, "f_47_hedges", dict_lookup[["f_47_hedges"]])
  }

  if ("f_48_amplifiers" %in% requested) {
    amplifier_surface <- locate_surface_pattern_matches(tokens, "f_48_amplifiers", dict_lookup[["f_48_amplifiers"]])
    vraiment_amplifier <- tokens_with_context %>%
      dplyr::filter(
        .data$lemma == "vraiment",
        .data$next_pos %in% c("ADJ", "ADV") |
          .data$prev_pos %in% c("ADJ", "ADV")
      ) %>%
      dplyr::transmute(
        feature = "f_48_amplifiers",
        doc_id = .data$doc_id,
        sentence_id = .data$sentence_id,
        start_token_id_int = .data$token_id_int,
        end_token_id_int = .data$token_id_int,
        keyword_token = .data$token,
        count = 1L,
        match_type = "exact"
      )
    rows[["f48"]] <- dplyr::bind_rows(amplifier_surface, vraiment_amplifier) %>%
      dplyr::distinct(.data$feature, .data$doc_id, .data$sentence_id, .data$start_token_id_int, .keep_all = TRUE)
  }

  if ("f_50_discourse_particles" %in% requested) {
    discourse_surface <- locate_surface_pattern_matches(tokens, "f_50_discourse_particles", dict_lookup[["f_50_discourse_particles"]])
    donc_discourse <- tokens_with_context %>%
      dplyr::filter(
        .data$lemma == "donc",
        .data$is_sentence_initial |
          .data$is_sentence_final |
          .data$sentence_has_question
      ) %>%
      dplyr::transmute(
        feature = "f_50_discourse_particles",
        doc_id = .data$doc_id,
        sentence_id = .data$sentence_id,
        start_token_id_int = .data$token_id_int,
        end_token_id_int = .data$token_id_int,
        keyword_token = .data$token,
        count = 1L,
        match_type = "exact"
      )
    rows[["f50"]] <- dplyr::bind_rows(discourse_surface, donc_discourse) %>%
      dplyr::distinct(.data$feature, .data$doc_id, .data$sentence_id, .data$start_token_id_int, .keep_all = TRUE)
  }

  dplyr::bind_rows(rows)
}

locate_passive_feature_matches <- function(tokens, selected_features, engine) {
  requested <- intersect(selected_features, c("f_17_agentless_passives", "f_18_by_passives"))
  if (length(requested) == 0) {
    return(tibble::tibble())
  }

  passive_rel_values <- if (engine == "spacy") c("auxpass", "aux:pass") else "aux:pass"
  tokens_with_context <- prepare_exact_locator_context(tokens)
  head_lookup <- tokens_with_context %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int",
      head_pos = "pos",
      head_token = "token",
      head_token_id_int = "token_id_int"
    )

  passive_candidates <- tokens_with_context %>%
    dplyr::filter(.data$dep_rel %in% passive_rel_values) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "head_token_id_int")
    )

  f17 <- passive_candidates %>%
    dplyr::filter(
      .data$head_pos == "VERB",
      !.data$passive_agent_next2,
      !.data$passive_agent_next3
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$head_token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_17_agentless_passives",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = pmin(.data$token_id_int, .data$head_token_id_int),
      end_token_id_int = pmax(.data$token_id_int, .data$head_token_id_int),
      keyword_token = paste(.data$token, .data$head_token),
      count = 1L,
      match_type = "exact"
    )

  f18 <- passive_candidates %>%
    dplyr::filter(
      .data$head_pos == "VERB",
      (.data$passive_agent_next2 | .data$passive_agent_next3)
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$head_token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_18_by_passives",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = pmin(.data$token_id_int, .data$head_token_id_int),
      end_token_id_int = dplyr::if_else(.data$passive_agent_next3, .data$next3_token_id_int, .data$next2_token_id_int),
      keyword_token = paste(.data$token, .data$head_token),
      count = 1L,
      match_type = "exact"
    )

  dplyr::bind_rows(f17, f18) %>%
    dplyr::filter(.data$feature %in% requested)
}

locate_modal_feature_matches <- function(tokens, selected_features, dict_lookup) {
  requested <- intersect(selected_features, c("f_52_modal_possibility", "f_53_modal_necessity", "f_54_modal_predictive"))
  if (length(requested) == 0) {
    return(tibble::tibble())
  }

  tokens_with_context <- prepare_exact_locator_context(tokens)
  possibility_lemmas <- dictionary_to_lemmas(dict_lookup, "f_52_modal_possibility")
  necessity_lemmas <- dictionary_to_lemmas(dict_lookup, "f_53_modal_necessity")
  predictive_lemmas <- dictionary_to_lemmas(dict_lookup, "f_54_modal_predictive")

  lemma_rows <- function(feature, lemmas) {
    tokens_with_context %>%
      dplyr::filter(
        .data$lemma %in% lemmas,
        .data$pos %in% c("VERB", "AUX")
      ) %>%
      dplyr::transmute(
        feature = feature,
        doc_id = .data$doc_id,
        sentence_id = .data$sentence_id,
        start_token_id_int = .data$token_id_int,
        end_token_id_int = .data$token_id_int,
        keyword_token = .data$token,
        count = 1L,
        match_type = "exact"
      )
  }

  has_de_inf <- function(df) {
    df %>%
      dplyr::filter(
        .data$next_lemma == "de",
        .data$next_pos == "ADP",
        .data$next2_pos %in% c("VERB", "AUX"),
        .data$next2_morph_verbform == "Inf"
      )
  }

  possibilite_pattern <- tokens_with_context %>%
    dplyr::filter(
      .data$lemma == "possibilit\u00e9",
      .data$prev2_lemma == "avoir"
    ) %>%
    has_de_inf() %>%
    dplyr::transmute(
      feature = "f_52_modal_possibility",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$prev2_token_id_int,
      end_token_id_int = .data$next2_token_id_int,
      keyword_token = paste(.data$prev2_token, .data$prev_token, .data$token, .data$next_token, .data$next2_token),
      count = 1L,
      match_type = "exact"
    )

  etre_support_pattern <- function(feature, target_lemmas) {
    tokens_with_context %>%
      dplyr::filter(
        .data$lemma %in% target_lemmas,
        (.data$prev_lemma == "\u00eatre" | .data$prev2_lemma == "\u00eatre")
      ) %>%
      has_de_inf() %>%
      dplyr::transmute(
        feature = feature,
        doc_id = .data$doc_id,
        sentence_id = .data$sentence_id,
        start_token_id_int = dplyr::if_else(.data$prev_lemma == "\u00eatre", .data$prev_token_id_int, .data$prev2_token_id_int),
        end_token_id_int = .data$next2_token_id_int,
        keyword_token = paste(.data$token, .data$next_token, .data$next2_token),
        count = 1L,
        match_type = "exact"
      )
  }

  predictive_aller <- tokens_with_context %>%
    dplyr::filter(
      .data$lemma == "aller",
      .data$pos %in% c("VERB", "AUX"),
      .data$next_pos %in% c("VERB", "AUX"),
      .data$next_morph_verbform == "Inf"
    ) %>%
    dplyr::transmute(
      feature = "f_54_modal_predictive",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$next_token_id_int,
      keyword_token = paste(.data$token, .data$next_token),
      count = 1L,
      match_type = "exact"
    )

  risquer_pattern <- tokens_with_context %>%
    dplyr::filter(.data$lemma == "risquer") %>%
    has_de_inf() %>%
    dplyr::transmute(
      feature = "f_54_modal_predictive",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$next2_token_id_int,
      keyword_token = paste(.data$token, .data$next_token, .data$next2_token),
      count = 1L,
      match_type = "exact"
    )

  etre_future_pattern <- tokens_with_context %>%
    dplyr::filter(
      .data$token %in% c("sera", "serait"),
      .data$lemma == "\u00eatre"
    ) %>%
    dplyr::transmute(
      feature = "f_54_modal_predictive",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  rows <- dplyr::bind_rows(
    lemma_rows("f_52_modal_possibility", possibility_lemmas),
    possibilite_pattern,
    lemma_rows("f_53_modal_necessity", necessity_lemmas),
    etre_support_pattern("f_53_modal_necessity", c("obliger", "n\u00e9cessaire")),
    lemma_rows("f_54_modal_predictive", predictive_lemmas),
    predictive_aller,
    risquer_pattern,
    etre_support_pattern("f_54_modal_predictive", c("susceptible")),
    etre_future_pattern
  )

  rows %>%
    dplyr::filter(.data$feature %in% requested) %>%
    dplyr::distinct(.data$feature, .data$doc_id, .data$sentence_id, .data$start_token_id_int, .keep_all = TRUE)
}

locate_relative_feature_matches <- function(tokens, selected_features) {
  requested <- intersect(selected_features, c("f_29_that_subj", "f_30_that_obj", "f_31_wh_subj", "f_32_wh_obj", "f_33_pied_piping", "f_34_sentence_relatives"))
  if (length(requested) == 0) {
    return(tibble::tibble())
  }

  tokens_with_context <- prepare_exact_locator_context(tokens)
  relative_subject_that_lemmas <- c("qui")
  relative_object_that_lemmas <- c("que")
  wh_subject_relative_lemmas <- c("lequel", "laquelle", "lesquel", "lesquelle", "lesquels", "lesquelles")
  wh_object_relative_lemmas <- c(
    "dont",
    "lequel", "laquelle", "lesquel", "lesquelle", "lesquels", "lesquelles",
    "duquel", "desquels", "desquelles",
    "auquel", "auxquels", "auxquelles"
  )
  pied_piping_relative_lemmas <- c(
    "dont",
    "lequel", "laquelle", "lesquels", "lesquelles",
    "auquel", "auxquels", "auxquelles",
    "duquel", "desquels", "desquelles"
  )
  sentence_relative_anchors <- c("ce", "cela", "ceci", "celui", "celle", "ceux", "celles")

  build_rel_rows <- function(df, feature, start_col = "token_id_int", end_col = "token_id_int") {
    df %>%
      dplyr::transmute(
        feature = feature,
        doc_id = .data$doc_id,
        sentence_id = .data$sentence_id,
        start_token_id_int = .data[[start_col]],
        end_token_id_int = .data[[end_col]],
        keyword_token = .data$token,
        count = 1L,
        match_type = "exact"
      )
  }

  f29 <- tokens_with_context %>%
    dplyr::filter(
      .data$is_relative_subject,
      .data$lemma %in% relative_subject_that_lemmas,
      (
        dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), default = FALSE) |
          (
            dplyr::lag(.data$pos == "ADJ", default = FALSE) &
              dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), 2, default = FALSE)
          )
      )
    ) %>%
    build_rel_rows("f_29_that_subj")

  f30 <- tokens_with_context %>%
    dplyr::filter(
      .data$is_relative_object,
      .data$lemma %in% relative_object_that_lemmas,
      dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), default = FALSE)
    ) %>%
    build_rel_rows("f_30_that_obj")

  f31 <- tokens_with_context %>%
    dplyr::filter(
      .data$is_relative_subject,
      .data$lemma %in% wh_subject_relative_lemmas,
      (
        dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), default = FALSE) |
          (
            dplyr::lag(.data$pos == "PUNCT", default = FALSE) &
              dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), 2, default = FALSE)
          )
      )
    ) %>%
    build_rel_rows("f_31_wh_subj")

  f32 <- tokens_with_context %>%
    dplyr::filter(
      .data$is_relative_object,
      .data$lemma %in% wh_object_relative_lemmas,
      (
        dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), default = FALSE) |
          (
            dplyr::lag(.data$pos == "PUNCT", default = FALSE) &
              dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), 2, default = FALSE)
          )
      )
    ) %>%
    build_rel_rows("f_32_wh_obj")

  f33 <- tokens_with_context %>%
    dplyr::filter(
      .data$is_relative_pronoun,
      .data$lemma %in% pied_piping_relative_lemmas,
      (
        .data$lemma == "dont" |
          dplyr::lag(.data$pos == "ADP", default = FALSE)
      )
    ) %>%
    dplyr::transmute(
      feature = "f_33_pied_piping",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = dplyr::if_else(.data$lemma == "dont", .data$token_id_int, .data$prev_token_id_int),
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  f34 <- tokens_with_context %>%
    dplyr::filter(
      .data$is_relative_pronoun,
      dplyr::lag(.data$token %in% sentence_relative_anchors, default = FALSE),
      (
        dplyr::lag(.data$pos == "PUNCT", 2, default = TRUE) |
          is.na(dplyr::lag(.data$token, 2))
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_34_sentence_relatives",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$prev_token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = paste(.data$prev_token, .data$token),
      count = 1L,
      match_type = "exact"
    )

  dplyr::bind_rows(f29, f30, f31, f32, f33, f34) %>%
    dplyr::filter(.data$feature %in% requested)
}

locate_clause_embedding_feature_matches <- function(tokens, selected_features) {
  requested <- intersect(
    selected_features,
    c(
      "f_21_that_verb_comp", "f_22_that_adj_comp", "f_23_wh_clause",
      "f_24_infinitives", "f_25_present_participle", "f_26_past_participle",
      "f_27_past_participle_whiz", "f_28_present_participle_whiz",
      "f_35_because", "f_36_though", "f_37_if", "f_38_other_adv_sub",
      "f_60_that_deletion"
    )
  )
  if (length(requested) == 0) {
    return(tibble::tibble())
  }

  tokens_with_context <- prepare_exact_locator_context(tokens) %>%
    dplyr::mutate(
      is_infinitive = dplyr::if_else(
        .data$pos %in% c("VERB", "AUX") & !is.na(.data$morph_verbform),
        .data$morph_verbform == "Inf",
        FALSE
      ),
      is_present_participle = dplyr::case_when(
        .data$tag == "VBG" ~ TRUE,
        .data$pos %in% c("VERB", "AUX") & !is.na(.data$morph_verbform) & .data$morph_verbform == "Ger" ~ TRUE,
        .data$pos %in% c("VERB", "AUX") & !is.na(.data$morph_verbform) & .data$morph_verbform == "Part" &
          .data$morph_tense %in% c("Pres", "Imp") ~ TRUE,
        TRUE ~ FALSE
      ),
      is_past_participle = dplyr::case_when(
        .data$tag == "VBN" ~ TRUE,
        .data$pos %in% c("VERB", "AUX") & !is.na(.data$morph_verbform) &
          .data$morph_verbform == "Part" &
          .data$morph_tense %in% c("Past", "Pqp") ~ TRUE,
        .data$pos %in% c("VERB", "ADJ") &
          stringr::str_detect(
            stringr::str_to_lower(.data$token),
            "(\u00e9|\u00e9e|\u00e9s|\u00e9es|i|ie|is|ies|u|ue|us|ues|it|ite|its|ites)$"
          ) &
          stringr::str_detect(
            dplyr::coalesce(.data$dep_rel, ""),
            "^(acl|advcl|xcomp|ccomp|root)"
          ) ~ TRUE,
        TRUE ~ FALSE
      )
    )

  head_lookup <- tokens_with_context %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int",
      head_pos = "pos",
      head_lemma = "lemma",
      head_token = "token"
    )

  complementizers <- c("que", "qu'", "qu\u2019")
  wh_lemmas <- c(
    "qui", "que", "quoi", "dont",
    "\u00f9", "ou", "quand", "comment", "pourquoi", "combien",
    "lequel", "laquelle", "lesquels", "lesquelles",
    "auquel", "auxquels", "auxquelles",
    "duquel", "desquels", "desquelles"
  )
  parce_follow_tokens <- c("que", "qu'", "qu\u2019")
  because_single_tokens <- c("car", "puisque", "puisqu'", "puisqu\u2019", "comme")
  counted_subordinators <- unique(c(
    complementizers,
    parce_follow_tokens,
    because_single_tokens,
    "parce",
    "quoique",
    "si"
  ))

  f21 <- tokens_with_context %>%
    dplyr::filter(
      .data$token %in% complementizers,
      .data$pos == "SCONJ",
      .data$prev_pos %in% c("VERB", "AUX")
    ) %>%
    dplyr::transmute(
      feature = "f_21_that_verb_comp",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  f22 <- tokens_with_context %>%
    dplyr::filter(
      .data$token %in% complementizers,
      .data$pos == "SCONJ",
      .data$prev_pos == "ADJ"
    ) %>%
    dplyr::transmute(
      feature = "f_22_that_adj_comp",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  f23 <- tokens_with_context %>%
    dplyr::filter(
      .data$lemma %in% wh_lemmas,
      .data$pos %in% c("PRON", "ADV", "DET", "ADJ", "NOUN", "PROPN"),
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(obj|obl|nsubj|iobj|expl|mark)"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(.data$head_pos %in% c("VERB", "AUX", "ADJ")) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_23_wh_clause",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  f24 <- tokens_with_context %>%
    dplyr::filter(.data$is_infinitive) %>%
    dplyr::transmute(
      feature = "f_24_infinitives",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = dplyr::if_else(.data$prev_pos == "ADP", .data$prev_token_id_int, .data$token_id_int),
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  f25 <- tokens_with_context %>%
    dplyr::filter(
      .data$is_present_participle,
      .data$dep_rel %in% c("advcl", "ccomp"),
      (
        dplyr::lag(.data$dep_rel == "punct", default = TRUE) |
          (
            dplyr::lag(.data$token %in% c("en"), default = FALSE) &
              dplyr::lag(.data$dep_rel %in% c("mark", "case"), default = FALSE)
          )
      )
    ) %>%
    dplyr::transmute(
      feature = "f_25_present_participle",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = dplyr::if_else(.data$prev_token == "en", .data$prev_token_id_int, .data$token_id_int),
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  f26 <- tokens_with_context %>%
    dplyr::filter(
      .data$is_past_participle,
      (
        .data$dep_rel %in% c("advcl", "ccomp") |
          (
            .data$dep_rel == "acl" &
              dplyr::lag(.data$dep_rel == "punct", default = TRUE)
          )
      ),
      (
        dplyr::lag(.data$dep_rel == "punct", default = TRUE) |
          (
            dplyr::lag(.data$token %in% c("en"), default = FALSE) &
              dplyr::lag(.data$dep_rel %in% c("mark", "case"), default = FALSE)
          )
      )
    ) %>%
    dplyr::transmute(
      feature = "f_26_past_participle",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = dplyr::if_else(.data$prev_token == "en", .data$prev_token_id_int, .data$token_id_int),
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  f27 <- tokens_with_context %>%
    dplyr::filter(
      .data$is_past_participle,
      dplyr::lag(.data$pos == "NOUN"),
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^acl") | .data$dep_rel == "root"
    ) %>%
    dplyr::transmute(
      feature = "f_27_past_participle_whiz",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  f28 <- tokens_with_context %>%
    dplyr::filter(
      .data$is_present_participle,
      dplyr::lag(.data$pos == "NOUN"),
      .data$dep_rel == "acl"
    ) %>%
    dplyr::transmute(
      feature = "f_28_present_participle_whiz",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  f35 <- tokens_with_context %>%
    dplyr::filter(
      (
        .data$token %in% because_single_tokens &
          .data$pos %in% c("SCONJ", "CCONJ") &
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(mark|cc)")
      ) |
        (
          .data$token == "parce" &
            .data$next_token %in% parce_follow_tokens &
            .data$pos %in% c("SCONJ", "ADV")
        )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_35_because",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = dplyr::if_else(.data$token == "parce", .data$next_token_id_int, .data$token_id_int),
      keyword_token = dplyr::if_else(.data$token == "parce", paste(.data$token, .data$next_token), .data$token),
      count = 1L,
      match_type = "exact"
    )

  f36 <- tokens_with_context %>%
    dplyr::filter(
      (.data$token == "quoique" & .data$pos %in% c("SCONJ")) |
        (
          .data$token == "bien" &
            .data$next_token %in% parce_follow_tokens &
            .data$next_pos %in% c("SCONJ")
        ) |
        (
          .data$token == "m\u00eame" &
            .data$next_token == "si" &
            .data$next_pos %in% c("SCONJ")
        )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_36_though",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = dplyr::if_else(.data$token %in% c("bien", "m\u00eame"), .data$next_token_id_int, .data$token_id_int),
      keyword_token = dplyr::if_else(.data$token %in% c("bien", "m\u00eame"), paste(.data$token, .data$next_token), .data$token),
      count = 1L,
      match_type = "exact"
    )

  f37 <- tokens_with_context %>%
    dplyr::filter(
      (
        (.data$token == "si" | .data$token == "s'") &
          .data$pos %in% c("SCONJ") &
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^mark")
      ) |
        (
          .data$token == "moins" &
            .data$prev_token %in% c("\u00e0", "au") &
            .data$next_token %in% parce_follow_tokens
        ) |
        (
          .data$token == "condition" &
            .data$prev_token == "\u00e0" &
            .data$next_token %in% parce_follow_tokens
        )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_37_if",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = dplyr::if_else(.data$token %in% c("moins", "condition"), .data$prev_token_id_int, .data$token_id_int),
      end_token_id_int = dplyr::if_else(.data$token %in% c("moins", "condition"), .data$next_token_id_int, .data$token_id_int),
      keyword_token = dplyr::case_when(
        .data$token == "moins" ~ paste(.data$prev_token, .data$token, .data$next_token),
        .data$token == "condition" ~ paste(.data$prev_token, .data$token, .data$next_token),
        TRUE ~ .data$token
      ),
      count = 1L,
      match_type = "exact"
    )

  f38 <- tokens_with_context %>%
    dplyr::filter(
      .data$pos %in% c("SCONJ", "ADP", "ADV"),
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^mark"),
      !.data$token %in% counted_subordinators
    ) %>%
    dplyr::transmute(
      feature = "f_38_other_adv_sub",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  clause_marks <- tokens_with_context %>%
    dplyr::filter(
      .data$dep_rel == "mark",
      .data$lemma %in% complementizers
    ) %>%
    dplyr::transmute(
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      token_id_int = .data$head_token_id_int,
      has_mark = TRUE
    ) %>%
    dplyr::distinct()

  clause_deletions <- tokens_with_context %>%
    dplyr::filter(.data$dep_rel %in% c("ccomp", "xcomp")) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(.data$head_pos %in% c("VERB", "AUX", "ADJ")) %>%
    dplyr::left_join(
      clause_marks,
      by = c("doc_id", "sentence_id", "token_id_int")
    ) %>%
    dplyr::filter(is.na(.data$has_mark)) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE)

  f60 <- clause_deletions %>%
    dplyr::transmute(
      feature = "f_60_that_deletion",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  dplyr::bind_rows(f21, f22, f23, f24, f25, f26, f27, f28, f35, f36, f37, f38, f60) %>%
    dplyr::filter(.data$feature %in% requested)
}

locate_negation_feature_matches <- function(tokens, selected_features, word_lists_lookup) {
  requested <- intersect(selected_features, c("f_66_neg_synthetic", "f_67_neg_analytic"))
  if (length(requested) == 0) {
    return(tibble::tibble())
  }

  neg_synthetic_terms <- normalize_terms(get_word_list(word_lists_lookup, "neg_synthetic_determiners"))
  negation_particle_terms <- normalize_terms(get_word_list(word_lists_lookup, "negation_particles"))
  negation_part_lemmas <- unique(c(negation_particle_terms, "n'", "n\u2019"))
  negation_adverbs <- normalize_terms(get_word_list(word_lists_lookup, "neg_analytic_adverbs"))

  f66 <- tokens %>%
    dplyr::filter(.data$lemma %in% neg_synthetic_terms) %>%
    dplyr::transmute(
      feature = "f_66_neg_synthetic",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  negation_particles_df <- tokens %>%
    dplyr::filter(
      .data$lemma %in% negation_part_lemmas,
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id,
      .data$sentence_id,
      head_token_id_int = .data$head_token_id_int,
      neg_particle_token_id_int = .data$token_id_int,
      neg_particle_token = .data$token,
      has_ne = TRUE
    ) %>%
    dplyr::distinct()

  f67 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% negation_adverbs,
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      negation_particles_df,
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::filter(.data$has_ne) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_67_neg_analytic",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = pmin(.data$neg_particle_token_id_int, .data$token_id_int),
      end_token_id_int = pmax(.data$neg_particle_token_id_int, .data$token_id_int),
      keyword_token = paste(.data$neg_particle_token, .data$token),
      count = 1L,
      match_type = "exact"
    )

  dplyr::bind_rows(f66, f67) %>%
    dplyr::filter(.data$feature %in% requested)
}

locate_coordination_split_feature_matches <- function(tokens, selected_features, word_lists_lookup) {
  requested <- intersect(
    selected_features,
    c(
      "f_61_stranded_preposition",
      "f_62_split_infinitive",
      "f_63_split_auxiliary",
      "f_64_phrasal_coordination",
      "f_65_clausal_coordination"
    )
  )
  if (length(requested) == 0) {
    return(tibble::tibble())
  }

  tokens_ctx <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::arrange(.data$sentence_id, .data$token_id_int, .by_group = TRUE) %>%
    dplyr::mutate(
      lag1_pos = dplyr::lag(.data$pos),
      lag1_lemma = dplyr::lag(.data$lemma),
      lag1_sent = dplyr::lag(.data$sentence_id),
      lag1_token_id_int = dplyr::lag(.data$token_id_int),
      lag2_pos = dplyr::lag(.data$pos, 2),
      lag2_lemma = dplyr::lag(.data$lemma, 2),
      lag2_sent = dplyr::lag(.data$sentence_id, 2),
      lag2_token_id_int = dplyr::lag(.data$token_id_int, 2),
      lag3_pos = dplyr::lag(.data$pos, 3),
      lag3_lemma = dplyr::lag(.data$lemma, 3),
      lag3_sent = dplyr::lag(.data$sentence_id, 3),
      lag3_token_id_int = dplyr::lag(.data$token_id_int, 3),
      lag4_pos = dplyr::lag(.data$pos, 4),
      lag4_lemma = dplyr::lag(.data$lemma, 4),
      lag4_sent = dplyr::lag(.data$sentence_id, 4),
      lag4_token_id_int = dplyr::lag(.data$token_id_int, 4),
      lead1_pos = dplyr::lead(.data$pos),
      lead1_lemma = dplyr::lead(.data$lemma),
      lead1_sent = dplyr::lead(.data$sentence_id),
      lead1_prontype = dplyr::lead(.data$morph_prontype)
    ) %>%
    dplyr::ungroup()

  stranded_pronouns <- c("qui", "quoi")
  inf_prepositions <- c("\u00e0", "a", "au", "aux", "de", "d'", "d\u2019", "du", "des", "pour")
  filler_pos <- c("ADV", "PART", "PRON", "DET")
  negation_particle_terms <- normalize_terms(get_word_list(word_lists_lookup, "negation_particles"))
  negation_part_lemmas <- unique(c(negation_particle_terms, "n'", "n\u2019"))

  f61 <- tokens_ctx %>%
    dplyr::filter(
      .data$pos == "ADP",
      !is.na(.data$lead1_sent),
      .data$lead1_sent == .data$sentence_id,
      .data$lead1_pos == "PRON",
      .data$lead1_lemma %in% stranded_pronouns,
      stringr::str_detect(dplyr::coalesce(.data$lead1_prontype, ""), "Rel|Int")
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_61_stranded_preposition",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$token_id_int,
      end_token_id_int = .data$token_id_int + 1L,
      keyword_token = paste(.data$token, .data$lead1_lemma),
      count = 1L,
      match_type = "exact"
    )

  candidate_inf <- tokens_ctx %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      !is.na(.data$morph_verbform),
      .data$morph_verbform == "Inf"
    ) %>%
    dplyr::mutate(
      lag1_same = !is.na(.data$lag1_sent) & .data$lag1_sent == .data$sentence_id,
      lag2_same = !is.na(.data$lag2_sent) & .data$lag2_sent == .data$sentence_id,
      lag3_same = !is.na(.data$lag3_sent) & .data$lag3_sent == .data$sentence_id,
      lag4_same = !is.na(.data$lag4_sent) & .data$lag4_sent == .data$sentence_id,
      filler1_ok = .data$lag1_same & .data$lag1_pos %in% filler_pos,
      filler2_ok = .data$lag2_same & .data$lag2_pos %in% filler_pos,
      filler3_ok = .data$lag3_same & .data$lag3_pos %in% filler_pos,
      adv12 = (.data$lag1_same & .data$lag1_pos == "ADV") | (.data$lag2_same & .data$lag2_pos == "ADV"),
      adv123 = (
        (.data$lag1_same & .data$lag1_pos == "ADV") |
          (.data$lag2_same & .data$lag2_pos == "ADV") |
          (.data$lag3_same & .data$lag3_pos == "ADV")
      ),
      has_split2 = .data$lag2_same &
        .data$lag2_pos == "ADP" &
        .data$lag2_lemma %in% inf_prepositions &
        .data$lag1_same &
        .data$lag1_pos == "ADV",
      has_split3 = .data$lag3_same &
        .data$lag3_pos == "ADP" &
        .data$lag3_lemma %in% inf_prepositions &
        .data$filler1_ok &
        .data$filler2_ok &
        .data$adv12,
      has_split4 = .data$lag4_same &
        .data$lag4_pos == "ADP" &
        .data$lag4_lemma %in% inf_prepositions &
        .data$filler1_ok &
        .data$filler2_ok &
        .data$filler3_ok &
        .data$adv123
    ) %>%
    dplyr::filter(.data$has_split2 | .data$has_split3 | .data$has_split4)

  f62 <- candidate_inf %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_62_split_infinitive",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = dplyr::case_when(
        .data$has_split2 ~ .data$lag2_token_id_int,
        .data$has_split3 ~ .data$lag3_token_id_int,
        .data$has_split4 ~ .data$lag4_token_id_int,
        TRUE ~ .data$token_id_int
      ),
      end_token_id_int = .data$token_id_int,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  adverbial_interveners <- tokens %>%
    dplyr::filter(
      .data$pos == "ADV" |
        (
          .data$pos == "PART" &
            .data$lemma %in% negation_part_lemmas
        )
    ) %>%
    dplyr::transmute(
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      adv_token_id_int = .data$token_id_int
    )

  head_lookup <- tokens %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int",
      head_pos = "pos",
      head_lemma = "lemma",
      head_token = "token",
      head_morph_verbform = "morph_verbform"
    )

  aux_dependencies <- tokens %>%
    dplyr::filter(stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux")) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(
      .data$head_pos %in% c("VERB", "AUX"),
      !is.na(.data$token_id_int),
      !is.na(.data$head_token_id_int),
      .data$token_id_int != .data$head_token_id_int
    ) %>%
    dplyr::mutate(
      span_min = pmin(.data$token_id_int, .data$head_token_id_int),
      span_max = pmax(.data$token_id_int, .data$head_token_id_int)
    )

  french_compound_verbs <- tokens %>%
    dplyr::filter(
      .data$lemma %in% c("avoir", "\u00eatre"),
      .data$pos %in% c("VERB", "AUX")
    ) %>%
    dplyr::left_join(
      tokens %>%
        dplyr::filter(
          .data$dep_rel %in% c("xcomp", "ccomp"),
          stringr::str_detect(dplyr::coalesce(.data$morph_verbform, ""), "Part")
        ) %>%
        dplyr::transmute(
          .data$doc_id,
          .data$sentence_id,
          head_token_id_int = .data$head_token_id_int,
          participle_token_id_int = .data$token_id_int
        ),
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    ) %>%
    dplyr::filter(!is.na(.data$participle_token_id_int)) %>%
    dplyr::mutate(
      span_min = pmin(.data$token_id_int, .data$participle_token_id_int),
      span_max = pmax(.data$token_id_int, .data$participle_token_id_int),
      head_token_id_int = .data$participle_token_id_int
    )

  all_aux_dependencies <- dplyr::bind_rows(aux_dependencies, french_compound_verbs)

  f63 <- all_aux_dependencies %>%
    dplyr::left_join(
      adverbial_interveners,
      by = c("doc_id", "sentence_id"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(
      !is.na(.data$adv_token_id_int),
      .data$adv_token_id_int > .data$span_min,
      .data$adv_token_id_int < .data$span_max
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .data$head_token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_63_split_auxiliary",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = .data$span_min,
      end_token_id_int = .data$span_max,
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  token_lookup <- tokens %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int",
      token_pos = "pos",
      token_dep_rel = "dep_rel",
      token_head_token_id_int = "head_token_id_int",
      token_morph_verbform = "morph_verbform"
    )

  subject_pron_lemmas <- c("je", "tu", "il", "elle", "on", "nous", "vous", "ils", "elles", "lui", "leur")

  subject_heads <- tokens %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^nsubj") |
        (
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(obj|iobj)") &
            .data$pos == "PRON" &
            .data$lemma %in% subject_pron_lemmas
        )
    ) %>%
    dplyr::distinct(
      .data$doc_id,
      .data$sentence_id,
      clause_head_token_id_int = .data$head_token_id_int
    ) %>%
    dplyr::mutate(has_subject = TRUE)

  cc_tokens <- tokens %>%
    dplyr::filter(.data$pos == "CCONJ", .data$dep_rel == "cc") %>%
    dplyr::left_join(
      token_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::rename(
      conj_pos = "token_pos",
      conj_dep_rel = "token_dep_rel",
      conj_head_token_id_int = "token_head_token_id_int",
      conj_morph_verbform = "token_morph_verbform"
    ) %>%
    dplyr::left_join(
      token_lookup %>%
        dplyr::select(
          "doc_id", "sentence_id", "token_id_int",
          first_conj_pos = "token_pos"
        ),
      by = c("doc_id", "sentence_id", "conj_head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::left_join(
      subject_heads,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "clause_head_token_id_int")
    ) %>%
    dplyr::mutate(has_subject = dplyr::coalesce(.data$has_subject, FALSE)) %>%
    dplyr::select(-dplyr::any_of("clause_head_token_id_int"))

  f64 <- cc_tokens %>%
    dplyr::filter(
      .data$conj_dep_rel == "conj",
      .data$conj_pos %in% c("NOUN", "PROPN", "ADJ", "ADV"),
      !is.na(.data$first_conj_pos),
      .data$first_conj_pos == .data$conj_pos,
      !.data$has_subject
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_64_phrasal_coordination",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = pmin(.data$conj_head_token_id_int, .data$head_token_id_int),
      end_token_id_int = pmax(.data$conj_head_token_id_int, .data$head_token_id_int),
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  f65 <- cc_tokens %>%
    dplyr::filter(
      .data$conj_dep_rel == "conj",
      .data$conj_pos %in% c("VERB", "AUX"),
      .data$has_subject,
      is.na(.data$conj_morph_verbform) | !.data$conj_morph_verbform %in% c("Inf", "Ger", "Part")
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::transmute(
      feature = "f_65_clausal_coordination",
      doc_id = .data$doc_id,
      sentence_id = .data$sentence_id,
      start_token_id_int = pmin(.data$conj_head_token_id_int, .data$head_token_id_int),
      end_token_id_int = pmax(.data$conj_head_token_id_int, .data$head_token_id_int),
      keyword_token = .data$token,
      count = 1L,
      match_type = "exact"
    )

  dplyr::bind_rows(f61, f62, f63, f64, f65) %>%
    dplyr::filter(.data$feature %in% requested)
}

locate_exact_audit_matches <- function(tokens, selected_features, word_lists_lookup, dict_lookup, engine) {
  matches <- dplyr::bind_rows(
    locate_nominal_feature_matches(tokens, selected_features, word_lists_lookup),
    locate_stative_feature_matches(tokens, selected_features),
    locate_clause_embedding_feature_matches(tokens, selected_features),
    locate_negation_feature_matches(tokens, selected_features, word_lists_lookup),
    locate_coordination_split_feature_matches(tokens, selected_features, word_lists_lookup),
    locate_lexical_class_matches(tokens, selected_features, dict_lookup),
    locate_passive_feature_matches(tokens, selected_features, engine),
    locate_modal_feature_matches(tokens, selected_features, dict_lookup),
    locate_relative_feature_matches(tokens, selected_features)
  )

  if (ncol(matches) == 0) {
    return(tibble::tibble(
      feature = character(),
      doc_id = character(),
      sentence_id = integer(),
      start_token_id_int = integer(),
      end_token_id_int = integer(),
      keyword_token = character(),
      count = integer(),
      match_type = character()
    ))
  }

  matches
}

prepare_audit_display_tokens <- function(tokens) {
  tokens <- dplyr::as_tibble(tokens)

  if (!"sentence_id" %in% colnames(tokens)) {
    tokens <- dplyr::mutate(tokens, sentence_id = 1L)
  }

  tokens %>%
    dplyr::mutate(
      token_id_int = suppressWarnings(as.integer(.data$token_id))
    ) %>%
    dplyr::arrange(.data$doc_id, .data$sentence_id, .data$token_id_int)
}

attach_kwic_columns <- function(rows, tokens, window) {
  if (nrow(rows) == 0) {
    return(rows %>%
      dplyr::mutate(
        left = character(),
        keyword = character(),
        right = character(),
        chunk = character()
      ))
  }

  sentence_chunks <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::summarise(chunk = paste(.data$token, collapse = " "), .groups = "drop")

  kwic_rows <- purrr::pmap_dfr(
    list(rows$doc_id, rows$sentence_id, rows$start_token_id_int, rows$end_token_id_int),
    function(current_doc_id, current_sentence_id, start_token_id_int, end_token_id_int) {
      sentence_tokens <- tokens %>%
        dplyr::filter(
          .data$doc_id == current_doc_id,
          .data$sentence_id == current_sentence_id
        ) %>%
        dplyr::arrange(.data$token_id_int)

      if (!is.finite(start_token_id_int) || !is.finite(end_token_id_int)) {
        return(tibble::tibble(left = NA_character_, keyword = NA_character_, right = NA_character_))
      }

      token_ids <- sentence_tokens$token_id_int
      anchor_positions <- which(
        !is.na(token_ids) &
          token_ids >= start_token_id_int &
          token_ids <= end_token_id_int
      )

      if (length(anchor_positions) == 0) {
        return(tibble::tibble(left = NA_character_, keyword = NA_character_, right = NA_character_))
      }

      anchor_start <- min(anchor_positions)
      anchor_end <- max(anchor_positions)

      left_start <- max(1L, anchor_start - window)
      left_end <- anchor_start - 1L
      right_start <- anchor_end + 1L
      right_end <- min(nrow(sentence_tokens), anchor_end + window)

      left_idx <- if (left_end >= left_start) seq.int(left_start, left_end) else integer()
      right_idx <- if (right_end >= right_start) seq.int(right_start, right_end) else integer()

      tibble::tibble(
        left = paste(sentence_tokens$token[left_idx], collapse = " "),
        keyword = paste(sentence_tokens$token[anchor_positions], collapse = " "),
        right = paste(sentence_tokens$token[right_idx], collapse = " ")
      )
    }
  )

  dplyr::bind_cols(rows, kwic_rows) %>%
    dplyr::left_join(sentence_chunks, by = c("doc_id", "sentence_id"))
}

audit_features_from_spacy_tokens <- function(tokens,
                                             feature = NULL,
                                             category = NULL,
                                             sample_n = NULL,
                                             seed = NULL,
                                             max_per_doc = NULL,
                                             window = 5,
                                             engine = c("spacy", "udpipe")) {
  validate_audit_sampling(sample_n = sample_n, max_per_doc = max_per_doc, seed = seed)
  window <- normalize_window(window)
  engine <- match.arg(engine)
  selected_features <- resolve_audit_feature_set(feature = feature, category = category)
  word_lists_lookup <- word_lists
  dict_lookup <- dict
  display_tokens <- prepare_audit_display_tokens(tokens)

  tokens <- prepare_parsed_tokens(tokens)

  exact_rows <- locate_exact_audit_matches(tokens, selected_features, word_lists_lookup, dict_lookup, engine)
  exact_features <- unique(exact_rows$feature)
  fallback_features <- setdiff(selected_features, exact_features)

  sentence_index <- build_sentence_audit_index(display_tokens)

  sentence_tokens <- tokens %>%
    dplyr::mutate(
      sentence_id = dplyr::coalesce(.data$sentence_id, 1L),
      doc_id = paste(.data$doc_id, .data$sentence_id, sep = "::"),
      sentence_id = 1L
    )

  sentence_tokens <- structure(sentence_tokens, class = c("spacyr_parsed", "data.frame"))

  sentence_counts <- biber(sentence_tokens, measure = "none", normalize = FALSE)
  fallback_rows <- counts_to_audit_rows(sentence_counts, fallback_features)
  if (nrow(fallback_rows) > 0) {
    fallback_rows <- fallback_rows %>%
      dplyr::left_join(sentence_index, by = "audit_doc_id") %>%
      dplyr::mutate(
        category = feature_to_category_code(.data$feature),
        match_type = "sentence",
        left = NA_character_,
        keyword = NA_character_,
        right = NA_character_
      ) %>%
      dplyr::select(
        "feature", "category", "doc_id", "sentence_id", "count",
        "token_count", "match_type", "left", "keyword", "right", "chunk"
      )
  } else {
    fallback_rows <- tibble::tibble(
      feature = character(),
      category = character(),
      doc_id = character(),
      sentence_id = integer(),
      count = integer(),
      token_count = integer(),
      match_type = character(),
      left = character(),
      keyword = character(),
      right = character(),
      chunk = character()
    )
  }

  exact_joined <- exact_rows %>%
    attach_kwic_columns(display_tokens, window = window) %>%
    dplyr::mutate(
      category = feature_to_category_code(.data$feature),
      token_count = NA_integer_
    ) %>%
    dplyr::select(
      "feature", "category", "doc_id", "sentence_id", "count",
      "token_count", "match_type", "left", "keyword", "right", "chunk"
    )

  joined <- dplyr::bind_rows(exact_joined, fallback_rows)

  sampled <- sample_audit_rows(joined, sample_n = sample_n, max_per_doc = max_per_doc, seed = seed)

  new_audit_features_result(sampled)
}

#' Print audit results compactly
#'
#' @param x An `audit_features_result` object.
#' @param n Maximum number of rows to print.
#' @param ... Unused.
#' @export
print.audit_features_result <- function(x, n = 10, ...) {
  total_rows <- nrow(x)
  if ("match_type" %in% colnames(x)) {
    exact_rows <- sum(x$match_type == "exact", na.rm = TRUE)
    sentence_rows <- sum(x$match_type == "sentence", na.rm = TRUE)
    summary_suffix <- paste0(" (", exact_rows, " exact, ", sentence_rows, " sentence)")
  } else {
    summary_suffix <- ""
  }

  cat(
    "audit_features_result:", total_rows, "rows",
    summary_suffix,
    "\n"
  )

  preview_n <- min(total_rows, n)
  preview <- tibble::as_tibble(unclass(x))[seq_len(preview_n), , drop = FALSE]
  print(preview)

  if (total_rows > preview_n) {
    cat("...", total_rows - preview_n, "more rows\n")
  }

  invisible(x)
}

#' @export
audit_features.spacyr_parsed <- function(tokens,
                                         feature = NULL,
                                         category = NULL,
                                         sample_n = NULL,
                                         seed = NULL,
                                         max_per_doc = NULL,
                                         window = 5) {
  audit_features_from_spacy_tokens(
    tokens = tokens,
    feature = feature,
    category = category,
    sample_n = sample_n,
    seed = seed,
    max_per_doc = max_per_doc,
    window = window,
    engine = "spacy"
  )
}

#' @export
audit_features.udpipe_connlu <- function(tokens,
                                         feature = NULL,
                                         category = NULL,
                                         sample_n = NULL,
                                         seed = NULL,
                                         max_per_doc = NULL,
                                         window = 5) {
  udpipe_tks <- coerce_udpipe_to_spacy_tokens(tokens)

  audit_features_from_spacy_tokens(
    tokens = udpipe_tks,
    feature = feature,
    category = category,
    sample_n = sample_n,
    seed = seed,
    max_per_doc = max_per_doc,
    window = window,
    engine = "udpipe"
  )
}
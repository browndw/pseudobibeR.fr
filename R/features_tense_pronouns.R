# Tense, aspect, pronoun and question features

#' Past and present tense (French)
#'
#' Finite verbs and auxiliaries in any mood (D-33): f_01 with Tense
#' Past/Imp/Pqp, f_03 with Tense Pres. The conditional and imperative are not
#' present tense (UDPipe French marks the conditional `Tense=Pres|Mood=Cnd`);
#' conditionals are counted in f_54. Verbs without a VerbForm annotation are
#' accepted.
#'
#' @param tokens Prepared tokens.
#' @param doc_ids Document IDs.
#' @return Block result with f_01 and f_03 evidence.
#' @keywords internal
block_tense_fr <- function(tokens, doc_ids) {
  finite <- tokens$pos %in% c("VERB", "AUX") &
    (is.na(tokens$morph_verbform) | tokens$morph_verbform == "Fin")
  f01 <- tokens[finite & tokens$morph_tense %in% c("Past", "Imp", "Pqp"), ] %>%
    count_feature_traced("f_01_past_tense")
  f03 <- tokens[finite & tokens$morph_tense %in% "Pres" &
                  !tokens$morph_mood %in% c("Cnd", "Imp"), ] %>%
    count_feature_traced("f_03_present_tense")
  make_block_result(doc_ids, bind_evidence(f01$evidence, f03$evidence))
}

#' Perfect aspect and pro-verb faire (French)
#'
#' f_02: one occurrence per participle governed by a tense auxiliary
#' (avoir/être), anchored on the auxiliary. Passive auxiliaries (`aux:pass`)
#' are excluded. The participle must carry `VerbForm=Part`.
#' f_12: lexical faire with an object pronoun from `proverb_object_pronouns`
#' (`je le fais`), excluding causative (`aux:caus`) and reflexive (`se faire`)
#' uses, parallel to Spanish D-22.
#'
#' @param tokens Prepared tokens.
#' @param doc_ids Document IDs.
#' @param head_lookup Head token attributes.
#' @param proverb_pronouns Object pronoun lemmas for f_12.
#' @return Block result with f_02 and f_12 evidence.
#' @keywords internal
block_aux_tense_fr <- function(tokens, doc_ids, head_lookup, proverb_pronouns) {
  f02 <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("AUX", "VERB"),
      .data$lemma %in% c("avoir", "\u00eatre"),
      stringr::str_detect(.data$dep_rel, "^(aux|cop)"),
      !stringr::str_detect(.data$dep_rel, "pass"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(head_lookup,
                     by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")) %>%
    dplyr::filter(
      .data$head_pos %in% c("VERB", "AUX", "ADJ", "NOUN"),
      dplyr::coalesce(.data$head_morph_verbform, "") == "Part",
      dplyr::coalesce(.data$head_morph_voice, "") != "Pass"
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$head_token_id_int, .keep_all = TRUE) %>%
    count_feature_traced("f_02_perfect_aspect")

  proverb_objects <- tokens %>%
    dplyr::filter(
      .data$pos == "PRON",
      .data$match_lemma %in% proverb_pronouns,
      stringr::str_detect(.data$dep_rel, "^obj")
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int)
  reflexive_heads <- tokens %>%
    dplyr::filter(.data$match_token %in% c("se", "s'"), .data$pos == "PRON") %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int)
  f12 <- tokens %>%
    dplyr::filter(
      .data$lemma == "faire",
      .data$pos %in% c("VERB", "AUX"),
      !stringr::str_detect(.data$dep_rel, "^aux")
    ) %>%
    dplyr::semi_join(proverb_objects, by = c("doc_id", "sentence_id", "token_id_int")) %>%
    dplyr::anti_join(reflexive_heads, by = c("doc_id", "sentence_id", "token_id_int")) %>%
    count_feature_traced("f_12_proverb_do")
  make_block_result(doc_ids, bind_evidence(f02$evidence, f12$evidence))
}

#' Personal pronouns, pronoun it and wh-questions (French)
#'
#' f_06-f_08: union of dictionary forms and personal pronouns
#' (`PronType=Prs`) with the matching `Person` value, one occurrence per word.
#' Demonstrative ce/ça (f_10), partitive en and generic on are not personal
#' pronouns (D-50). Pronouns counted as f_09
#' (impersonal/expletive) are excluded from f_08.
#' f_09: impersonal/expletive subject pronouns (il, ce/c', cela, ça; matched
#' on the surface form, so elle never qualifies): any `expl` relation, or a
#' subject of a weather verb (directly or through an infinitival complement,
#' `il pourrait pleuvoir`), of faire with a weather complement (`il fait
#' froid`) or of falloir, or of an adjective, raising verb or
#' listed impersonal verb that has its own clausal complement (a que-clause,
#' or an infinitive introduced by de/à: `il semble que`, `il est facile de`).
#' f_13: interrogative words in a sentence containing a question mark,
#' excluding words inside a relative clause (`acl`).
#'
#' @param tokens Prepared tokens.
#' @param doc_ids Document IDs.
#' @param head_lookup Head token attributes.
#' @param dict_lookup Dictionary.
#' @param clause_complements Heads with a ccomp/csubj/xcomp dependent.
#' @param weather_lemmas,weather_faire_terms,raising_verbs,impersonal_clause_verbs,wh_question_lemmas
#'   Lemma inventories.
#' @return Block result with f_06-f_09 and f_13 evidence.
#' @keywords internal
block_personal_pronouns_fr <- function(tokens, doc_ids, head_lookup, dict_lookup,
                                       clause_complements, weather_lemmas, weather_faire_terms,
                                       raising_verbs, impersonal_clause_verbs,
                                       wh_question_lemmas) {
  weather_faire <- tokens %>%
    dplyr::filter(.data$match_token %in% weather_faire_terms,
                  stringr::str_detect(.data$dep_rel, "^(obj|xcomp|amod|advmod)")) %>%
    dplyr::semi_join(tokens[tokens$lemma == "faire", ],
                     by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$head_token_id_int) %>%
    dplyr::mutate(is_weather_faire = TRUE)
  weather_complement_heads <- tokens %>%
    dplyr::filter(.data$dep_rel == "xcomp", .data$lemma %in% weather_lemmas) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$head_token_id_int) %>%
    dplyr::mutate(has_weather_complement = TRUE)
  f09_rows <- tokens %>%
    dplyr::filter(
      .data$pos == "PRON",
      .data$match_token %in% c("il", "ce", "c'", "cela", "\u00e7a"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(head_lookup,
                     by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")) %>%
    dplyr::left_join(clause_complements, by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::left_join(weather_complement_heads,
                     by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::left_join(weather_faire, by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::mutate(has_clause_comp = dplyr::coalesce(.data$has_clause_comp, FALSE),
                  has_weather_complement = dplyr::coalesce(.data$has_weather_complement, FALSE)) %>%
    dplyr::filter(
      stringr::str_detect(.data$dep_rel, "^expl") |
        (
          stringr::str_detect(.data$dep_rel, "^nsubj") &
            (
              .data$head_lemma %in% c(weather_lemmas, "falloir") |
                dplyr::coalesce(.data$is_weather_faire, FALSE) |
                .data$has_weather_complement |
                (.data$head_pos == "ADJ" & .data$has_clause_comp) |
                (.data$head_pos %in% c("VERB", "AUX") &
                   .data$head_lemma %in% c(raising_verbs, impersonal_clause_verbs) &
                   .data$has_clause_comp)
            )
        )
    )
  f09 <- count_feature_traced(f09_rows, "f_09_pronoun_it")

  person_rows <- function(feature, person) {
    hits <- surface_matches(tokens, dict_lookup[[feature]])
    dictionary_rows <- tokens[unlist(lapply(seq_len(nrow(hits)), function(i) {
      seq.int(hits$start_row[i], hits$end_row[i])
    })), ]
    dictionary_rows <- dictionary_rows[dictionary_rows$pos != "PUNCT", ]
    # Personal pronouns only: UDPipe also marks demonstrative ce/ça, partitive
    # en and generic on as Person=3 (D-50).
    morph_rows <- tokens[tokens$pos == "PRON" & tokens$morph_person %in% person &
                           stringr::str_detect(dplyr::coalesce(tokens$morph_prontype, ""), "Prs"), ]
    dplyr::bind_rows(dictionary_rows, morph_rows) %>%
      dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE)
  }
  f06 <- count_feature_traced(person_rows("f_06_first_person_pronouns", "1"),
                              "f_06_first_person_pronouns")
  f07 <- count_feature_traced(person_rows("f_07_second_person_pronouns", "2"),
                              "f_07_second_person_pronouns")
  f08 <- person_rows("f_08_third_person_pronouns", "3") %>%
    dplyr::anti_join(f09_rows, by = c("doc_id", "sentence_id", "token_id_int")) %>%
    count_feature_traced("f_08_third_person_pronouns")

  question_sentences <- tokens %>%
    dplyr::filter(.data$token == "?") %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id)
  relative_clause_heads <- tokens %>%
    dplyr::filter(stringr::str_detect(.data$dep_rel, "^acl")) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, head_token_id_int = .data$token_id_int)
  f13 <- tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_question_lemmas,
      .data$pos %in% c("ADV", "PRON", "DET", "ADJ")
    ) %>%
    dplyr::anti_join(relative_clause_heads, by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::semi_join(question_sentences, by = c("doc_id", "sentence_id")) %>%
    count_feature_traced("f_13_wh_question")

  make_block_result(doc_ids, bind_evidence(f06$evidence, f07$evidence, f08$evidence,
                                           f09$evidence, f13$evidence))
}

#' Keep pronoun uses of indefinite pronouns (French f_11)
#'
#' Parallel to the Spanish pronoun-only rule (D-52). Multiword matches
#' (`quelque chose`, `tout le monde`, `n'importe qui`) are kept. Forms that are
#' only ever pronouns (quelqu'un, chacun, quiconque, autrui, quelques-uns) are
#' kept whatever their tag. `personne` counts as PRON, or as a determinerless
#' NOUN in a sentence with ne (`Je ne vois personne`, as for f_66); `rien`
#' unless it has an article (`un rien`); `tout`, `aucun` and `nul` only as PRON
#' outside a determiner relation (`Tout est pret`, `Aucun n'est venu`, not
#' `tout petit`, `aucun livre`), plus `tout` tagged ADV on a verb (`il a tout
#' mange`), except before en/a.
#'
#' @param evidence Combined evidence.
#' @param tokens Prepared tokens.
#' @return `evidence` without non-pronominal f_11 rows.
#' @keywords internal
gate_indefinite_pronouns_fr <- function(evidence, tokens) {
  f11 <- evidence$feature == "f_11_indefinite_pronouns" & !grepl(" ", evidence$token, fixed = TRUE)
  if (!any(f11)) return(evidence)
  idx <- match(paste(evidence$doc_id, evidence$sentence_id, evidence$token_id)[f11],
               paste(tokens$doc_id, tokens$sentence_id, tokens$token_id_int))
  form <- normalize_terms(tokens$token[idx])
  pos <- tokens$pos[idx]
  det_rel <- stringr::str_detect(dplyr::coalesce(tokens$dep_rel[idx], ""), "^det($|:)")
  child_key <- function(rows) {
    paste(rows$doc_id, rows$sentence_id, rows$head_token_id_int)
  }
  token_key <- paste(tokens$doc_id[idx], tokens$sentence_id[idx], tokens$token_id_int[idx])
  determined <- token_key %in% child_key(tokens[stringr::str_detect(tokens$dep_rel, "^det($|:)"), ])
  ne_sentences <- unique(paste(tokens$doc_id, tokens$sentence_id)[
    tokens$match_token %in% c("ne", "n'")])
  in_ne <- paste(tokens$doc_id[idx], tokens$sentence_id[idx]) %in% ne_sentences
  always <- c("quelqu'un", "quelqu'une", "quelques-uns", "quelques-unes", "chacun", "chacune",
              "quiconque", "autrui", "quelquechose")
  head_pos <- tokens$pos[match(
    paste(tokens$doc_id[idx], tokens$sentence_id[idx], tokens$head_token_id_int[idx]),
    paste(tokens$doc_id, tokens$sentence_id, tokens$token_id_int)
  )]
  # UDPipe also tags pronoun tout as ADV advmod of the verb (il a tout mange);
  # degree tout modifies adjectives/adverbs (tout petit), and tout en/tout a
  # are other constructions.
  tout_object <- form == "tout" & pos == "ADV" & head_pos %in% c("VERB", "AUX") &
    !normalize_terms(dplyr::coalesce(tokens$next_token[idx], "")) %in% c("en", "\u00e0", "a")
  keep <- form %in% always | tout_object |
    (form == "personne" & (pos == "PRON" | (pos == "NOUN" & !determined & in_ne))) |
    (form == "rien" & !determined) |
    (form %in% c("tout", "aucun", "aucune", "nul", "nulle") & pos == "PRON" & !det_rel)
  drop <- which(f11)[!keep]
  if (length(drop)) evidence[-drop, ] else evidence
}

# Subordination and clause-embedding features

#' Add verb-form and relative-pronoun flags
#'
#' @param tokens Prepared tokens.
#' @param relative_pronoun_candidates Relative pronoun lemmas.
#' @return `tokens` with `is_infinitive`, `is_present_participle`,
#'   `is_past_participle`, `is_relative_pronoun`, `is_relative_subject` and
#'   `is_relative_object` columns.
#' @keywords internal
add_clause_flags <- function(tokens, relative_pronoun_candidates) {
  verbal <- tokens$pos %in% c("VERB", "AUX")
  vf <- dplyr::coalesce(tokens$morph_verbform, "")
  tense <- dplyr::coalesce(tokens$morph_tense, "")
  dep <- tokens$dep_rel
  lequel <- c("lequel", "laquelle", "lesquel", "lesquelle", "lesquels", "lesquelles",
              "auquel", "auxquels", "auxquelles", "duquel", "desquels", "desquelles")
  tokens$is_infinitive <- verbal & vf == "Inf"
  tokens$is_present_participle <- tokens$tag == "VBG" |
    (verbal & (vf == "Ger" | (vf == "Part" & tense %in% c("Pres", "Imp"))))
  tokens$is_past_participle <- tokens$tag == "VBN" |
    (tokens$pos %in% c("VERB", "AUX", "ADJ") & vf == "Part" &
       tense %in% c("Past", "Pqp", ""))
  tokens$is_past_participle <- tokens$is_past_participle & !tokens$is_present_participle
  tokens$is_relative_pronoun <-
    stringr::str_detect(dplyr::coalesce(tokens$morph_prontype, ""), "Rel") |
    (tokens$lemma %in% relative_pronoun_candidates &
       stringr::str_detect(dep, "^(nsubj|obj|obl|iobj|expl|mark|acl)"))
  tokens$is_relative_subject <- tokens$is_relative_pronoun & stringr::str_detect(dep, "nsubj")
  tokens$is_relative_object <- tokens$is_relative_pronoun & (
    stringr::str_detect(dep, "obj") |
      (stringr::str_detect(dep, "^obl") & tokens$lemma %in% c(lequel, "dont") &
         dplyr::coalesce(tokens$prev_pos, "") != "ADP") |
      (tokens$lemma == "que" & stringr::str_detect(dep, "^mark"))
  )
  tokens
}

#' Infinitive and participial clause features (French)
#'
#' f_24 verbal infinitives, excluding nominal infinitives (f_15). f_25/f_26 present/past participial adverbial
#' clauses that are clause-initial, follow punctuation, or follow `en`.
#' f_27/f_28 past/present participles in a plain `acl` relation to a noun,
#' without their own auxiliary, copula or subject, and not set off by
#' punctuation; comma-delimited participial clauses remain f_26.
#'
#' @param tokens Prepared tokens with context and clause flags.
#' @param doc_ids Document IDs.
#' @param head_lookup Head token attributes.
#' @param nominal_infinitives Rows counted in f_15.
#' @param gerondif_nouns Gérondifs mistagged as nouns (`gerondif_noun_rows_fr()`),
#'   added to f_25.
#' @return Block result with f_24-f_28 evidence.
#' @keywords internal
block_participial_clauses_fr <- function(tokens, doc_ids, head_lookup,
                                         nominal_infinitives = tokens[0, ],
                                         gerondif_nouns = tokens[0, ]) {
  initial_or_punct <- is.na(tokens$prev_pos) | tokens$prev_dep_rel %in% "punct" |
    tokens$prev_pos %in% "PUNCT"
  after_en <- tokens$prev_token %in% "en" & tokens$prev_dep_rel %in% c("mark", "case")
  nominal_head <- tokens %>%
    dplyr::left_join(head_lookup,
                     by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")) %>%
    dplyr::pull("head_pos") %in% c("NOUN", "PROPN")
  acl <- stringr::str_detect(tokens$dep_rel, "^acl")
  clause_children <- tokens %>%
    dplyr::filter(stringr::str_detect(.data$dep_rel, "^(aux|cop|nsubj)")) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int)
  reduced <- tokens$dep_rel == "acl" &
    !paste(tokens$doc_id, tokens$sentence_id, tokens$token_id_int) %in%
    paste(clause_children$doc_id, clause_children$sentence_id, clause_children$token_id_int)

  f24 <- tokens[tokens$is_infinitive, ] %>%
    dplyr::anti_join(nominal_infinitives, by = c("doc_id", "sentence_id", "token_id_int")) %>%
    count_feature_traced("f_24_infinitives")
  f25 <- tokens[tokens$is_present_participle & tokens$dep_rel %in% c("advcl", "ccomp") &
                  (initial_or_punct | after_en), ] %>%
    dplyr::bind_rows(gerondif_nouns) %>%
    count_feature_traced("f_25_present_participle")
  f27_rows <- tokens[tokens$is_past_participle & reduced & nominal_head & !initial_or_punct, ]
  f27 <- count_feature_traced(f27_rows, "f_27_past_participle_whiz")
  f26 <- tokens[tokens$is_past_participle &
                  (tokens$dep_rel %in% c("advcl", "ccomp") | (acl & initial_or_punct)) &
                  (initial_or_punct | after_en), ] %>%
    dplyr::anti_join(f27_rows, by = c("doc_id", "sentence_id", "token_id_int")) %>%
    count_feature_traced("f_26_past_participle")
  f28 <- tokens[tokens$is_present_participle & reduced & nominal_head & !initial_or_punct, ] %>%
    count_feature_traced("f_28_present_participle_whiz")
  make_block_result(doc_ids, bind_evidence(f24$evidence, f25$evidence, f26$evidence,
                                           f27$evidence, f28$evidence))
}

#' Resumptive and orphan-preposition relatives (French f_61)
#'
#' Colloquial competitors of pied-piping (D-29, D-47) in a relative introduced
#' by que directly after a noun. Within the relative (up to the next
#' punctuation or relative marker) one of:
#' \itemize{
#'   \item an orphan preposition or adverb (`orphan_prepositions`) with no
#'     complement of its own: la fille que je sors avec;
#'   \item a frame verb + preposition + tonic lui/elle/eux/elles
#'     (`resumptive_prepositional_frames`): un ami que je compte sur lui;
#'   \item resumptive clitic en before a verb whose de-complement dont would
#'     replace (`resumptive_en_verbs`): le film que je t'en ai parle.
#' }
#' One occurrence per relative, anchored on the first marker found.
#'
#' @param tokens Prepared tokens with context columns.
#' @param word_lists_lookup Word lists.
#' @return Data frame with `doc_id`, `sentence_id`, `que_id` (the relative
#'   que), `start` and `end` (marker span).
#' @keywords internal
resumptive_relative_rows_fr <- function(tokens, word_lists_lookup) {
  empty <- data.frame(doc_id = character(), sentence_id = integer(), que_id = integer(),
                      start = integer(), end = integer())
  orphans <- normalize_terms(get_word_list(word_lists_lookup, "orphan_prepositions"))
  frames <- strsplit(normalize_terms(get_word_list(word_lists_lookup,
                                                   "resumptive_prepositional_frames")), "_")
  frame_verbs <- vapply(frames, `[`, character(1), 1)
  frame_preps <- vapply(frames, `[`, character(1), 2)
  en_verbs <- normalize_terms(get_word_list(word_lists_lookup, "resumptive_en_verbs"))
  tonic <- c("lui", "elle", "eux", "elles")
  complement_pos <- c("DET", "NOUN", "PROPN", "PRON", "NUM", "ADJ", "VERB")

  ques <- which(tokens$match_token %in% c("que", "qu'") &
                  tokens$prev_pos %in% c("NOUN", "PROPN"))
  if (!length(ques)) return(empty)
  n <- nrow(tokens)
  found <- lapply(ques, function(q) {
    i <- q + 1L
    window <- integer()
    while (i <= n && tokens$doc_id[i] == tokens$doc_id[q] &&
           tokens$sentence_id[i] == tokens$sentence_id[q] &&
           !tokens$pos[i] %in% "PUNCT" &&
           !tokens$match_token[i] %in% c("que", "qu'", "qui", "dont")) {
      window <- c(window, i)
      i <- i + 1L
    }
    if (!length(window)) return(NULL)
    tok <- tokens$match_token[window]
    lem <- tokens$match_lemma[window]
    hits <- integer()
    ends <- integer()
    orphan <- window[tok %in% orphans & !tokens$next_pos[window] %in% complement_pos]
    if (length(orphan)) {
      hits <- c(hits, orphan[1]); ends <- c(ends, orphan[1])
    }
    preps <- window[seq_along(window) < length(window)]
    # The frame verb must be the relative's own predicate (its first lexical
    # verb), not a main-clause verb after it (la femme que j'ai vue parlait d'elle).
    own_verb <- window[tokens$pos[window] == "VERB"][1]
    for (k in preps) {
      prep <- sub("'$", "", tokens$match_token[k])
      prep <- ifelse(prep == "d", "de", prep)
      if (!normalize_terms(tokens$next_token[k]) %in% tonic || is.na(own_verb) || own_verb > k) next
      matched <- frame_preps == prep & frame_verbs == tokens$match_lemma[own_verb]
      if (any(matched)) {
        hits <- c(hits, k); ends <- c(ends, k + 1L)
        break
      }
    }
    en <- window[tok == "en" & tokens$pos[window] == "PRON"]
    for (k in en) {
      if (!is.na(own_verb) && own_verb > k && own_verb <= k + 3L &&
          tokens$match_lemma[own_verb] %in% en_verbs) {
        hits <- c(hits, k); ends <- c(ends, k)
        break
      }
    }
    if (!length(hits)) return(NULL)
    first <- which.min(hits)
    data.frame(doc_id = tokens$doc_id[q], sentence_id = tokens$sentence_id[q],
               que_id = tokens$token_id_int[q],
               start = tokens$token_id_int[hits[first]],
               end = tokens$token_id_int[ends[first]])
  })
  found <- dplyr::bind_rows(found)
  if (!nrow(found)) empty else found
}

#' Relative clause features (French)
#'
#' Exclusive partition (D-24/D-25), one relative pronoun per bin.
#' A nominal relative is a relative clause (`acl`) attached to a noun or
#' proper noun. Precedence: f_33 pied-piping (dont, contracted auquel/duquel
#' forms, or a preposition before qui/quoi/lequel), then f_34 sentence
#' relatives (punctuation + ce/cela... + relative pronoun), then the nominal
#' bins: f_29 qui (subject), f_30 que (object/complement), f_31/f_32 lequel
#' family in subject/other roles. French marks the role of qui/que by form.
#' que of a resumptive or orphan-preposition relative (f_61) is not in f_30.
#' où and free relatives (ce qui, celui qui) are not nominal-bin relatives.
#'
#' @param tokens Prepared tokens with context and clause flags.
#' @param doc_ids Document IDs.
#' @param head_lookup Head token attributes.
#' @param resumptive Rows from `resumptive_relative_rows_fr()`; their que
#'   counts in f_61, not in f_30.
#' @return Block result with f_29-f_34 evidence.
#' @keywords internal
block_relatives_fr <- function(tokens, doc_ids, head_lookup,
                               resumptive = NULL) {
  keys <- c("doc_id", "sentence_id", "token_id_int")
  if (is.null(resumptive)) {
    resumptive <- data.frame(doc_id = character(), sentence_id = integer(), que_id = integer())
  }
  lequel <- c("lequel", "laquelle", "lesquel", "lesquelle", "lesquels", "lesquelles")
  contracted <- c("auquel", "auxquels", "auxquelles", "duquel", "desquels", "desquelles")
  anchors <- c("ce", "cela", "ceci", "celui", "celle", "ceux", "celles")
  clauses <- tokens %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id, antecedent_id = .data$head_token_id_int,
                     clause_id = .data$token_id_int, clause_dep = .data$dep_rel)
  antecedents <- dplyr::select(head_lookup, "doc_id", "sentence_id",
                               antecedent_id = "token_id_int", antecedent_pos = "head_pos")
  relatives <- tokens[tokens$is_relative_pronoun &
                        tokens$match_lemma %in% c("qui", "que", "qu'", "quoi", "dont",
                                                  lequel, contracted), ] %>%
    dplyr::left_join(clauses, by = c("doc_id", "sentence_id", "head_token_id_int" = "clause_id")) %>%
    dplyr::left_join(antecedents, by = c("doc_id", "sentence_id", "antecedent_id")) %>%
    dplyr::mutate(
      nominal = stringr::str_detect(dplyr::coalesce(.data$clause_dep, ""), "^acl") &
        .data$antecedent_pos %in% c("NOUN", "PROPN"),
      pied = .data$match_lemma %in% c("dont", contracted) |
        (.data$prev_pos %in% "ADP" & .data$match_lemma %in% c("qui", "quoi", lequel)),
      subject = stringr::str_detect(.data$dep_rel, "^nsubj")
    )

  f33_rows <- relatives[relatives$pied & relatives$nominal, ]
  f33 <- count_feature_traced(f33_rows, "f_33_pied_piping")

  f34_rows <- tokens[tokens$is_relative_pronoun & tokens$prev_token %in% anchors &
                       tokens$prev2_pos %in% "PUNCT", ] %>%
    dplyr::anti_join(f33_rows, by = keys)
  f34 <- span_evidence(tokens, data.frame(doc_id = f34_rows$doc_id,
                                          sentence_id = f34_rows$sentence_id,
                                          start = f34_rows$prev_id, end = f34_rows$token_id_int),
                       "f_34_sentence_relatives")

  nominal <- relatives[relatives$nominal & !relatives$pied, ] %>%
    dplyr::anti_join(f34_rows, by = keys) %>%
    dplyr::anti_join(resumptive, by = c("doc_id", "sentence_id", "token_id_int" = "que_id"))
  f29 <- count_feature_traced(nominal[nominal$match_lemma == "qui", ], "f_29_that_subj")
  f30 <- count_feature_traced(nominal[nominal$match_lemma %in% c("que", "qu'"), ], "f_30_that_obj")
  f31 <- count_feature_traced(nominal[nominal$match_lemma %in% lequel & nominal$subject, ],
                              "f_31_wh_subj")
  f32 <- count_feature_traced(nominal[nominal$match_lemma %in% lequel & !nominal$subject, ],
                              "f_32_wh_obj")
  make_block_result(doc_ids, bind_evidence(f29$evidence, f30$evidence, f31$evidence,
                                           f32$evidence, f33$evidence, f34))
}

#' Clause embedding, adverbial subordination and that-deletion (French)
#'
#' f_21/f_22: complementizer que whose clause (headed by any part of speech,
#' including copular predicates) is a ccomp/xcomp/csubj of a verb (f_21), or
#' whose governing predicate is an adjective (f_22).
#' f_23: interrogative words introducing an embedded complement clause, and
#' free relatives with ce que/ce qui that complement a verb (one occurrence,
#' recorded as the two-word span); relative clauses on nouns, sentence
#' relatives and direct questions are excluded. f_35 parce que; f_36 bien
#' que, quoique; f_37 conditional si (not indirect-question si: a ccomp/xcomp
#' clause governed by a question or knowledge verb), a moins que, sauf si; f_38 other `mark` subordinators
#' (puisque, comme, quand...) plus a condition que, once per phrase. que
#' inside these phrases is not an f_21/f_22 complementizer. f_60 is computed
#' by `block_reduced_complements_fr()`.
#'
#' @param tokens Prepared tokens with context columns.
#' @param doc_ids Document IDs.
#' @param head_lookup Head token attributes.
#' @return Block result with f_21-f_23 and f_35-f_38 evidence.
#' @keywords internal
block_clause_embedding_fr <- function(tokens, doc_ids, head_lookup) {
  complementizers <- c("que", "qu'")
  wh_lemmas <- c("qui", "que", "quoi", "o\u00f9", "ou", "quand", "comment", "pourquoi",
                 "combien", "lequel", "laquelle", "lesquels", "lesquelles")
  keys <- c("doc_id", "sentence_id")

  clause_heads <- tokens %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id, governor_id = .data$head_token_id_int,
                     head_token_id_int = .data$token_id_int, clause_dep_rel = .data$dep_rel)
  governors <- tokens %>%
    dplyr::transmute(.data$doc_id, .data$sentence_id, governor_id = .data$token_id_int,
                     governor_pos = .data$pos)
  que_marks <- tokens %>%
    dplyr::filter(.data$match_token %in% complementizers | .data$lemma == "que",
                  .data$pos == "SCONJ", .data$dep_rel == "mark") %>%
    dplyr::left_join(head_lookup,
                     by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")) %>%
    dplyr::left_join(clause_heads, by = c(keys, "head_token_id_int")) %>%
    dplyr::left_join(governors, by = c(keys, "governor_id"))
  # Adverbial subordinators (Biber 35-38; D-44). Only unambiguous markers are
  # f_35-f_37: parce que; bien que/quoique; conditional si, a moins que and
  # sauf si (meme si counts through si, like English "even if"). puisque,
  # comme, encore que and a condition que are f_38; car (coordinator, like
  # English "for") is not counted.
  sconj_next <- tokens$next_pos %in% "SCONJ"
  no_phrase <- rep(FALSE, nrow(tokens))
  span_of <- function(single, next_phrase, around_phrase = no_phrase) {
    keep <- single | next_phrase | around_phrase
    data.frame(
      doc_id = tokens$doc_id[keep], sentence_id = tokens$sentence_id[keep],
      start = ifelse(around_phrase, tokens$prev_id, tokens$token_id_int)[keep],
      end = ifelse(next_phrase | around_phrase, tokens$next_id, tokens$token_id_int)[keep]
    )
  }
  clause_idx <- match(
    paste(tokens$doc_id, tokens$sentence_id, tokens$head_token_id_int),
    paste(clause_heads$doc_id, clause_heads$sentence_id, clause_heads$head_token_id_int)
  )
  clause_dep <- clause_heads$clause_dep_rel[clause_idx]
  governor_lemma <- tokens$lemma[match(
    paste(tokens$doc_id, tokens$sentence_id, clause_heads$governor_id[clause_idx]),
    paste(tokens$doc_id, tokens$sentence_id, tokens$token_id_int)
  )]
  si_mark <- tokens$match_token %in% c("si", "s'") & tokens$pos == "SCONJ" &
    stringr::str_detect(tokens$dep_rel, "^mark")
  # Indirect yes/no questions (je me demande si..., je ne sais pas si...) are
  # complements, not conditions. As in Spanish, both a complement relation and
  # a question/knowledge governor are required: UDPipe also attaches postposed
  # conditionals as ccomp (Je serais venu si tu m'avais appele).
  interrogative_governors <- c(
    "demander", "interroger", "questionner", "savoir", "ignorer", "douter",
    "v\u00e9rifier", "contr\u00f4ler", "tester", "chercher", "voir", "regarder",
    "d\u00e9cider", "d\u00e9terminer", "rappeler", "souvenir", "raconter", "dire",
    "expliquer", "pr\u00e9ciser", "indiquer", "comprendre", "deviner", "oublier",
    "consulter", "examiner", "\u00e9valuer", "juger"
  )
  indirect_si <- si_mark &
    stringr::str_detect(dplyr::coalesce(clause_dep, ""), "^(ccomp|xcomp)($|:)") &
    governor_lemma %in% interrogative_governors
  f35_spans <- span_of(
    no_phrase,
    tokens$token == "parce" & tokens$next_token %in% complementizers
  )
  f36_spans <- span_of(
    tokens$match_token %in% c("quoique", "quoiqu'") & tokens$pos == "SCONJ",
    tokens$token == "bien" & tokens$next_token %in% complementizers & sconj_next
  )
  f37_spans <- span_of(
    si_mark & !indirect_si & !tokens$prev_token %in% "sauf",
    tokens$token == "sauf" & tokens$next_token %in% c("si", "s'") & sconj_next,
    tokens$token == "moins" & normalize_terms(dplyr::coalesce(tokens$prev_token, "")) == "\u00e0" &
      tokens$next_token %in% complementizers
  )
  f38_phrase_spans <- span_of(
    no_phrase, no_phrase,
    tokens$token == "condition" & tokens$prev_token %in% "\u00e0" &
      tokens$next_token %in% complementizers
  )
  phrase_words <- dplyr::bind_rows(f35_spans, f36_spans, f37_spans, f38_phrase_spans) %>%
    dplyr::rename(from = "start", to = "end")
  outside_phrases <- function(rows) {
    if (!nrow(phrase_words)) return(rows)
    dplyr::anti_join(rows, phrase_words,
                     by = dplyr::join_by("doc_id", "sentence_id", "token_id_int" >= "from",
                                         "token_id_int" <= "to"))
  }

  que_marks <- outside_phrases(que_marks)
  f21 <- que_marks %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$clause_dep_rel, ""), "^(ccomp|xcomp|csubj)($|:)"),
      .data$governor_pos %in% c("VERB", "AUX")
    ) %>%
    count_feature_traced("f_21_that_verb_comp")
  f22 <- que_marks %>%
    dplyr::filter(
      !stringr::str_detect(dplyr::coalesce(.data$clause_dep_rel, ""), "^acl"),
      .data$governor_pos %in% "ADJ"
    ) %>%
    count_feature_traced("f_22_that_adj_comp")

  f23 <- tokens %>%
    dplyr::filter(
      .data$match_lemma %in% wh_lemmas,
      .data$pos %in% c("PRON", "ADV", "DET", "ADJ"),
      stringr::str_detect(.data$dep_rel, "^(obj|obl|nsubj|iobj|advmod|mark|det)"),
      # Relative qui/que/lequel (pseudo-relatives: je l'ai vu qui partait) are not
      # interrogatives; UDPipe also marks interrogative où as Rel, so adverbs stay.
      !(.data$match_lemma %in% c("qui", "que", "quoi", "lequel", "laquelle", "lesquels",
                                 "lesquelles") &
          stringr::str_detect(dplyr::coalesce(.data$morph_prontype, ""), "Rel"))
    ) %>%
    dplyr::left_join(clause_heads, by = c(keys, "head_token_id_int")) %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$clause_dep_rel, ""),
                          "^(ccomp|csubj|xcomp|obj|nsubj|obl|advcl)")
    ) %>%
    count_feature_traced("f_23_wh_clause")
  # Free relatives with ce que/ce qui complementing a verb (Je sais ce qui
  # s'est passé), the French form of indirect "what" clauses. Clauses after
  # punctuation or at sentence start (sentence relatives, f_34) and clauses
  # governed by a preposition (dans ce qui...) are excluded.
  free_relatives <- tokens %>%
    dplyr::filter(
      .data$match_token == "ce", .data$pos == "PRON",
      .data$next_token %in% c("que", "qu'", "qui"),
      !is.na(.data$prev_pos), .data$prev_pos != "PUNCT",
      stringr::str_detect(.data$dep_rel, "^(obj|nsubj|ccomp|xcomp|csubj)")
    ) %>%
    dplyr::left_join(head_lookup,
                     by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")) %>%
    dplyr::filter(.data$head_pos %in% "VERB")
  f23_free <- span_evidence(
    tokens,
    data.frame(doc_id = free_relatives$doc_id, sentence_id = free_relatives$sentence_id,
               start = free_relatives$token_id_int, end = free_relatives$next_id),
    "f_23_wh_clause"
  )

  f35 <- span_evidence(tokens, f35_spans, "f_35_because")
  f36 <- span_evidence(tokens, f36_spans, "f_36_though")
  f37 <- span_evidence(tokens, f37_spans, "f_37_if")

  counted <- c(complementizers, "parce", "quoique", "quoiqu'", "si", "s'")
  # A preposition marking a non-finite clause (de/à/pour/sans + infinitive,
  # en + gérondif) is not an adverbial subordinator; pour que, avant que and
  # pendant que introduce finite clauses and remain.
  finite_aux <- tokens %>%
    dplyr::filter(
      (stringr::str_detect(.data$dep_rel, "^(aux|cop)") & .data$morph_verbform %in% "Fin") |
        stringr::str_detect(.data$dep_rel, "^nsubj") |
        (.data$dep_rel == "mark" & .data$match_token %in% complementizers)
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, clause_id = .data$head_token_id_int)
  head_form <- tokens$morph_verbform[match(
    paste(tokens$doc_id, tokens$sentence_id, tokens$head_token_id_int),
    paste(tokens$doc_id, tokens$sentence_id, tokens$token_id_int)
  )]
  head_has_finite_aux <- paste(tokens$doc_id, tokens$sentence_id, tokens$head_token_id_int) %in%
    paste(finite_aux$doc_id, finite_aux$sentence_id, finite_aux$clause_id)
  nonfinite_adp <- tokens$pos == "ADP" & head_form %in% c("Inf", "Ger", "Part") &
    !head_has_finite_aux
  f38_rows <- tokens[tokens$pos %in% c("SCONJ", "ADP", "ADV") &
                       stringr::str_detect(tokens$dep_rel, "^mark") &
                       !tokens$match_token %in% counted & !nonfinite_adp, ] %>%
    outside_phrases()
  f38 <- bind_evidence(
    count_feature_traced(f38_rows, "f_38_other_adv_sub")$evidence,
    span_evidence(tokens, f38_phrase_spans, "f_38_other_adv_sub")
  )

  make_block_result(doc_ids, bind_evidence(f21$evidence, f22$evidence, f23$evidence, f23_free,
                                           f35, f36, f37, f38))
}

#' Reduced complement clauses (French f_60)
#'
#' Port of the Spanish selector (D-27, D-46). f_60 counts the reduced
#' realization of a que-clause under the predicates in
#' `reduced_complement_predicates` (croire, penser, esperer, dire, affirmer,
#' pretendre): a bare infinitive (`Je crois avoir compris`, `Il dit avoir vu le
#' voleur`) or a finite complement without que (`Je crois il est parti`).
#' Auxiliary and copular finite/infinitive forms license participial or
#' nonverbal clause heads; clear coordinated complements inherit the context.
#' Clauses introduced by a preposition, subordinator or interrogative word
#' (`penser a partir`, `dire de partir`, `je pense que...`), nominal
#' infinitives and other governors are excluded. Infinitives also remain in
#' f_24. One occurrence per clause head.
#'
#' @param tokens Prepared tokens.
#' @param doc_ids Document IDs.
#' @param head_lookup Head token attributes.
#' @param predicates Governing predicate lemmas.
#' @param nominal_infinitives Rows counted in f_15.
#' @return Block result with f_60 evidence.
#' @keywords internal
block_reduced_complements_fr <- function(tokens, doc_ids, head_lookup, predicates,
                                         nominal_infinitives = tokens[0, ]) {
  keys <- c("doc_id", "sentence_id", "token_id_int")
  wh_lemmas <- c("qui", "que", "quoi", "o\u00f9", "quand", "comment", "pourquoi",
                 "combien", "lequel", "laquelle", "lesquels", "lesquelles")
  introduced <- tokens %>%
    dplyr::filter(
      stringr::str_detect(.data$dep_rel, "^(mark|case)($|:)") | .data$pos == "SCONJ" |
        .data$match_lemma %in% c("que", "qu'") |
        (.data$match_lemma %in% wh_lemmas & .data$pos %in% c("PRON", "ADV"))
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int) %>%
    dplyr::mutate(introduced = TRUE)
  candidates <- dplyr::left_join(tokens, introduced, by = keys)
  clauses <- candidates %>%
    dplyr::filter(stringr::str_detect(.data$dep_rel, "^(ccomp|xcomp)($|:)")) %>%
    dplyr::left_join(head_lookup,
                     by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")) %>%
    dplyr::filter(.data$head_pos == "VERB", normalize_terms(.data$head_lemma) %in% predicates) %>%
    dplyr::anti_join(nominal_infinitives, by = keys) %>%
    dplyr::select(dplyr::all_of(names(candidates))) %>%
    dplyr::mutate(introduced = dplyr::coalesce(.data$introduced, FALSE))
  coordinated <- dplyr::filter(candidates, stringr::str_detect(.data$dep_rel, "^conj($|:)"))
  repeat {
    children <- coordinated %>%
      dplyr::inner_join(
        dplyr::select(clauses, dplyr::all_of(keys), parent_introduced = "introduced"),
        by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
      ) %>%
      dplyr::anti_join(clauses, by = keys) %>%
      dplyr::anti_join(nominal_infinitives, by = keys) %>%
      dplyr::mutate(introduced = dplyr::coalesce(.data$introduced, FALSE) |
                      .data$parent_introduced) %>%
      dplyr::select(dplyr::all_of(names(clauses)))
    if (nrow(children) == 0L) break
    clauses <- dplyr::bind_rows(clauses, children)
  }
  verbal_forms <- dplyr::filter(tokens, .data$pos %in% c("VERB", "AUX"),
                                .data$morph_verbform %in% c("Fin", "Inf"))
  forms <- dplyr::bind_rows(
    dplyr::select(verbal_forms, dplyr::all_of(keys)),
    verbal_forms %>%
      dplyr::filter(stringr::str_detect(.data$dep_rel, "^(aux|cop)($|:)")) %>%
      dplyr::transmute(.data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int)
  ) %>%
    dplyr::distinct()
  rows <- tokens %>%
    dplyr::semi_join(dplyr::filter(clauses, !.data$introduced), by = keys) %>%
    dplyr::semi_join(forms, by = keys)
  make_block_result(doc_ids, count_feature_traced(rows, "f_60_that_deletion")$evidence)
}

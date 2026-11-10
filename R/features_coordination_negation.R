# Contractions, stranded prepositions, split structures, coordination,
# negation and lexical-membership features

#' Contraction features (French)
#'
#' f_59 counts colloquial reductions that compete with an available full form
#' (D-26, D-30, D-45), as English contractions compete with full forms:
#' \itemize{
#'   \item listed forms (`colloquial_contractions`: p'tit, m'sieur, chuis,
#'     j'sais, y'a...), longest match, straight or curly apostrophe, once
#'     across native splits;
#'   \item subject tu reduced to t' before a finite singular verb (t'es,
#'     t'as, t'avais) with no subject before it, skipping ne (excludes object
#'     or reflexive te: je t'aime, tu t'es trompé, ça t'a plu);
#'   \item subject je reduced to j' before a consonant other than h
#'     (j'pense, j'veux);
#'   \item il y a without il (y a, y en a), excluding y a-t-il.
#' }
#' Obligatory elision (j'ai, l'homme, c'est, qu'il, aujourd'hui) has no
#' competing form and is not counted. One occurrence per reduction, recorded
#' as the span from the reduced word to its host.
#'
#' @param tokens Prepared tokens with context columns.
#' @param doc_ids Document IDs.
#' @param word_lists_lookup Word lists.
#' @return Block result with f_59 evidence.
#' @keywords internal
block_contractions_fr <- function(tokens, doc_ids, word_lists_lookup) {
  forms <- get_word_list(word_lists_lookup, "colloquial_contractions")
  hits <- surface_matches(tokens, forms)
  listed <- data.frame(
    doc_id = tokens$doc_id[hits$start_row], sentence_id = tokens$sentence_id[hits$start_row],
    start = tokens$token_id_int[hits$start_row], end = tokens$token_id_int[hits$end_row]
  )

  # A reduced clitic may be split from its apostrophe (T ' es); the host is
  # then the word after the lone apostrophe.
  lone_apostrophe <- tokens$next_token %in% c("'", "\u2019")
  host_token <- normalize_terms(ifelse(lone_apostrophe, tokens$next2_token, tokens$next_token))
  host_id <- ifelse(lone_apostrophe, tokens$next2_id, tokens$next_id)
  host_idx <- match(paste(tokens$doc_id, tokens$sentence_id, host_id),
                    paste(tokens$doc_id, tokens$sentence_id, tokens$token_id_int))
  clitic <- function(letter) {
    tokens$match_token == paste0(letter, "'") | (tokens$match_token == letter & lone_apostrophe)
  }
  # The word before the clitic, skipping ne (tu ne t'es pas).
  skip_ne <- normalize_terms(dplyr::coalesce(tokens$prev_token, "")) %in% c("ne", "n'")
  before_pos <- ifelse(skip_ne, tokens$prev2_pos, tokens$prev_pos)
  before_token <- normalize_terms(ifelse(skip_ne, tokens$prev2_token, tokens$prev_token))
  after_subject <- before_pos %in% c("PRON", "NOUN", "PROPN")
  # UDPipe marks 1sg/2sg-ambiguous forms (avais, étais) as Person=1. With no
  # subject before it, t' before a finite singular verb is reduced subject tu;
  # after a subject (je t'ai vu, tu t'es trompé) it is object/reflexive te.
  host_sg <- tokens$pos[host_idx] %in% c("VERB", "AUX") &
    tokens$morph_person[host_idx] %in% c("1", "2") &
    tokens$morph_number[host_idx] %in% "Sing" &
    tokens$morph_verbform[host_idx] %in% "Fin"
  consonant <- "^[bcdfgjklmnpqrstvwxz\u00e7]"

  tu_split <- clitic("t") & host_sg & !after_subject
  tu_joined <- stringr::str_detect(tokens$match_token, "^t'(es|as|\u00e9tais|avais|auras|seras)$") &
    !after_subject
  je_split <- clitic("j") & stringr::str_detect(dplyr::coalesce(host_token, ""), consonant)
  je_joined <- stringr::str_detect(tokens$match_token, paste0("^j'", substring(consonant, 2), "[[:alpha:]]"))
  avoir_a <- c("a", "avait", "aura", "aurait")
  y_a <- tokens$match_token == "y" & !before_token %in% "il" &
    (tokens$next_token %in% avoir_a |
       (tokens$next_token %in% "en" & tokens$next2_token %in% avoir_a)) &
    !stringr::str_detect(dplyr::coalesce(normalize_terms(tokens$next2_token), ""), "^-?t(-il)?$")
  y_end <- ifelse(tokens$next_token %in% "en", tokens$next2_id, tokens$next_id)

  rule <- function(keep, end) {
    data.frame(doc_id = tokens$doc_id[keep], sentence_id = tokens$sentence_id[keep],
               start = tokens$token_id_int[keep], end = end[keep])
  }
  ruled <- dplyr::bind_rows(
    rule(tu_split | je_split, host_id),
    rule(tu_joined | je_joined, tokens$token_id_int),
    rule(y_a, y_end)
  ) %>%
    dplyr::filter(!is.na(.data$end))
  if (nrow(listed)) {
    ruled <- dplyr::anti_join(
      ruled, listed,
      by = dplyr::join_by("doc_id", "sentence_id", "start" >= "start", "start" <= "end")
    )
  }
  f59 <- span_evidence(tokens, dplyr::bind_rows(listed, ruled), "f_59_contractions")
  make_block_result(doc_ids, f59)
}

#' Stranded preposition and split infinitive features (French)
#'
#' f_61: colloquial competitors of pied-piping (D-29, D-47) from
#' `resumptive_relative_rows_fr()`: orphan preposition, resumptive PP or
#' resumptive en, one per relative. Pied-piping itself is f_33.
#' f_62 (D-28, D-48): a preposition linked to an infinitive and separated from
#' it by adverbs (and clitic pronouns), at least one adverb being neither a
#' negation nor a light adverb that normally precedes the infinitive
#' (`preinfinitival_light_adverbs`: bien, mieux, trop...). `de vraiment
#' comprendre` and `sans vraiment savoir` count; `pour mieux comprendre` and
#' `de ne pas venir` do not. Nominal infinitives are excluded. One occurrence
#' per infinitive, recorded as the span from the preposition.
#'
#' @param tokens Prepared tokens with context columns.
#' @param doc_ids Document IDs.
#' @param resumptive Rows from `resumptive_relative_rows_fr()`.
#' @param word_lists_lookup Word lists.
#' @param nominal_infinitives Rows counted in f_15.
#' @return Block result with f_61 and f_62 evidence.
#' @keywords internal
block_stranded_split_fr <- function(tokens, doc_ids, resumptive = NULL,
                                    word_lists_lookup = list(),
                                    nominal_infinitives = tokens[0, ]) {
  if (is.null(resumptive)) {
    resumptive <- data.frame(doc_id = character(), sentence_id = integer(),
                             start = integer(), end = integer())
  }
  f61 <- span_evidence(tokens, dplyr::select(resumptive, "doc_id", "sentence_id", "start", "end"),
                       "f_61_stranded_preposition")

  # f_62 (D-28, D-48): preposition + adverb(s) + infinitive, where an adverb
  # that normally follows the infinitive comes before it. Light adverbs that
  # normally precede (pour mieux comprendre) and negation (de ne pas venir)
  # do not qualify on their own. Clitic pronouns may intervene.
  negation <- c("ne", "n'", "pas", "plus", "jamais", "rien", "point", "gu\u00e8re", "non")
  light <- normalize_terms(get_word_list(word_lists_lookup, "preinfinitival_light_adverbs"))
  clitics <- c("le", "la", "les", "l'", "se", "s'", "y", "en", "me", "m'", "te", "t'",
               "lui", "leur", "nous", "vous")
  nominal_keys <- paste(nominal_infinitives$doc_id, nominal_infinitives$sentence_id,
                        nominal_infinitives$token_id_int)
  infinitives <- which(tokens$pos %in% c("VERB", "AUX") & tokens$morph_verbform %in% "Inf" &
                         !paste(tokens$doc_id, tokens$sentence_id, tokens$token_id_int) %in%
                         nominal_keys)
  same_sentence <- function(i, j) {
    j >= 1L && tokens$doc_id[j] == tokens$doc_id[i] && tokens$sentence_id[j] == tokens$sentence_id[i]
  }
  spans <- lapply(infinitives, function(i) {
    j <- i - 1L
    adverbs <- character()
    while (same_sentence(i, j) &&
           (tokens$pos[j] == "ADV" ||
              (tokens$pos[j] == "PRON" && tokens$match_token[j] %in% clitics))) {
      if (tokens$pos[j] == "ADV") adverbs <- c(adverbs, tokens$match_token[j])
      j <- j - 1L
    }
    if (!length(adverbs) || !same_sentence(i, j) || tokens$pos[j] != "ADP") return(NULL)
    linked <- tokens$head_token_id_int[j] %in% tokens$token_id_int[i] ||
      stringr::str_detect(tokens$dep_rel[j], "^(mark|case|fixed)")
    if (!linked || !any(!adverbs %in% c(negation, light))) return(NULL)
    data.frame(doc_id = tokens$doc_id[i], sentence_id = tokens$sentence_id[i],
               start = tokens$token_id_int[j], end = tokens$token_id_int[i])
  })
  f62 <- span_evidence(tokens, dplyr::bind_rows(
    data.frame(doc_id = character(), sentence_id = integer(), start = integer(), end = integer()),
    spans
  ), "f_62_split_infinitive")
  make_block_result(doc_ids, bind_evidence(f61, f62))
}

#' Split auxiliary and coordination features (French)
#'
#' f_63 (D-49): avoir/être, or modal pouvoir/devoir, separated from its own
#' participle/infinitive by adverbs, at least one of them other than the
#' analytic negator ne/pas/point/plus (`a souvent mangé`, `n'a jamais vu`,
#' `peut facilement résoudre`; not `n'a pas mangé`). One occurrence per
#' auxiliary. f_64: a coordinator joining two
#' words of the same POS (NOUN, PROPN, ADJ, ADV, VERB, AUX) when the second
#' conjunct has no subject of its own. f_65: a coordinator joining finite
#' verbal predicates that have their own subject.
#'
#' @param tokens Prepared tokens.
#' @param doc_ids Document IDs.
#' @param token_lookup Token attributes keyed by token ID.
#' @param subject_heads Clause heads with an explicit subject.
#' @param head_lookup Head token attributes.
#' @param negation_part_lemmas Negation particle lemmas.
#' @return Block result with f_63-f_65 evidence.
#' @keywords internal
block_split_coordination_fr <- function(tokens, doc_ids, token_lookup, subject_heads,
                                        head_lookup, negation_part_lemmas) {
  keys <- c("doc_id", "sentence_id")
  # f_63 (D-49): AUX + ADV + (ADV) + VB, as in English and Spanish. The
  # auxiliary is avoir/être or modal pouvoir/devoir; the verb is its own
  # participle or infinitive. The analytic negator alone (n'a pas mangé), the
  # counterpart of English not, does not count; negative adverbs do (n'a
  # jamais vu, like has never seen). Clitics may precede a modal's infinitive.
  negation <- c("ne", "n'", "pas", "point", "plus")
  clitics <- c("le", "la", "les", "l'", "se", "s'", "y", "en", "me", "m'", "te", "t'",
               "lui", "leur", "nous", "vous")
  aux_lemmas <- c("avoir", "\u00eatre")
  modal_lemmas <- c("pouvoir", "devoir")
  candidates <- which(tokens$pos %in% c("AUX", "VERB") &
                        tokens$match_lemma %in% c(aux_lemmas, modal_lemmas))
  same_sentence <- function(i, j) {
    j <= nrow(tokens) && tokens$doc_id[j] == tokens$doc_id[i] &&
      tokens$sentence_id[j] == tokens$sentence_id[i]
  }
  splits <- lapply(candidates, function(i) {
    modal <- tokens$match_lemma[i] %in% modal_lemmas
    j <- i + 1L
    adverbs <- character()
    while (same_sentence(i, j) &&
           (tokens$pos[j] %in% c("ADV", "PART") ||
              (tokens$pos[j] == "PRON" & tokens$match_token[j] %in% if (modal) clitics))) {
      if (tokens$pos[j] != "PRON") adverbs <- c(adverbs, tokens$match_token[j])
      j <- j + 1L
    }
    if (!length(adverbs) || !same_sentence(i, j) || all(adverbs %in% negation)) return(NULL)
    form <- if (modal) "Inf" else "Part"
    if (!tokens$morph_verbform[j] %in% form) return(NULL)
    linked <- tokens$head_token_id_int[i] %in% tokens$token_id_int[j] ||
      tokens$head_token_id_int[j] %in% tokens$token_id_int[i] ||
      (stringr::str_detect(tokens$dep_rel[j], "^aux") &&
         tokens$head_token_id_int[i] %in% tokens$head_token_id_int[j])
    if (!linked) return(NULL)
    tokens[i, ]
  })
  f63 <- dplyr::bind_rows(tokens[0, ], splits) %>%
    count_feature_traced("f_63_split_auxiliary")

  cc <- tokens %>%
    dplyr::filter(.data$pos == "CCONJ", .data$dep_rel == "cc") %>%
    dplyr::left_join(token_lookup, by = c(keys, "head_token_id_int" = "token_id_int")) %>%
    dplyr::left_join(
      dplyr::select(token_lookup, "doc_id", "sentence_id", "token_id_int",
                    first_conj_pos = "token_pos"),
      by = c(keys, "token_head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::left_join(subject_heads, by = c(keys, "head_token_id_int" = "clause_head_token_id_int")) %>%
    dplyr::mutate(has_subject = dplyr::coalesce(.data$has_subject, FALSE))
  f64 <- cc %>%
    dplyr::filter(
      .data$token_dep_rel %in% "conj",
      .data$token_pos %in% c("NOUN", "PROPN", "ADJ", "ADV", "VERB", "AUX"),
      dplyr::coalesce(.data$first_conj_pos == .data$token_pos, FALSE),
      !.data$has_subject
    ) %>%
    count_feature_traced("f_64_phrasal_coordination")
  f65 <- cc %>%
    dplyr::filter(
      .data$token_dep_rel %in% "conj",
      .data$token_pos %in% c("VERB", "AUX"),
      .data$has_subject,
      !.data$token_morph_verbform %in% c("Inf", "Ger", "Part")
    ) %>%
    count_feature_traced("f_65_clausal_coordination")
  make_block_result(doc_ids, bind_evidence(f63$evidence, f64$evidence, f65$evidence))
}

#' Negation features (French)
#'
#' Tottie's distinction (D-18), parallel to Spanish D-20 and D-17.
#' f_66 synthetic negation: each negative word (aucun, nul, personne, rien,
#' jamais, ni, aucunement, nullement), including in concord with ne
#' (`Personne n'a rien dit` = 2). Excluded: words with an article (`une
#' personne`, `un rien`), nouns other than determinerless personne in a
#' sentence with ne (a common misparse of `Je ne vois personne`), and
#' non-negative jamais after si/à/que (`si jamais`, `à jamais`, `plus que
#' jamais`). f_67 analytic negation: the particle pas (with or without ne,
#' `c'est pas grave`), point or plus sharing a head with ne (`il n'en veut
#' plus`, not comparative `plus grand`), and constituent non (`non
#' négligeable`), not the reply `Non.`. ne alone (`ne ... que`, expletive ne)
#' is not counted.
#'
#' @param tokens Prepared tokens with context columns.
#' @param doc_ids Document IDs.
#' @param neg_synthetic_terms Synthetic negation lemmas.
#' @param negation_part_lemmas Concord particle lemmas (ne).
#' @param negation_adverbs Analytic negation particles (pas, point, plus).
#' @return Block result with f_66 and f_67 evidence.
#' @keywords internal
block_negation_fr <- function(tokens, doc_ids, neg_synthetic_terms, negation_part_lemmas,
                              negation_adverbs) {
  keys <- c("doc_id", "sentence_id", "token_id_int")
  articles <- tokens %>%
    dplyr::filter(.data$dep_rel == "det",
                  .data$morph_prontype %in% "Art" | .data$lemma %in% c("un", "le")) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int)
  ne_tokens <- tokens[tokens$match_lemma %in% negation_part_lemmas |
                        tokens$match_token %in% c("ne", "n'"), ]
  ne_sentences <- dplyr::distinct(ne_tokens, .data$doc_id, .data$sentence_id)
  ne_heads <- dplyr::distinct(ne_tokens, .data$doc_id, .data$sentence_id, .data$head_token_id_int)

  candidates <- tokens[tokens$match_lemma %in% neg_synthetic_terms |
                         tokens$match_token %in% neg_synthetic_terms, ] %>%
    dplyr::anti_join(articles, by = keys)
  word_pos <- !candidates$pos %in% c("NOUN", "PROPN", "VERB", "AUX")
  personne_noun <- candidates$pos == "NOUN" & candidates$match_lemma == "personne" &
    paste(candidates$doc_id, candidates$sentence_id) %in%
    paste(ne_sentences$doc_id, ne_sentences$sentence_id)
  not_ever <- candidates$match_lemma == "jamais" &
    candidates$prev_token %in% c("si", "s'", "\u00e0", "que", "qu'")
  f66 <- count_feature_traced(candidates[(word_pos | personne_noun) & !not_ever, ],
                              "f_66_neg_synthetic")

  particles <- tokens[tokens$pos %in% c("ADV", "PART"), ]
  shares_ne <- paste(particles$doc_id, particles$sentence_id, particles$head_token_id_int) %in%
    paste(ne_heads$doc_id, ne_heads$sentence_id, ne_heads$head_token_id_int)
  analytic <- (particles$match_lemma == "pas" & "pas" %in% negation_adverbs) |
    (particles$match_lemma %in% setdiff(negation_adverbs, "pas") & shares_ne) |
    (particles$match_token == "non" & stringr::str_detect(particles$dep_rel, "^advmod"))
  f67 <- count_feature_traced(particles[analytic, ], "f_67_neg_analytic")
  make_block_result(doc_ids, bind_evidence(f66$evidence, f67$evidence))
}

#' Infinitives functioning as nouns (French f_15)
#'
#' Parallel to the Spanish selector (D-23, extended 2026-11-09). An
#' infinitive-shaped word (-er/-ir/-re/-oir) with infinitive morphology or a
#' known verb lemma is nominal when (a) it has a determiner, possessive or
#' attributive adjective and no verbal dependents (`le savoir`, `son rire`,
#' `le boire et le manger`), or (b) it heads a subject clause placed before its
#' predicate (`Fumer tue`, `Lire est un plaisir`), like an English gerund
#' subject. Postposed subjects (`Il est important de se reposer`), objects and
#' prepositional infinitives remain verbal infinitives (f_24).
#'
#' @param tokens Prepared tokens.
#' @param word_lists_lookup Word lists.
#' @param dict_lookup Dictionary.
#' @return Token rows of nominal infinitives.
#' @keywords internal
nominal_infinitive_rows_fr <- function(tokens, word_lists_lookup, dict_lookup) {
  keys <- c("doc_id", "sentence_id", "token_id_int")
  heads <- function(rows) {
    dplyr::distinct(rows, .data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int)
  }
  verb_features <- c("f_52_modal_possibility", "f_53_modal_necessity", "f_55_verb_public",
                     "f_56_verb_private", "f_57_verb_suasive", "f_58_verb_seem")
  known <- normalize_terms(c(
    get_word_list(word_lists_lookup, "nominal_infinitive_lemmas"),
    get_word_list(word_lists_lookup, "impersonal_verbs"),
    get_word_list(word_lists_lookup, "linking_matchlist"),
    get_word_list(word_lists_lookup, "verb_matchlist"),
    unlist(lapply(verb_features, function(feature) dictionary_to_lemmas(dict_lookup, feature)),
           use.names = FALSE)
  ))
  candidates <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX", "NOUN", "PROPN"),
      stringr::str_detect(.data$match_token, "(er|ir|re|oir)$"),
      is.na(.data$morph_verbform) | .data$morph_verbform == "Inf",
      !stringr::str_detect(.data$dep_rel, "^(aux|cop)($|:)"),
      .data$pos %in% c("VERB", "AUX") | .data$morph_verbform %in% "Inf" |
        .data$match_token %in% known | .data$match_lemma %in% known
    )
  markers <- tokens %>%
    dplyr::filter((.data$pos == "DET" & stringr::str_detect(.data$dep_rel, "^det($|:)")) |
                    (.data$pos == "ADJ" & .data$dep_rel == "amod"))
  genitive_heads <- heads(tokens[tokens$pos == "ADP" & tokens$dep_rel == "case" &
                                   tokens$match_token %in% c("de", "d'", "du", "des"), ])
  objects <- tokens %>%
    dplyr::filter(stringr::str_detect(.data$dep_rel, "^(obj|iobj)($|:)")) %>%
    dplyr::anti_join(genitive_heads, by = keys)
  verbal <- tokens %>%
    dplyr::filter((.data$pos == "ADV" & .data$dep_rel == "advmod" &
                     !.data$match_token %in% c("ne", "n'")) |
                    (.data$pos %in% c("VERB", "AUX") &
                       stringr::str_detect(.data$dep_rel, "^(aux|ccomp|xcomp)($|:)")) |
                    stringr::str_detect(.data$dep_rel, "^(nsubj|csubj)($|:)"))
  excluded <- heads(dplyr::bind_rows(objects, verbal))
  eligible <- dplyr::anti_join(candidates, excluded, by = keys)
  nominal <- dplyr::semi_join(eligible, heads(markers), by = keys)
  coordinated <- eligible %>%
    dplyr::filter(.data$dep_rel == "conj") %>%
    dplyr::semi_join(nominal, by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int"))
  subjects <- candidates %>%
    dplyr::filter(stringr::str_detect(.data$dep_rel, "^(nsubj|csubj)($|:)"),
                  !is.na(.data$head_token_id_int),
                  .data$token_id_int < .data$head_token_id_int)
  rows <- dplyr::bind_rows(nominal, coordinated, subjects) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int)
  dplyr::semi_join(tokens, rows, by = keys)
}

#' Gérondifs mistagged as nouns
#'
#' UDPipe French-GSD often tags the participle of `en` + present participle as
#' a NOUN with lemma = form (`Il marche en chantant` -> `chantant` NOUN obl).
#' These rows are recovered for f_25 and removed from f_16. Candidates follow
#' `en`, end in `-ant`, attach as `obl`/`advcl`/`nmod`, have no determiner, and
#' are not in `gerondif_noun_stoplist` (`en enfant`, `en tant que`).
#'
#' @param tokens Prepared tokens with context columns.
#' @param word_lists_lookup Word lists.
#' @return Token rows.
#' @keywords internal
gerondif_noun_rows_fr <- function(tokens, word_lists_lookup) {
  stoplist <- normalize_terms(get_word_list(word_lists_lookup, "gerondif_noun_stoplist"))
  determined <- tokens %>%
    dplyr::filter(stringr::str_detect(.data$dep_rel, "^det($|:)")) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, token_id_int = .data$head_token_id_int)
  tokens %>%
    dplyr::filter(
      .data$pos == "NOUN",
      .data$prev_token %in% "en",
      stringr::str_detect(.data$match_token, "ant$"),
      stringr::str_detect(.data$dep_rel, "^(obl|advcl|nmod)($|:)"),
      !.data$match_token %in% stoplist,
      !.data$match_lemma %in% stoplist
    ) %>%
    dplyr::anti_join(determined, by = c("doc_id", "sentence_id", "token_id_int"))
}

#' Lexical membership features (French)
#'
#' f_10: standalone demonstrative pronouns (`demonstrative_matchlist`: ce/c',
#' ça, cela, ceci, celui...) tagged PRON with `PronType=Dem` or no PronType,
#' excluding f_09 occurrences (c'est important de...) and the ce of
#' ce que/ce qui/ce dont relatives (D-50). Clefts (c'est X qui) remain f_10.
#' f_14: nouns with a nominalization suffix, minus a stoplist. f_15:
#' infinitives functioning as nouns (`nominal_infinitive_rows_fr()`); the
#' gérondif is adverbial and belongs to f_25. f_16: other non-hyphenated
#' nouns, excluding gérondifs mistagged as nouns. f_14, f_15 and f_16 are
#' disjoint by occurrence. f_51: demonstrative determiners.
#'
#' @param tokens Prepared tokens with context columns.
#' @param doc_ids Document IDs.
#' @param word_lists_lookup Word lists.
#' @param pronoun_it_evidence f_09 evidence excluded from f_10.
#' @param nominal_infinitives Rows from `nominal_infinitive_rows_fr()`.
#' @param gerondif_nouns Rows from `gerondif_noun_rows_fr()`.
#' @return Block result with f_10, f_14-f_16 and f_51 evidence.
#' @keywords internal
block_lexical_membership_fr <- function(tokens, doc_ids, word_lists_lookup,
                                        pronoun_it_evidence = empty_evidence_tibble(),
                                        nominal_infinitives = tokens[0, ],
                                        gerondif_nouns = tokens[0, ]) {
  keys <- c("doc_id", "sentence_id", "token_id_int")
  demonstratives <- normalize_terms(get_word_list(word_lists_lookup, "demonstrative_matchlist"))
  # The ce of ce que/ce qui/ce dont belongs to a free or sentence relative
  # (f_23/f_34), like English what/which, not a demonstrative (D-50).
  relative_ce <- tokens$match_token %in% c("ce", "c'") &
    tokens$next_token %in% c("que", "qu'", "qui", "dont")
  f10 <- tokens[tokens$match_token %in% demonstratives & tokens$pos == "PRON" & !relative_ce &
                  (tokens$morph_prontype %in% "Dem" | is.na(tokens$morph_prontype)), ] %>%
    dplyr::anti_join(pronoun_it_evidence,
                     by = c("doc_id", "sentence_id", "token_id_int" = "token_id")) %>%
    count_feature_traced("f_10_demonstrative_pronoun")

  suffixes <- get_word_list(word_lists_lookup, "nominalization_suffixes")
  suffix_pattern <- if (length(suffixes)) {
    paste0("(", paste(stringr::str_replace_all(suffixes, "([\\W])", "\\\\\\1"),
                      collapse = "|"), ")$")
  } else {
    "^$"
  }
  stoplist <- normalize_terms(get_word_list(word_lists_lookup, "nominalization_stoplist"))
  f14_rows <- tokens[tokens$pos == "NOUN" & stringr::str_detect(tokens$lemma, suffix_pattern) &
                       !tokens$match_lemma %in% stoplist, ] %>%
    dplyr::anti_join(nominal_infinitives, by = keys)
  f14 <- count_feature_traced(f14_rows, "f_14_nominalizations")

  f15_rows <- nominal_infinitives
  f15 <- count_feature_traced(f15_rows, "f_15_gerunds")

  f16 <- tokens[tokens$pos %in% c("NOUN", "PROPN") & !stringr::str_detect(tokens$token, "-"), ] %>%
    dplyr::anti_join(f14_rows, by = keys) %>%
    dplyr::anti_join(f15_rows, by = keys) %>%
    dplyr::anti_join(gerondif_nouns, by = keys) %>%
    count_feature_traced("f_16_other_nouns")

  f51 <- tokens[tokens$match_token %in% c("ce", "cet", "cette", "ces") &
                  tokens$dep_rel == "det" & tokens$pos %in% c("DET", "PRON"), ] %>%
    count_feature_traced("f_51_demonstratives")

  make_block_result(doc_ids, bind_evidence(f10$evidence, f14$evidence, f15$evidence,
                                           f16$evidence, f51$evidence))
}

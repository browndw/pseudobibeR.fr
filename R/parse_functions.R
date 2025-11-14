#' Extract Biber features from a document parsed and annotated by spacyr or udpipe
#'
#' Takes data that has been part-of-speech tagged and dependency parsed and
#' extracts counts of features that have been used in Douglas Biber's research
#' since the late 1980s.
#'
#' Refer to `spacyr::spacy_parse()` or `udpipe::udpipe_annotate()` for details
#' on parsing texts. These must be configured to do part-of-speech and
#' dependency parsing. For `spacyr::spacy_parse()`, use the `dependency = TRUE`,
#' `tag = TRUE`, and `pos = TRUE` arguments; for `udpipe::udpipe_annotate()`,
#' set the `tagger` and `parser` arguments to `"default"`.
#'
#' Feature extraction relies on the `dict` and `word_lists` datasets to match specific features;
#' see their documentation
#' and values for details on the exact patterns and words matched by each. The
#' function identifies other features based on local cues, which are
#' approximations. Because they rely on probabilistic taggers provided by spaCy
#' or udpipe, the accuracy of the resulting counts are dependent on the accuracy
#' of those models. Thus, texts with irregular spellings, non-normative
#' punctuation, etc. will likely produce unreliable outputs, unless taggers are
#' tuned specifically for those purposes.
#'
#' The following feature families are detected. Each description highlights
#' typical French cues for the feature. The repository file
#' `biber_features_equivalents_french.csv` and the packaged dataset
#' `french_examples` list the exact examples that guide every heuristic.
#'
#' ## Tense and aspect markers
#' 
#' \describe{
#' \item{f_01_past_tense}{Verbs in a past-tense form (e.g., \emph{il parlait}).}
#' \item{f_02_perfect_aspect}{Perfect aspect built with auxiliaries such as \emph{avoir} or \emph{etre} plus a past participle (e.g., \emph{j'ai fini}).}
#' \item{f_03_present_tense}{Finite verbs in the present tense (e.g., \emph{nous parlons}).}
#' }
#'
#' ## Place and time adverbials
#'
#' \describe{
#' \item{f_04_place_adverbials}{Adverbs of place (e.g., \emph{dehors}, \emph{ailleurs}).}
#' \item{f_05_time_adverbials}{Adverbs of time (e.g., \emph{bientot}, \emph{hier}).}
#' }
#'
#' ## Pronouns and pro-verbs
#'
#' \describe{
#' \item{f_06_first_person_pronouns}{First-person pronouns (e.g., \emph{je}, \emph{nous}).}
#' \item{f_07_second_person_pronouns}{Second-person pronouns (e.g., \emph{tu}, \emph{vous}).}
#' \item{f_08_third_person_pronouns}{Third-person personal pronouns other than expletives (e.g., \emph{il}, \emph{elle}, \emph{ils}).}
#' \item{f_09_pronoun_it}{Impersonal or expletive subjects (e.g., \emph{il} in \emph{il faut} or \emph{il pleut}).}
#' \item{f_10_demonstrative_pronoun}{Standalone demonstratives that replace a noun (e.g., \emph{celui-ci}).}
#' \item{f_11_indefinite_pronouns}{Indefinite pronouns (e.g., \emph{quelqu'un}, \emph{personne}).}
#' \item{f_12_proverb_do}{Uses of \emph{faire} as a pro-verb (e.g., \emph{je le fais}).}
#' }
#'
#' ## Questions
#'
#' \describe{
#' \item{f_13_wh_question}{Direct interrogatives beginning with \emph{qui}, \emph{quoi}, \emph{ou}, etc.}
#' }
#'
#' ## Nominal forms
#'
#' \describe{
#' \item{f_14_nominalizations}{Nominalisations such as words ending in \emph{-tion}, \emph{-ment}, \emph{-age}.}
#' \item{f_15_gerunds}{Gerunds or \emph{groupe infinitif} headed by \emph{en} (e.g., \emph{en travaillant}).}
#' \item{f_16_other_nouns}{All other noun tokens once nominalisations and gerunds are excluded.}
#' }
#'
#' ## Passives
#'
#' \describe{
#' \item{f_17_agentless_passives}{Passive clauses without an explicit agent (e.g., \emph{La decision a ete prise}).}
#' \item{f_18_by_passives}{Passive clauses with an expressed agent (e.g., \emph{La decision a ete prise par le directeur}).}
#' }
#'
#' ## Stative forms
#'
#' \describe{
#' \item{f_19_be_main_verb}{\emph{etre} used as a lexical verb.}
#' \item{f_20_existential_there}{Existential constructions (e.g., \emph{il y a}).}
#' }
#'
#' ## Subordination features
#'
#' \describe{
#' \item{f_21_that_verb_comp}{Verb complements introduced by \emph{que} (e.g., \emph{je pense qu'il vient}).}
#' \item{f_22_that_adj_comp}{Adjectival complements introduced by \emph{que} (e.g., \emph{je suis heureux que tu sois la}).}
#' \item{f_23_wh_clause}{\emph{wh}-clauses such as \emph{ce que}, \emph{ce qui}.}
#' \item{f_24_infinitives}{Infinitival clauses.}
#' \item{f_25_present_participle}{Adverbial clauses headed by a present participle (e.g., \emph{en chantant}).}
#' \item{f_26_past_participle}{Adverbial clauses headed by a past participle (e.g., \emph{construit en un an}).}
#' \item{f_27_past_participle_whiz}{Post-nominal reduced relatives with past participles (e.g., \emph{la maison construite en 2020}).}
#' \item{f_28_present_participle_whiz}{Post-nominal reduced relatives with present participles (e.g., \emph{les enfants jouant dans le parc}).}
#' \item{f_29_that_subj}{Subject relatives introduced by \emph{qui} following a noun.}
#' \item{f_30_that_obj}{Object relatives introduced by \emph{que}.}
#' \item{f_31_wh_subj}{Subject relatives with \emph{lequel/laquelle/...}.}
#' \item{f_32_wh_obj}{Object relatives with \emph{dont}, \emph{lequel/laquelle/...}.}
#' \item{f_33_pied_piping}{Pied-piping relatives including the preposition (e.g., \emph{la maniere dans laquelle il procede}).}
#' \item{f_34_sentence_relatives}{Sentence relatives such as \emph{ce qui} following a clause (e.g., \emph{Il a reussi, ce qui est remarquable}).}
#' \item{f_35_because}{Causal subordinates (e.g., \emph{parce que}, \emph{car}).}
#' \item{f_36_though}{Concessive subordinates (e.g., \emph{bien que}, \emph{meme si}).}
#' \item{f_37_if}{Conditional subordinates (e.g., \emph{si}, \emph{a moins que}).}
#' \item{f_38_other_adv_sub}{Other adverbial subordinators (e.g., \emph{tandis que}, \emph{lorsque}).}
#' }
#'
#' ## Prepositional phrases, adjectives, and adverbs
#'
#' \describe{
#' \item{f_39_prepositions}{Prepositions functioning as case markers.}
#' \item{f_40_adj_attr}{Attributive adjectives placed before or alongside a noun (e.g., \emph{une grande maison}).}
#' \item{f_41_adj_pred}{Predicative adjectives following a copula (e.g., \emph{la maison est grande}).}
#' \item{f_42_adverbs}{Adverbs not counted elsewhere (e.g., \emph{vraiment}).}
#' }
#'
#' ## Lexical specificity
#'
#' \describe{
#' \item{f_43_type_token}{Type-token ratio computed with the requested measure.}
#' \item{f_44_mean_word_length}{Average token length (letters only).}
#' }
#'
#' ## Lexical classes
#'
#' \describe{
#' \item{f_45_conjuncts}{Sentence connectors such as \emph{cependant}, \emph{toutefois}.}
#' \item{f_46_downtoners}{Downtoners (e.g., \emph{a peine}, \emph{presque}).}
#' \item{f_47_hedges}{Hedges or approximators (e.g., \emph{environ}, \emph{quelque peu}).}
#' \item{f_48_amplifiers}{Amplifiers (e.g., \emph{tres}, \emph{particulierement}).}
#' \item{f_49_emphatics}{Emphatic markers (e.g., \emph{vraiment}, \emph{bien}).}
#' \item{f_50_discourse_particles}{Discourse particles often sentence-initial (e.g., \emph{eh bien}, \emph{bon}).}
#' \item{f_51_demonstratives}{Demonstrative determiners (e.g., \emph{ce}, \emph{cet}, \emph{cette}, \emph{ces}).}
#' }
#'
#' ## Modals
#'
#' \describe{
#' \item{f_52_modal_possibility}{Verbs expressing possibility (e.g., \emph{pouvoir}, \emph{il se peut}).}
#' \item{f_53_modal_necessity}{Necessity modals (e.g., \emph{devoir}, \emph{falloir}).}
#' \item{f_54_modal_predictive}{Predictive markers (e.g., \emph{aller} + infinitive, \emph{devoir} au futur).}
#' }
#'
#' ## Specialized verb classes
#'
#' \describe{
#' \item{f_55_verb_public}{Public communication verbs (e.g., \emph{declarer}, \emph{annoncer}).}
#' \item{f_56_verb_private}{Private cognition verbs (e.g., \emph{penser}, \emph{croire}).}
#' \item{f_57_verb_suasive}{Suasive verbs (e.g., \emph{ordonner}, \emph{proposer}).}
#' \item{f_58_verb_seem}{Verbs of seeming (e.g., \emph{sembler}, \emph{paraitre}).}
#' }
#'
#' ## Reduced forms and dispreferred structures
#'
#' \describe{
#' \item{f_59_contractions}{Contractions (e.g., \emph{j'ai}, \emph{c'etait}).}
#' \item{f_60_that_deletion}{Subordinator \emph{que} omission in complement clauses.}
#' \item{f_61_stranded_preposition}{Stranded prepositions (e.g., \emph{la personne que je parlais avec}).}
#' \item{f_62_split_infinitive}{Split infinitives (e.g., \emph{de vraiment comprendre}).}
#' \item{f_63_split_auxiliary}{Auxiliary and participle separated by an adverb (e.g., \emph{a probablement ete vu}).}
#' }
#'
#' ## Co-ordination
#'
#' \describe{
#' \item{f_64_phrasal_coordination}{Coordination of like phrases (e.g., \emph{pommes et oranges}).}
#' \item{f_65_clausal_coordination}{Coordination of independent clauses (e.g., sentence-initial \emph{et}).}
#' }
#'
#' ## Negation
#'
#' \describe{
#' \item{f_66_neg_synthetic}{Synthetic negation with determiners or adjectives (e.g., \emph{aucun}, \emph{nul}).}
#' \item{f_67_neg_analytic}{Analytic negation with \emph{ne} ... adverb (e.g., \emph{ne ... pas}, \emph{ne ... jamais}).}
#' }
#'
#' @param tokens A dataset of tokens created by `spacyr::spacy_parse()` or
#'   `udpipe::udpipe_annotate()`
#' @param measure Measure to use for type-token ratio. Passed to
#'   `quanteda.textstats::textstat_lexdiv()` to calculate the statistic. Can be
#'   the Moving Average Type-Token Ratio (MATTR), ordinary Type-Token Ratio
#'   (TTR), corrected TTR (CTTR), Mean Segmental Type-Token Ratio (MSTTR), or
#'   `"none"` to skip calculating a type-token ratio. If a statistic is chosen
#'   but there are fewer than 200 token in the smallest document, the TTR is
#'   used instead.
#' @param normalize If `TRUE`, count features are normalized to the rate per
#'   1,000 tokens.
#' @return A `data.frame` of features containing one row per document and one
#'   column per feature. If `normalize` is `TRUE`, count features are normalized
#'   to the rate per 1,000 tokens.
#' @references Biber, Douglas (1985). "Investigating macroscopic textual
#' variation through multifeature/multidimensional analyses." *Linguistics*
#' 23(2), 337-360. \doi{10.1515/ling.1985.23.2.337}
#'
#' Biber, Douglas (1988). *Variation across Speech and Writing*.
#'   Cambridge University Press.
#'
#' Biber, Douglas (1995). *Dimensions of Register Variation: A Cross-Linguistic
#' Comparison.* Cambridge University Press.
#'
#' Covington, M. A., & McFall, J. D. (2010). Cutting the Gordian Knot: The
#' Moving-Average Type-Token Ratio (MATTR). *Journal of Quantitative
#' Linguistics*, 17(2), 94-100. \doi{10.1080/09296171003643098}
#' @examples
#' # Parse the example documents provided with the package
#' biber(udpipe_samples)
#'
#' biber(spacy_samples)
#' @importFrom magrittr %>%
#' @seealso [dict], [word_lists]
#' @export
biber <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                  normalize = TRUE) {
  UseMethod("biber")
}

#' @rdname biber
#' @export
biber.spacyr_parsed <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                                normalize = TRUE) {
  if ("dep_rel" %in% colnames(tokens) == F) stop("be sure to set 'dependency = T' when using spacy_parse")
  if ("tag" %in% colnames(tokens) == F) stop("be sure to set 'tag = T' when using spacy_parse")
  if ("pos" %in% colnames(tokens) == F) stop("be sure to set 'pos = T' when using spacy_parse")

  measure <- match.arg(measure)

  return(parse_biber_features(tokens, measure, normalize, "spacy"))
}

#' @rdname biber
#' @export
biber.udpipe_connlu <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                                normalize = TRUE) {

  # implicitly depends on the data.frame method for udpipe_connlu from
  # udpipe, so we have to put udpipe in Suggests and try to load it
  if (!requireNamespace("udpipe", quietly = TRUE)) {
    stop("udpipe package must be installed to extract features from udpipe-tagged text")
  }

  udpipe_tks <- as.data.frame(tokens, stringsAsFactors = FALSE)

  if ("dep_rel" %in% colnames(udpipe_tks) == F) stop("Be sure to set parser = 'default'")
  if ("xpos" %in% colnames(udpipe_tks) == F) stop("Be sure to set tagger = 'default'")
  if ("upos" %in% colnames(udpipe_tks) == F) stop("Be sure to set tagger = 'default'")

  measure <- match.arg(measure)

  udpipe_tks <- udpipe_tks %>%
    dplyr::select("doc_id", "sentence_id", "token_id", "token", "lemma", "upos",
                  "xpos", "feats", "head_token_id", "dep_rel") %>%
    dplyr::rename(pos = "upos", tag = "xpos") %>%
    dplyr::mutate(tag = dplyr::if_else(is.na(.data$tag) | .data$tag == "", .data$pos, .data$tag))

  udpipe_tks <- structure(udpipe_tks, class = c("spacyr_parsed", "data.frame"))

  return(parse_biber_features(udpipe_tks, measure, normalize, "udpipe"))
}

#' @importFrom rlang .data :=
#' @importFrom utils tail
parse_biber_features <- function(tokens, measure, normalize, engine = c("spacy", "udpipe")) {
  engine <- match.arg(engine)

  dict_lookup <- dict
  word_lists_lookup <- word_lists

  get_word_list <- function(name, default = character()) {
    values <- word_lists_lookup[[name]]
    if (is.null(values)) {
      return(default)
    }
    values
  }

  normalize_terms <- function(values) {
    stringr::str_replace_all(values, "_", " ")
  }

  dictionary_to_lemmas <- function(feature) {
    if (!feature %in% names(dict_lookup)) {
      return(character())
    }
    raw_terms <- as.list(dict_lookup[feature])
    if (length(raw_terms) == 0) {
      return(character())
    }
    raw_terms <- unname(unlist(raw_terms, recursive = TRUE, use.names = FALSE))
    if (is.null(raw_terms) || length(raw_terms) == 0) {
      return(character())
    }
    cleaned <- stringr::str_replace_all(raw_terms, "_", " ")
    lemmas <- purrr::map_chr(cleaned, function(term) {
      parts <- stringr::str_split(term, "\\s+")[[1]]
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0) {
        return(NA_character_)
      }
      stringr::str_to_lower(tail(parts, 1))
    })
    unique(stats::na.omit(lemmas))
  }

  df <- list()

  tokens <- tokens %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(token = stringr::str_to_lower(.data$token)) %>%
    dplyr::mutate(pos = dplyr::if_else(.data$token == "\n", "PUNCT", .data$pos)) %>%
    dplyr::filter(.data$pos != "SPACE")

  if ("morph" %in% colnames(tokens)) {
    tokens <- tokens %>%
      dplyr::mutate(morph = purrr::map_chr(.data$morph, function(x) {
        if (inherits(x, "python.builtin.object")) {
          if (requireNamespace("reticulate", quietly = TRUE)) {
            value <- reticulate::py_to_r(x)
            if (is.null(value) || length(value) == 0) {
              return("")
            }
            value <- as.character(value)
            if (length(value) == 1) {
              return(value)
            }
            return(paste(value, collapse = "|"))
          }
          return(as.character(x))
        }
        if (is.null(x) || length(x) == 0) {
          return("")
        }
        if (length(x) == 1) {
          return(as.character(x))
        }
        paste(as.character(x), collapse = "|")
      })) %>%
      dplyr::mutate(morph = dplyr::na_if(.data$morph, ""))

    if ("feats" %in% colnames(tokens)) {
      tokens <- tokens %>%
        dplyr::mutate(feats = dplyr::coalesce(.data$feats, .data$morph))
    } else {
      tokens <- tokens %>%
        dplyr::mutate(feats = .data$morph)
    }
  }

  if (!"feats" %in% colnames(tokens)) {
    tokens <- dplyr::mutate(tokens, feats = NA_character_)
  }

  tokens <- tokens %>%
    dplyr::mutate(
      token_id_int = suppressWarnings(as.integer(.data$token_id)),
      head_token_id_int = suppressWarnings(as.integer(.data$head_token_id)),
      morph_tense = extract_morph_value(.data$feats, "Tense"),
      morph_verbform = extract_morph_value(.data$feats, "VerbForm"),
      morph_mood = extract_morph_value(.data$feats, "Mood"),
      morph_prontype = extract_morph_value(.data$feats, "PronType"),
      morph_voice = extract_morph_value(.data$feats, "Voice"),
      morph_number = extract_morph_value(.data$feats, "Number"),
      morph_person = extract_morph_value(.data$feats, "Person")
    )
  
  tokens <- tokens %>%
    dplyr::arrange(.data$doc_id, .data$sentence_id, .data$token_id_int)

  doc_ids <- tokens %>% dplyr::distinct(.data$doc_id)

  proverb_pronouns <- normalize_terms(
  get_word_list("proverb_object_pronouns", c("le", "la", "les", "ce", "cela", "\u00e7a"))
  )

  neg_synthetic_terms <- normalize_terms(
    get_word_list("neg_synthetic_determiners", c("aucun", "aucune", "nul", "nulle", "ni", "sans"))
  )

  negation_particle_terms <- normalize_terms(get_word_list("negation_particles", c("ne")))
  negation_part_lemmas <- unique(c(negation_particle_terms, "n'", "n\u2019"))

  negation_adverbs <- normalize_terms(
    get_word_list(
      "neg_analytic_adverbs",
  c("pas", "plus", "jamais", "gu\u00e8re", "point", "personne", "rien", "nullement")
    )
  )

  impersonal_verbs <- normalize_terms(
    get_word_list(
      "impersonal_verbs",
  c("pleuvoir", "neiger", "bruiner", "gr\u00ealer", "venter", "tonner", "geler")
    )
  )

  weather_lemmas <- unique(c(impersonal_verbs, "bruiner", "tonner"))
  raising_verbs <- c("sembler", "para\u00eetre", "demeurer", "rester", "suffire", "convenir")
  wh_question_lemmas <- c(
  "qui", "que", "quoi", "o\u00f9", "quand", "comment", "pourquoi",
    "lequel", "laquelle", "lesquels", "lesquelles",
    "quel", "quelle", "quels", "quelles", "combien"
  )
  relative_pronoun_candidates <- c(
  "qui", "que", "quoi", "o\u00f9", "dont",
    "lequel", "laquelle", "lesquel", "lesquelle", "lesquels", "lesquelles",
    "auquel", "auxquels", "auxquelles",
    "duquel", "desquels", "desquelles"
  )

  de_markers <- tokens %>%
    dplyr::filter(
      .data$lemma == "de",
      .data$dep_rel %in% c("mark", "advmod", "case"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id,
      .data$sentence_id,
      head_token_id_int = .data$head_token_id_int,
      has_de_marker = TRUE
    ) %>%
    dplyr::distinct()

  que_markers <- tokens %>%
    dplyr::filter(
  .data$lemma %in% c("que", "qu'", "qu\u2019"),
      .data$dep_rel %in% c("mark", "expl", "obj"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id,
      .data$sentence_id,
      head_token_id_int = .data$head_token_id_int,
      has_que_marker = TRUE
    ) %>%
    dplyr::distinct()

  clause_complements <- tokens %>%
    dplyr::filter(
      .data$dep_rel %in% c("ccomp", "xcomp", "csubj", "advcl"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id,
      .data$sentence_id,
      head_token_id_int = .data$head_token_id_int,
      has_clause_comp = TRUE
    ) %>%
    dplyr::distinct()

  lexical_text <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(text = paste(.data$token, collapse = " "), .groups = "drop")

  if (nrow(lexical_text) > 0) {
  biber_tks <- quanteda::tokens(lexical_text$text, what = "word", remove_punct = FALSE)
    names(biber_tks) <- lexical_text$doc_id

    multiword_patterns <- get_word_list("multiword_patterns")
    if (length(multiword_patterns) > 0) {
      multi_phrases <- quanteda::phrase(multiword_patterns)
      biber_tks <- quanteda::tokens_compound(biber_tks, pattern = multi_phrases)
    }

    biber_tks <- quanteda::tokens_tolower(biber_tks)

    biber_1 <- quanteda::tokens_lookup(biber_tks, dictionary = dict_lookup, nomatch = NULL) %>%
      quanteda::dfm() %>%
      quanteda::convert(to = "data.frame") %>%
      dplyr::as_tibble()

    if ("document" %in% colnames(biber_1)) {
      biber_1 <- biber_1 %>% dplyr::rename(doc_id = "document")
    }
  } else {
    biber_1 <- tibble::tibble(doc_id = character())
  }

  df[["f_01_past_tense"]] <- tokens %>%
    dplyr::filter(
      .data$pos == "VERB",
      .data$morph_tense %in% c("Past", "Imp", "Pqp"),
      is.na(.data$morph_verbform) | .data$morph_verbform %in% c("Fin")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_01_past_tense = "n")

  df[["f_03_present_tense"]] <- tokens %>%
    dplyr::filter(
      .data$pos == "VERB",
      .data$morph_tense %in% c("Pres"),
      is.na(.data$morph_verbform) | .data$morph_verbform %in% c("Fin")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_03_present_tense = "n")

  head_lookup <- tokens %>%
    dplyr::select(
      "doc_id", "sentence_id", "token_id_int",
      head_pos = "pos",
      head_lemma = "lemma",
      head_token = "token",
      head_feats = "feats",
      head_morph_verbform = "morph_verbform",
      head_morph_voice = "morph_voice",
      head_morph_tense = "morph_tense"
    )

  perfect_candidates <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("AUX", "VERB"),
  .data$lemma %in% c("avoir", "\u00eatre"),
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^(aux|cop)"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::mutate(
      head_is_participle = (
        .data$head_pos %in% c("VERB", "AUX") &
          .data$head_morph_verbform == "Part" &
          dplyr::coalesce(.data$head_morph_voice, "") != "Pass"
      ) |
        (
          .data$head_pos %in% c("ADJ", "NOUN") &
            stringr::str_detect(
              dplyr::coalesce(.data$head_feats, ""),
              "VerbForm=Part"
            ) &
            dplyr::coalesce(.data$head_morph_voice, "") != "Pass"
        ) |
        (
          .data$head_pos == "NOUN" &
            stringr::str_detect(
              stringr::str_to_lower(dplyr::coalesce(.data$head_token, "")),
              "(\u00e9|\u00e9e|\u00e9s|\u00e9es|i|ie|is|ies|u|ue|us|ues)$"
            )
        )
    ) %>%
    dplyr::filter(.data$head_is_participle) %>%
    dplyr::distinct(.data$doc_id, .data$head_token_id_int, .keep_all = TRUE)

  df[["f_02_perfect_aspect"]] <- perfect_candidates %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_02_perfect_aspect = "n")

  pronoun_it_candidates <- tokens %>%
    dplyr::filter(
      .data$pos == "PRON",
  .data$lemma %in% c("il", "ce", "cela", "\u00e7a"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::left_join(de_markers, by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::left_join(que_markers, by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::left_join(clause_complements, by = c("doc_id", "sentence_id", "head_token_id_int")) %>%
    dplyr::mutate(
      has_de_marker = dplyr::coalesce(.data$has_de_marker, FALSE),
      has_que_marker = dplyr::coalesce(.data$has_que_marker, FALSE),
      has_clause_comp = dplyr::coalesce(.data$has_clause_comp, FALSE),
      is_weather = .data$head_lemma %in% weather_lemmas,
      is_raising_verb = .data$head_lemma %in% raising_verbs,
      has_control_marker = .data$has_de_marker | .data$has_que_marker | .data$has_clause_comp
    ) %>%
    dplyr::filter(
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^expl") |
        (
          stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^nsubj") &
          (
            .data$is_weather |
            (
              .data$head_pos == "ADJ" &
                .data$has_control_marker
            ) |
            (
              .data$head_pos %in% c("VERB", "AUX") &
                (.data$is_raising_verb | .data$has_control_marker)
            )
          )
        )
    )

  df[["f_09_pronoun_it"]] <- pronoun_it_candidates %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_09_pronoun_it = "n")

  proverb_objects <- tokens %>%
    dplyr::filter(
      .data$pos == "PRON",
      .data$lemma %in% proverb_pronouns,
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^obj"),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id,
      .data$sentence_id,
      head_token_id_int = .data$head_token_id_int,
      has_proverb_object = TRUE
    ) %>%
    dplyr::distinct()

  proverb_candidates <- tokens %>%
    dplyr::filter(
      .data$lemma == "faire",
      .data$pos %in% c("VERB", "AUX"),
      !stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^aux"),
      !is.na(.data$token_id_int)
    )

  f12_counts <- proverb_candidates %>%
    dplyr::left_join(
      proverb_objects,
      by = c("doc_id", "sentence_id", "token_id_int" = "head_token_id_int")
    ) %>%
    dplyr::filter(.data$has_proverb_object) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_12_proverb_do = "n")

  df[["f_12_proverb_do"]] <- doc_ids %>%
    dplyr::left_join(f12_counts, by = "doc_id") %>%
    dplyr::mutate(f_12_proverb_do = dplyr::coalesce(.data$f_12_proverb_do, 0L))

  df[["f_10_demonstrative_pronoun"]] <- tokens %>%
    dplyr::filter(
      .data$token %in% get_word_list("pronoun_matchlist"),
      .data$morph_prontype == "Dem",
      .data$pos == "PRON"
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_10_demonstrative_pronoun = "n")

  question_sentences <- tokens %>%
    dplyr::filter(.data$token == "?") %>%
    dplyr::transmute(
      .data$doc_id,
      .data$sentence_id,
      has_question = TRUE
    ) %>%
    dplyr::distinct()

  df[["f_13_wh_question"]] <- tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_question_lemmas,
      .data$pos %in% c("ADV", "PRON", "DET", "ADJ")
    ) %>%
    dplyr::left_join(
      question_sentences,
      by = c("doc_id", "sentence_id")
    ) %>%
    dplyr::filter(!is.na(.data$has_question)) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_13_wh_question = "n")

  nominalization_suffixes <- get_word_list(
    "nominalization_suffixes",
  c("tion", "sion", "ment", "age", "ure", "ance", "ence", "esse", "it\u00e9", "isation", "issement")
  )

  nominalization_pattern <- if (length(nominalization_suffixes) > 0) {
    escaped <- stringr::str_replace_all(nominalization_suffixes, "([\\W])", "\\\\\\1")
    paste0("(", paste(escaped, collapse = "|") , ")$")
  } else {
    "^$"
  }

  nominal_stoplist <- normalize_terms(get_word_list("nominalization_stoplist"))

  df[["f_14_nominalizations"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos == "NOUN",
      stringr::str_detect(.data$lemma, nominalization_pattern)
    ) %>%
    dplyr::filter(!.data$lemma %in% nominal_stoplist) %>%
    dplyr::tally() %>%
    dplyr::rename(f_14_nominalizations = "n")

  gerund_stoplist <- normalize_terms(get_word_list("gerund_stoplist"))

  f_15_gerunds <- tokens %>%
    dplyr::filter(
      .data$pos %in% c("VERB", "AUX"),
      !is.na(.data$morph_verbform),
      .data$morph_verbform == "Ger"
    ) %>%
    dplyr::filter(!.data$lemma %in% gerund_stoplist)

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
    f_15_gerunds <- dplyr::bind_rows(f_15_gerunds, fallback_gerunds) %>%
      dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE)
  }

  gerunds_n <- f_15_gerunds %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(.data$pos %in% c("NOUN", "PROPN")) %>%
    dplyr::tally() %>%
    dplyr::rename(gerunds_n = "n")

  df[["f_15_gerunds"]] <- f_15_gerunds %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_15_gerunds = "n")

  df[["f_16_other_nouns"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos == "NOUN" |
        .data$pos == "PROPN"
    ) %>%
    dplyr::filter(
      stringr::str_detect(.data$token, "-") == F
    ) %>%
    dplyr::tally() %>%
    dplyr::left_join(df[["f_14_nominalizations"]], by = "doc_id") %>%
    dplyr::left_join(gerunds_n, by = "doc_id") %>%
    replace_nas() %>%
    dplyr::mutate(n = .data$n - .data$f_14_nominalizations - .data$gerunds_n) %>%
    dplyr::select("doc_id", "n") %>%
    dplyr::rename(f_16_other_nouns = "n")

  passive_rel_values <- if (engine == "spacy") c("auxpass", "aux:pass") else "aux:pass"
  passive_agents <- c("par")

  tokens <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::arrange(.data$sentence_id, .data$token_id_int, .by_group = TRUE) %>%
    dplyr::mutate(
      passive_agent_next2 = dplyr::lead(.data$token %in% passive_agents, 2, default = FALSE),
      passive_agent_next3 = dplyr::lead(.data$token %in% passive_agents, 3, default = FALSE),
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
      ),
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
      ),
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
        ),
      prev_token = dplyr::lag(.data$token),
      prev_lemma = dplyr::lag(.data$lemma),
      prev_pos = dplyr::lag(.data$pos),
      prev2_token = dplyr::lag(.data$token, 2),
      prev2_lemma = dplyr::lag(.data$lemma, 2),
      next_token = dplyr::lead(.data$token),
      next_lemma = dplyr::lead(.data$lemma),
      next_pos = dplyr::lead(.data$pos),
      next_dep_rel = dplyr::lead(.data$dep_rel),
      next2_token = dplyr::lead(.data$token, 2)
    ) %>%
    dplyr::ungroup()

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

  passive_candidates <- tokens %>%
    dplyr::filter(.data$dep_rel %in% passive_rel_values) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    )

  df[["f_17_agentless_passives"]] <- passive_candidates %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$head_pos == "VERB",
      !.data$passive_agent_next2,
      !.data$passive_agent_next3
    ) %>%
    dplyr::distinct(.data$doc_id, .data$head_token_id_int, .keep_all = TRUE) %>%
    dplyr::tally() %>%
    dplyr::rename(f_17_agentless_passives = "n")

  df[["f_18_by_passives"]] <- passive_candidates %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$head_pos == "VERB",
      (.data$passive_agent_next2 | .data$passive_agent_next3)
    ) %>%
    dplyr::distinct(.data$doc_id, .data$head_token_id_int, .keep_all = TRUE) %>%
    dplyr::tally() %>%
    dplyr::rename(f_18_by_passives = "n")

  df[["f_19_be_main_verb"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
  .data$lemma == "\u00eatre",
      stringr::str_detect(.data$dep_rel, "aux") == F
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_19_be_main_verb = "n")

  df[["f_20_existential_there"]] <- tokens %>%
    dplyr::group_by(.data$doc_id, .data$sentence_id) %>%
    dplyr::filter(
      .data$lemma == "avoir",
      .data$pos %in% c("VERB", "AUX"),
      dplyr::lag(.data$lemma == "y", default = FALSE),
      dplyr::lag(.data$lemma == "il", 2, default = FALSE),
      dplyr::lag(.data$pos == "PRON", 2, default = FALSE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .keep_all = TRUE) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_20_existential_there = "n")

  complementizers <- c("que", "qu'", "qu\u2019")
  wh_lemmas <- c(
    "qui", "que", "quoi", "dont",
  "o\u00f9", "ou", "quand", "comment", "pourquoi", "combien",
    "lequel", "laquelle", "lesquels", "lesquelles",
    "auquel", "auxquels", "auxquelles",
    "duquel", "desquels", "desquelles"
  )
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

  df[["f_21_that_verb_comp"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$token %in% complementizers,
      .data$pos == "SCONJ",
      dplyr::lag(.data$pos) %in% c("VERB", "AUX")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_21_that_verb_comp = "n")

  df[["f_22_that_adj_comp"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$token %in% complementizers,
      .data$pos == "SCONJ",
      dplyr::lag(.data$pos) == "ADJ"
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_22_that_adj_comp = "n")

  df[["f_23_wh_clause"]] <- tokens %>%
    dplyr::filter(
      .data$lemma %in% wh_lemmas,
      .data$pos %in% c("PRON", "ADV", "DET", "ADJ", "NOUN", "PROPN"),
      stringr::str_detect(
        dplyr::coalesce(.data$dep_rel, ""),
        "^(obj|obl|nsubj|iobj|expl|mark)"
      ),
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      head_lookup,
      by = c("doc_id", "sentence_id", "head_token_id_int" = "token_id_int")
    ) %>%
    dplyr::filter(.data$head_pos %in% c("VERB", "AUX", "ADJ")) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_23_wh_clause = "n")

  df[["f_24_infinitives"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(.data$is_infinitive) %>%
    dplyr::tally() %>%
    dplyr::rename(f_24_infinitives = "n")

  df[["f_25_present_participle"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
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
    dplyr::tally() %>%
    dplyr::rename(f_25_present_participle = "n")

  df[["f_26_past_participle"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
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
    dplyr::tally() %>%
    dplyr::rename(f_26_past_participle = "n")

  df[["f_27_past_participle_whiz"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_past_participle,
      dplyr::lag(.data$pos == "NOUN"),
      (
        stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^acl") |
          .data$dep_rel == "root"
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_27_past_participle_whiz = "n")

  df[["f_28_present_participle_whiz"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_present_participle,
      dplyr::lag(.data$pos == "NOUN"),
      .data$dep_rel == "acl"
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_28_present_participle_whiz = "n")

  df[["f_29_that_subj"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
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
    dplyr::tally() %>%
    dplyr::rename(f_29_that_subj = "n")

  df[["f_30_that_obj"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_relative_object,
      .data$lemma %in% relative_object_that_lemmas,
      dplyr::lag(.data$pos %in% c("NOUN", "PROPN"), default = FALSE)
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_30_that_obj = "n")

  df[["f_31_wh_subj"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
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
    dplyr::tally() %>%
    dplyr::rename(f_31_wh_subj = "n")

  df[["f_32_wh_obj"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
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
    dplyr::tally() %>%
    dplyr::rename(f_32_wh_obj = "n")

  df[["f_33_pied_piping"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_relative_pronoun,
      .data$lemma %in% pied_piping_relative_lemmas,
      (
        .data$lemma == "dont" |
          dplyr::lag(.data$pos == "ADP", default = FALSE)
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_33_pied_piping = "n")

  sentence_relative_anchors <- c("ce", "cela", "ceci", "celui", "celle", "ceux", "celles")
  parce_follow_tokens <- c("que", "qu'", "qu\u2019")
  because_single_tokens <- c("car", "puisque", "puisqu'", "puisqu\u2019", "comme")

  df[["f_34_sentence_relatives"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$is_relative_pronoun,
      dplyr::lag(.data$token %in% sentence_relative_anchors, default = FALSE),
      (
        dplyr::lag(.data$pos == "PUNCT", 2, default = TRUE) |
          is.na(dplyr::lag(.data$token, 2))
      )
    ) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int, .keep_all = TRUE) %>%
    dplyr::tally() %>%
    dplyr::rename(f_34_sentence_relatives = "n")

  df[["f_35_because"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
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
    dplyr::tally() %>%
    dplyr::rename(f_35_because = "n")

  df[["f_36_though"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      (
        .data$token == "quoique" & .data$pos %in% c("SCONJ")
      ) |
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
    dplyr::tally() %>%
    dplyr::rename(f_36_though = "n")

  df[["f_37_if"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      (
        .data$token == "si" &
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
    dplyr::tally() %>%
    dplyr::rename(f_37_if = "n")

  counted_subordinators <- unique(c(
    complementizers,
    parce_follow_tokens,
    because_single_tokens,
    "parce",
    "quoique",
    "si"
  ))

  df[["f_38_other_adv_sub"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos %in% c("SCONJ", "ADP", "ADV"),
      stringr::str_detect(dplyr::coalesce(.data$dep_rel, ""), "^mark"),
      !.data$token %in% counted_subordinators
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_38_other_adv_sub = "n")

  df[["f_39_prepositions"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos == "ADP",
      dplyr::coalesce(.data$dep_rel, "") %in% c("case", "fixed")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_39_prepositions = "n")

  df[["f_40_adj_attr"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos == "ADJ",
      (
        dplyr::lead(.data$pos == "NOUN") |
          dplyr::lead(.data$pos == "ADJ")  |
          (
            dplyr::lead(.data$token == ",") &
              dplyr::lead(.data$pos == "ADJ", 2)
          )
      )
    ) %>%
    dplyr::filter(stringr::str_detect(.data$token, "-") == F) %>%
    dplyr::tally() %>%
    dplyr::rename(f_40_adj_attr = "n")

  df[["f_41_adj_pred"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos == "ADJ",
      dplyr::lag(.data$pos == "VERB" | .data$pos == "AUX"),
  dplyr::lag(.data$lemma %in% get_word_list("linking_matchlist")),
      dplyr::lead(.data$pos != "NOUN"),
      dplyr::lead(.data$pos != "ADJ"),
      dplyr::lead(.data$pos != "ADV")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_41_adj_pred = "n")

  adverb_exclusions <- unique(c(
    dictionary_to_lemmas("f_46_downtoners"),
    dictionary_to_lemmas("f_47_hedges"),
    dictionary_to_lemmas("f_48_amplifiers"),
    dictionary_to_lemmas("f_49_emphatics"),
    dictionary_to_lemmas("f_50_discourse_particles"),
    negation_adverbs
  ))

  df[["f_42_adverbs"]] <- tokens %>%
    dplyr::filter(
      .data$pos == "ADV",
      !.data$lemma %in% adverb_exclusions
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_42_adverbs = "n")

  df[["f_51_demonstratives"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$token %in% get_word_list("pronoun_matchlist"),
      .data$dep_rel == "det",
      .data$pos %in% c("DET", "PRON")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_51_demonstratives = "n")

  verb_public_lemmas <- dictionary_to_lemmas("f_55_verb_public")
  verb_private_lemmas <- dictionary_to_lemmas("f_56_verb_private")
  verb_suasive_lemmas <- dictionary_to_lemmas("f_57_verb_suasive")
  verb_seem_lemmas <- dictionary_to_lemmas("f_58_verb_seem")

  df[["f_55_verb_public"]] <- tokens %>%
    dplyr::filter(
      .data$lemma %in% verb_public_lemmas,
      .data$pos %in% c("VERB", "AUX")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_55_verb_public = "n")

  df[["f_56_verb_private"]] <- tokens %>%
    dplyr::filter(
      .data$lemma %in% verb_private_lemmas,
      .data$pos %in% c("VERB", "AUX")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_56_verb_private = "n")

  df[["f_57_verb_suasive"]] <- tokens %>%
    dplyr::filter(
      .data$lemma %in% verb_suasive_lemmas,
      .data$pos %in% c("VERB", "AUX")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_57_verb_suasive = "n")

  df[["f_58_verb_seem"]] <- tokens %>%
    dplyr::filter(
      .data$lemma %in% verb_seem_lemmas,
      .data$pos %in% c("VERB", "AUX")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_58_verb_seem = "n")

  clause_marks <- tokens %>%
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

  clause_deletions <- tokens %>%
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
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int)

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

  split_auxiliary_tokens <- aux_dependencies %>%
    dplyr::left_join(
      adverbial_interveners,
      by = c("doc_id", "sentence_id")
    ) %>%
    dplyr::filter(
      !is.na(.data$adv_token_id_int),
      .data$adv_token_id_int > .data$span_min,
      .data$adv_token_id_int < .data$span_max
    ) %>%
    dplyr::distinct(.data$doc_id, .data$token_id_int)

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

  df[["f_60_that_deletion"]] <- clause_deletions %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_60_that_deletion = "n")

  df[["f_61_stranded_preposition"]] <- doc_ids %>%
    dplyr::mutate(f_61_stranded_preposition = 0L)

  df[["f_62_split_infinitive"]] <- doc_ids %>%
    dplyr::mutate(f_62_split_infinitive = 0L)

  df[["f_63_split_auxiliary"]] <- doc_ids %>%
    dplyr::left_join(
      split_auxiliary_tokens %>%
        dplyr::group_by(.data$doc_id) %>%
        dplyr::summarise(f_63_split_auxiliary = dplyr::n(), .groups = "drop"),
      by = "doc_id"
    ) %>%
    dplyr::mutate(f_63_split_auxiliary = dplyr::coalesce(.data$f_63_split_auxiliary, 0L))

  df[["f_64_phrasal_coordination"]] <- cc_tokens %>%
    dplyr::filter(
      .data$conj_dep_rel == "conj",
      .data$conj_pos %in% c("NOUN", "PROPN", "ADJ", "ADV"),
      !is.na(.data$first_conj_pos),
      .data$first_conj_pos == .data$conj_pos,
      !.data$has_subject
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(f_64_phrasal_coordination = dplyr::n(), .groups = "drop")

  df[["f_65_clausal_coordination"]] <- cc_tokens %>%
    dplyr::filter(
      .data$conj_dep_rel == "conj",
      .data$conj_pos %in% c("VERB", "AUX"),
      .data$has_subject,
      is.na(.data$conj_morph_verbform) |
        !.data$conj_morph_verbform %in% c("Inf", "Ger", "Part")
    ) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(f_65_clausal_coordination = dplyr::n(), .groups = "drop")

  f66_counts <- tokens %>%
    dplyr::filter(.data$lemma %in% neg_synthetic_terms) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_66_neg_synthetic = "n")

  df[["f_66_neg_synthetic"]] <- doc_ids %>%
    dplyr::left_join(f66_counts, by = "doc_id") %>%
    dplyr::mutate(f_66_neg_synthetic = dplyr::coalesce(.data$f_66_neg_synthetic, 0L))

  negation_particles_df <- tokens %>%
    dplyr::filter(
      .data$lemma %in% negation_part_lemmas,
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::transmute(
      .data$doc_id,
      .data$sentence_id,
      head_token_id_int = .data$head_token_id_int,
      has_ne = TRUE
    ) %>%
    dplyr::distinct()

  negation_adverbs_df <- tokens %>%
    dplyr::filter(
      .data$lemma %in% negation_adverbs,
      !is.na(.data$head_token_id_int)
    ) %>%
    dplyr::left_join(
      negation_particles_df,
      by = c("doc_id", "sentence_id", "head_token_id_int")
    ) %>%
    dplyr::filter(.data$has_ne) %>%
    dplyr::distinct(.data$doc_id, .data$sentence_id, .data$token_id_int) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_67_neg_analytic = "n")

  df[["f_67_neg_analytic"]] <- doc_ids %>%
    dplyr::left_join(negation_adverbs_df, by = "doc_id") %>%
    dplyr::mutate(f_67_neg_analytic = dplyr::coalesce(.data$f_67_neg_analytic, 0L))

  biber_tks <- biber_tks %>%
    quanteda::tokens_remove("\\d_", valuetype = "regex") %>%
    quanteda::tokens_remove("_punct_", valuetype = "fixed")


  biber_2 <- df %>% purrr::reduce(dplyr::full_join, by = "doc_id")

  biber_counts <- dplyr::full_join(biber_1, biber_2, by = "doc_id") %>%
    replace_nas()

  combine_features <- c(
    "f_51_demonstratives",
    "f_55_verb_public",
    "f_56_verb_private",
    "f_57_verb_suasive",
    "f_58_verb_seem"
  )

  for (feature in combine_features) {
    x_col <- paste0(feature, ".x")
    y_col <- paste0(feature, ".y")
    if (all(c(x_col, y_col) %in% colnames(biber_counts))) {
      x_vals <- dplyr::coalesce(biber_counts[[x_col]], 0L)
      y_vals <- dplyr::coalesce(biber_counts[[y_col]], 0L)
      if (feature == "f_51_demonstratives") {
        biber_counts[[feature]] <- pmax(x_vals, y_vals)
      } else {
        biber_counts[[feature]] <- x_vals + y_vals
      }
      biber_counts <- biber_counts %>%
        dplyr::select(-dplyr::any_of(c(x_col, y_col)))
    }
  }

  if ("f_11_indefinite_pronoun" %in% colnames(biber_counts)) {
    biber_counts <- biber_counts %>%
      dplyr::rename(f_11_indefinite_pronouns = "f_11_indefinite_pronoun")
  }

  if (normalize) {
    tot_counts <- data.frame(tot_counts = quanteda::ntoken(biber_tks)) %>%
      tibble::rownames_to_column("doc_id") %>%
      dplyr::as_tibble()

    biber_counts <- dplyr::full_join(biber_counts, tot_counts, by = "doc_id")

    biber_counts <- normalize_counts(biber_counts)
  }

  if (measure != "none") {
    if (min(quanteda::ntoken(biber_tks)) < 200) {
      message("Setting type-to-token ratio to TTR")
      measure <- "TTR"
    }

    f_43_type_token <- quanteda.textstats::textstat_lexdiv(biber_tks, measure = measure) %>%
      dplyr::rename(doc_id = "document", f_43_type_token := !!measure)

    biber_counts <- dplyr::full_join(biber_counts, f_43_type_token, by = "doc_id")
  }

  f_44_mean_word_length <- tokens %>%
    dplyr::filter(
      stringr::str_detect(.data$token, "^[a-z]+$")
    ) %>%
    dplyr::mutate(mean_word_length = stringr::str_length(.data$token)) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(f_44_mean_word_length = mean(.data$mean_word_length))

  biber_counts <- dplyr::full_join(biber_counts, f_44_mean_word_length, by = "doc_id")

  biber_counts <- biber_counts %>%
    dplyr::select(order(colnames(biber_counts)))

  biber_counts[] <- lapply(biber_counts, as.vector)

  return(biber_counts)
}

#' Normalize to counts per 1,000 tokens
#'
#' @param counts Data frame with numeric columns for counts of token, with one
#'   row per document. Must include a `tot_counts` column with the total number
#'   of tokens per document.
#' @return `counts` data frame with counts normalized to rate per 1,000 tokens,
#'   and `tot_counts` column removed
#' @keywords internal
normalize_counts <- function(counts) {
  counts %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ 1000 * . / tot_counts)) %>%
    dplyr::select(-"tot_counts")
}

#' Replace all NAs with 0
#'
#' @param x Vector potentially containing NAs
#' @keywords internal
replace_nas <- function(x) {
  replace(x, is.na(x), 0)
}

extract_morph_value <- function(feats, key) {
  res <- stringr::str_extract(feats, paste0(key, "=[^|]+"))
  stringr::str_remove(res, paste0(key, "="))
}

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
#' \item{f_02_perfect_aspect}{
#'   Perfect aspect built with auxiliaries such as \emph{avoir} or \emph{etre}
#'   plus a past participle (e.g., \emph{j'ai fini}).
#' }
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
#' \item{f_08_third_person_pronouns}{
#'   Third-person personal pronouns other than expletives
#'   (e.g., \emph{il}, \emph{elle}, \emph{ils}, \emph{lui}, \emph{se}).
#'   Demonstrative \emph{ce}/\emph{ça}, partitive \emph{en} and generic
#'   \emph{on} (like English \emph{one}) are not counted.
#' }
#' \item{f_09_pronoun_it}{Impersonal or expletive subjects (e.g., \emph{il} in \emph{il faut} or \emph{il pleut}; \emph{c'} in \emph{c'est important de lire}).}
#' \item{f_10_demonstrative_pronoun}{Demonstrative pronouns: \emph{ce}/\emph{c'} (\emph{c'est vrai}, like \emph{that's true}), \emph{ça}, \emph{cela}, \emph{ceci}, \emph{celui}/\emph{celle}/\emph{ceux} (e.g., \emph{celui-ci}, \emph{ceux qui}). The \emph{ce} of \emph{ce que}/\emph{ce qui} relatives (f_23, f_34) is not counted; clefts (\emph{c'est Marie qui}) are.}
#' \item{f_11_indefinite_pronouns}{Indefinite pronouns in pronoun use (e.g., \emph{quelqu'un}, \emph{quelque chose}, \emph{tout le monde}, \emph{personne}, \emph{rien}, \emph{tout}, \emph{aucun}). Nouns (\emph{une personne}, \emph{une chose}) and determiners (\emph{quelque temps}, \emph{aucun livre}) are not counted.}
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
#' \item{f_15_gerunds}{Infinitives functioning as nouns: with a determiner, possessive or attributive adjective (e.g., \emph{le savoir}, \emph{son rire}), or heading a subject placed before its verb (e.g., \emph{Fumer tue}). The gérondif (\emph{en travaillant}) is adverbial and counts in f_25.}
#' \item{f_16_other_nouns}{All other noun tokens once nominalisations and gerunds are excluded.}
#' }
#'
#' ## Passives
#'
#' \describe{
#' \item{f_17_agentless_passives}{Passive clauses without an explicit agent (e.g., \emph{La decision a ete prise}).}
#' \item{f_18_by_passives}{
#'   Passive clauses with an expressed agent
#'   (e.g., \emph{La decision a ete prise par le directeur}).
#' }
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
#' \item{f_22_that_adj_comp}{
#'   Adjectival complements introduced by \emph{que}
#'   (e.g., \emph{je suis heureux que tu sois la}).
#' }
#' \item{f_23_wh_clause}{\emph{wh}-clauses such as \emph{ce que}, \emph{ce qui}.}
#' \item{f_24_infinitives}{Infinitival clauses.}
#' \item{f_25_present_participle}{Adverbial clauses headed by a present participle, including the gérondif (e.g., \emph{en chantant}), also when the parser tags the participle as a noun.}
#' \item{f_26_past_participle}{Adverbial clauses headed by a past participle (e.g., \emph{construit en un an}).}
#' \item{f_27_past_participle_whiz}{
#'   Post-nominal reduced relatives with past participles
#'   (e.g., \emph{la maison construite en 2020}).
#' }
#' \item{f_28_present_participle_whiz}{
#'   Post-nominal reduced relatives with present participles
#'   (e.g., \emph{les enfants jouant dans le parc}).
#' }
#' \item{f_29_that_subj}{Subject relatives introduced by \emph{qui} following a noun.}
#' \item{f_30_that_obj}{Object relatives introduced by \emph{que}, excluding the \emph{que} of f_61 relatives.}
#' \item{f_31_wh_subj}{Subject relatives with \emph{lequel/laquelle/...}.}
#' \item{f_32_wh_obj}{Object relatives with \emph{dont}, \emph{lequel/laquelle/...}.}
#' \item{f_33_pied_piping}{
#'   Pied-piping relatives including the preposition
#'   (e.g., \emph{la maniere dans laquelle il procede}).
#' }
#' \item{f_34_sentence_relatives}{
#'   Sentence relatives such as \emph{ce qui} following a clause
#'   (e.g., \emph{Il a reussi, ce qui est remarquable}).
#' }
#' \item{f_35_because}{Causal subordinator \emph{parce que}. \emph{puisque} and \emph{comme} are multifunctional (f_38); \emph{car} is a coordinator, like English \emph{for}, and is not counted.}
#' \item{f_36_though}{Concessive subordinators \emph{bien que} and \emph{quoique}.}
#' \item{f_37_if}{Conditional \emph{si} (including \emph{même si}, like English \emph{even if}), \emph{à moins que} and \emph{sauf si}. Indirect-question \emph{si} (\emph{je me demande si}) is excluded.}
#' \item{f_38_other_adv_sub}{Other adverbial subordinators (e.g., \emph{tandis que}, \emph{lorsque}, \emph{puisque}, \emph{comme}, \emph{encore que}, \emph{à condition que}).}
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
#' \item{f_44_mean_word_length}{Mean length in letters of purely alphabetic word tokens (numerals, hyphenated and elided forms excluded); \code{NA} for a document without any.}
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
#' \item{f_59_contractions}{Colloquial reductions that compete with a full form (e.g., \emph{p'tit}, \emph{chuis}, \emph{t'as}, \emph{j'pense}, \emph{y a} for \emph{il y a}). Obligatory elision (\emph{j'ai}, \emph{c'était}) is not counted.}
#' \item{f_60_that_deletion}{Reduced complement clauses competing with a \emph{que}-clause under \emph{croire}, \emph{penser}, \emph{espérer}, \emph{dire}, \emph{affirmer} and \emph{prétendre}: a bare infinitive (\emph{Je crois avoir compris}) or a finite clause without \emph{que} (\emph{Je crois il est parti}).}
#' \item{f_61_stranded_preposition}{Colloquial competitors of pied-piping in \emph{que}-relatives: orphan preposition (\emph{la fille que je sors avec}), resumptive prepositional phrase (\emph{un ami que je compte sur lui}) or resumptive \emph{en} (\emph{le film que je t'en ai parlé}). Pied-piping (\emph{avec qui}, \emph{dont}) is f_33.}
#' \item{f_62_split_infinitive}{Preposition + adverb + infinitive where the adverb normally follows the infinitive (e.g., \emph{de vraiment comprendre}, \emph{sans vraiment savoir}). Light adverbs that normally precede the infinitive (\emph{pour mieux comprendre}) and negation alone (\emph{de ne pas venir}) are not counted.}
#' \item{f_63_split_auxiliary}{\emph{avoir}/\emph{être} or modal \emph{pouvoir}/\emph{devoir} separated from its participle or infinitive by an adverb (e.g., \emph{a probablement été vu}, \emph{n'a jamais vu}, \emph{peut facilement résoudre}). The analytic negator alone (\emph{n'a pas mangé}), like English \emph{not}, is not counted.}
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
#' \item{f_67_neg_analytic}{
#'   Analytic negation with \emph{ne} ... adverb
#'   (e.g., \emph{ne ... pas}, \emph{ne ... jamais}).
#' }
#' }
#'
#' @param tokens A dataset of tokens created by `spacyr::spacy_parse()` or
#'   `udpipe::udpipe_annotate()`
#' @param measure Measure to use for lexical diversity (f_43). Passed to
#'   `quanteda.textstats::textstat_lexdiv()`. Can be the Moving Average
#'   Type-Token Ratio (MATTR, default), ordinary Type-Token Ratio (TTR),
#'   corrected TTR (CTTR), Mean Segmental Type-Token Ratio (MSTTR), or `"none"`
#'   to omit f_43. One measure is used for the whole corpus: if any document
#'   has fewer than 200 retained tokens, MATTR falls back to TTR for every
#'   document, with a message. Explicitly selected TTR, CTTR and MSTTR are
#'   honored. Types are lower-case word forms with their part-of-speech tag;
#'   punctuation, symbols and digit-final numbers are excluded.
#' @param normalize If `TRUE`, counts are normalized per 1,000 word tokens
#'   (UD word rows excluding punctuation, symbols, spaces and digit-final
#'   numeric tokens such as `42`; function words and spelled-out numerals
#'   included). Metrics are never normalized.
#' @param feature_names `"language"` (default) to name columns with the
#'   French display names in [feature_names], or `"shared"` for the identifiers
#'   shared by the English, Spanish and French packages. Set
#'   `options(pseudobibeR.feature_names = "shared")` to change the default.
#' @return A `data.frame` with one row per document, in order of first
#'   appearance: `doc_id` and the 67 feature columns (66 when
#'   `measure = "none"`). Each raw count is the number of occurrences that
#'   `audit_features()` returns for the feature.
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
#' # Basic usage with pre-parsed samples
#' biber(udpipe_samples)
#' biber(spacy_samples)
#'
#' # Get raw counts instead of normalized rates
#' raw_features <- biber(udpipe_samples, normalize = FALSE)
#' head(raw_features[, 1:5])
#'
#' # Use different type-token ratio measures
#' features_ttr <- biber(udpipe_samples, measure = "TTR")
#' features_mattr <- biber(udpipe_samples, measure = "MATTR")
#'
#' # Skip type-token ratio calculation
#' features_no_ttr <- biber(udpipe_samples, measure = "none")
#'
#' \dontrun{
#' # Parse your own French text with udpipe
#' library(udpipe)
#' udmodel <- udpipe_load_model("french-gsd-ud-2.5-191206.udpipe")
#' my_text <- "Voici un exemple de texte en français."
#' parsed <- udpipe_annotate(udmodel, my_text, parser = "default", tagger = "default")
#' features <- biber(parsed)
#'
#' # Or with spacyr (requires Python and spaCy installation)
#' library(spacyr)
#' spacy_initialize(model = "fr_core_news_sm")
#' parsed_sp <- spacy_parse("Voici un exemple de texte.", dependency = TRUE, tag = TRUE, pos = TRUE)
#' features_sp <- biber(parsed_sp)
#' spacy_finalize()
#' }
#' @importFrom magrittr %>%
#' @seealso [audit_features()], [dict], [word_lists]
#' @export
biber <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                  normalize = TRUE, feature_names = getOption("pseudobibeR.feature_names", "language")) {
  UseMethod("biber")
}

#' @rdname biber
#' @export
biber.spacyr_parsed <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                                normalize = TRUE,
                               feature_names = getOption("pseudobibeR.feature_names", "language")) {
  feature_names <- match_feature_names(feature_names)
  validate_spacy_tokens(tokens)
  out <- extract_french_features(tokens, match.arg(measure), normalize)
  names(out) <- display_feature_names(names(out), feature_names)
  out
}

#' @rdname biber
#' @export
biber.udpipe_connlu <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                                normalize = TRUE,
                               feature_names = getOption("pseudobibeR.feature_names", "language")) {
  feature_names <- match_feature_names(feature_names)
  validate_udpipe_tokens(tokens)
  out <- extract_french_features(tokens, match.arg(measure), normalize)
  names(out) <- display_feature_names(names(out), feature_names)
  out
}

validate_spacy_tokens <- function(tokens) {
  if (is.null(tokens)) {
    stop("'tokens' cannot be NULL. Please provide a valid spacyr_parsed object.", call. = FALSE)
  }
  if (!is.data.frame(tokens)) {
    stop("'tokens' must be a data frame. Got object of class: ",
         paste(class(tokens), collapse = ", "), call. = FALSE)
  }
  if (nrow(tokens) == 0) {
    stop("'tokens' is empty (0 rows). Please provide a data frame with parsed tokens.",
         call. = FALSE)
  }
  missing_cols <- setdiff(c("doc_id", "token", "lemma", "pos", "tag", "dep_rel"),
                          colnames(tokens))
  if ("dep_rel" %in% missing_cols) {
    stop("Column 'dep_rel' not found. Be sure to set 'dependency = TRUE' when using spacy_parse().",
         call. = FALSE)
  }
  if ("tag" %in% missing_cols) {
    stop("Column 'tag' not found. Be sure to set 'tag = TRUE' when using spacy_parse().",
         call. = FALSE)
  }
  if ("pos" %in% missing_cols) {
    stop("Column 'pos' not found. Be sure to set 'pos = TRUE' when using spacy_parse().",
         call. = FALSE)
  }
  if (length(missing_cols)) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "),
         ". Ensure 'tokens' is from spacyr::spacy_parse() with dependency=TRUE, tag=TRUE, pos=TRUE.",
         call. = FALSE)
  }
  invisible(tokens)
}

validate_udpipe_tokens <- function(tokens) {
  if (!requireNamespace("udpipe", quietly = TRUE)) {
    stop("Package 'udpipe' must be installed to extract features from udpipe-tagged text.\n",
         "Install it with: install.packages('udpipe')", call. = FALSE)
  }
  udpipe_tks <- as.data.frame(tokens, stringsAsFactors = FALSE)
  if (nrow(udpipe_tks) == 0) {
    stop("'tokens' is empty (0 rows). Please provide parsed tokens from udpipe_annotate().",
         call. = FALSE)
  }
  missing_cols <- setdiff(c("doc_id", "token", "lemma", "upos", "xpos", "dep_rel"),
                          colnames(udpipe_tks))
  if ("dep_rel" %in% missing_cols) {
    stop("Column 'dep_rel' not found. Be sure to set parser = 'default' in udpipe_annotate().",
         call. = FALSE)
  }
  if (any(c("upos", "xpos") %in% missing_cols)) {
    stop("Column 'upos' or 'xpos' not found. Be sure to set tagger = 'default' in udpipe_annotate().",
         call. = FALSE)
  }
  if (length(missing_cols)) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "),
         ". Ensure 'tokens' is from udpipe_annotate() with tagger='default' and parser='default'.",
         call. = FALSE)
  }
  invisible(tokens)
}

extract_french_features <- function(tokens, measure, normalize, traced = FALSE) {
  if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize)) {
    stop("'normalize' must be TRUE or FALSE.", call. = FALSE)
  }
  parse_biber_features(prepare_parsed_tokens(tokens), measure, normalize, traced)
}

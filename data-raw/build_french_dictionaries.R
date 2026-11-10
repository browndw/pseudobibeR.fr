"%||%" <- function(x, y) {
  if (is.null(x) || (is.vector(x) && length(x) == 0)) {
    return(y)
  }
  x
}

# Build dictionary and word list objects for pseudobibeR.fr
# Run via: source("data-raw/build_french_dictionaries.R") from the package root.

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(quanteda)
  library(yaml)
})

# Keep YAML 1.1 boolean words (on, y, no...) as literal strings.
read_inventory <- function(path) {
  keep <- function(x) x
  yaml::read_yaml(path, handlers = list("bool#yes" = keep, "bool#no" = keep))
}

# Remove acute, grave, circumflex, diaeresis and cedilla marks (D-34); twins
# that are contrastive grammatical words are not added.
add_accent_twins <- function(values, contrastive) {
  if (!length(values)) return(values)
  decomposed <- stringi::stri_trans_nfd(values)
  twins <- stringi::stri_trans_nfc(
    stringi::stri_replace_all_regex(decomposed, "[\\u0300\\u0301\\u0302\\u0308\\u0327]", "")
  )
  # Check each word of a phrase: a twin is dropped when any word that lost an
  # accent becomes a contrastive form (tout_à must not yield tout_a).
  collides <- mapply(function(original, twin) {
    a <- strsplit(original, "[_ ]")[[1]]
    b <- strsplit(twin, "[_ ]")[[1]]
    any(a != b & b %in% contrastive)
  }, values, twins, USE.NAMES = FALSE)
  twins <- twins[twins != values & !collides]
  sort(unique(c(values, twins)))
}

normalize_terms <- function(values, replace_spaces = TRUE) {
  if (is.null(values) || length(values) == 0) {
    return(character())
  }

  cleaned <- values %>%
    purrr::map_chr(as.character) %>%
    stringr::str_to_lower() %>%
    stringr::str_squish()

  cleaned <- cleaned[nzchar(cleaned)]

  if (replace_spaces) {
    cleaned <- stringr::str_replace_all(cleaned, "\\s+", "_")
  }

  sort(unique(cleaned))
}

normalize_multiword_patterns <- function(values) {
  if (is.null(values) || length(values) == 0) {
    return(character())
  }

  normalized <- normalize_terms(values, replace_spaces = FALSE)
  normalized <- normalized[stringr::str_detect(normalized, "_|\\s")]

  if (length(normalized) == 0) {
    return(character())
  }

  normalized %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_squish() %>%
    unique() %>%
    sort()
}

dict_path <- file.path("data-raw", "dict.yaml")
if (!file.exists(dict_path)) {
  stop("Cannot find dictionary specification at ", dict_path)
}

dict_spec <- read_inventory(dict_path)
if (!is.list(dict_spec)) {
  stop("Dictionary specification must be a named list")
}

dict_list <- dict_spec %>%
  purrr::imap(function(values, feature) {
    normalize_terms(values, replace_spaces = TRUE)
  }) %>%
  purrr::compact()

dict_list <- dict_list[order(names(dict_list))]
contrastive <- normalize_terms(read_inventory(file.path("data-raw", "word_lists.yaml"))$accent_contrastive_forms)
dict_list <- lapply(dict_list, add_accent_twins, contrastive = contrastive)
dict <- quanteda::dictionary(dict_list)

dict_multiword_patterns <- normalize_multiword_patterns(unlist(dict_list, use.names = FALSE))

word_lists_path <- file.path("data-raw", "word_lists.yaml")
if (!file.exists(word_lists_path)) {
  stop("Cannot find word list specification at ", word_lists_path)
}

word_lists_spec <- read_inventory(word_lists_path)
if (!is.list(word_lists_spec)) {
  stop("Word list specification must be a named list")
}

manual_multiword_patterns <- normalize_multiword_patterns(word_lists_spec$multiword_patterns)
multiword_patterns <- sort(unique(c(dict_multiword_patterns, manual_multiword_patterns)))

word_lists <- list(
  pronoun_matchlist = normalize_terms(word_lists_spec$pronoun_matchlist, replace_spaces = TRUE),
  demonstrative_matchlist = normalize_terms(word_lists_spec$demonstrative_matchlist, replace_spaces = TRUE),
  proverb_object_pronouns = normalize_terms(word_lists_spec$proverb_object_pronouns, replace_spaces = TRUE),
  impersonal_verbs = normalize_terms(word_lists_spec$impersonal_verbs, replace_spaces = TRUE),
  linking_matchlist = normalize_terms(word_lists_spec$linking_matchlist, replace_spaces = TRUE),
  verb_matchlist = normalize_terms(word_lists_spec$verb_matchlist, replace_spaces = TRUE),
  nominalization_stoplist = normalize_terms(word_lists_spec$nominalization_stoplist, replace_spaces = TRUE),
  nominal_infinitive_lemmas = normalize_terms(word_lists_spec$nominal_infinitive_lemmas, replace_spaces = TRUE),
  gerondif_noun_stoplist = normalize_terms(word_lists_spec$gerondif_noun_stoplist, replace_spaces = TRUE),
  colloquial_contractions = normalize_terms(word_lists_spec$colloquial_contractions, replace_spaces = FALSE),
  reduced_complement_predicates = normalize_terms(word_lists_spec$reduced_complement_predicates, replace_spaces = TRUE),
  orphan_prepositions = normalize_terms(word_lists_spec$orphan_prepositions, replace_spaces = TRUE),
  prepositions_compound = normalize_terms(word_lists_spec$prepositions_compound, replace_spaces = FALSE),
  preinfinitival_light_adverbs = normalize_terms(word_lists_spec$preinfinitival_light_adverbs, replace_spaces = TRUE),
  resumptive_prepositional_frames = normalize_terms(word_lists_spec$resumptive_prepositional_frames, replace_spaces = FALSE),
  resumptive_en_verbs = normalize_terms(word_lists_spec$resumptive_en_verbs, replace_spaces = TRUE),
  nominalization_suffixes = normalize_terms(word_lists_spec$nominalization_suffixes, replace_spaces = FALSE),
  multiword_patterns = multiword_patterns,
  neg_synthetic_determiners = normalize_terms(word_lists_spec$neg_synthetic_determiners, replace_spaces = TRUE),
  negation_particles = normalize_terms(word_lists_spec$negation_particles, replace_spaces = TRUE),
  neg_analytic_adverbs = normalize_terms(word_lists_spec$neg_analytic_adverbs, replace_spaces = TRUE),
  accent_contrastive_forms = contrastive
)
exact_lists <- c("accent_contrastive_forms", "nominalization_suffixes", "multiword_patterns")
twin_lists <- setdiff(names(word_lists), exact_lists)
word_lists[twin_lists] <- lapply(word_lists[twin_lists], add_accent_twins, contrastive = contrastive)
word_lists$multiword_patterns <- sort(unique(c(
  word_lists$multiword_patterns,
  gsub("_", " ", unlist(dict_list, use.names = FALSE)[grepl("_", unlist(dict_list, use.names = FALSE))])
)))

examples_path <- file.path("data-raw", "french_examples.yaml")
french_examples <- tibble::tibble(feature = character(), example = character(), count = double())

if (file.exists(examples_path)) {
  examples_spec <- yaml::read_yaml(examples_path)
  if (!is.null(examples_spec)) {
    french_examples <- examples_spec %>%
      purrr::map_dfr(function(entry) {
        tibble::tibble(
          feature = entry$feature %||% NA_character_,
          example = entry$example %||% NA_character_,
          count = as.numeric(entry$count %||% NA_real_)
        )
      }) %>%
      dplyr::mutate(
        feature = stringr::str_squish(stringr::str_to_lower(.data$feature)),
        example = stringr::str_squish(.data$example)
      ) %>%
      dplyr::filter(!is.na(.data$feature), nzchar(.data$feature))

    french_examples <- french_examples %>%
      dplyr::arrange(.data$feature, .data$example) %>%
      dplyr::mutate(count = dplyr::coalesce(.data$count, 1))
  }
}

data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

save(dict, file = file.path(data_dir, "dict.rda"), compress = "xz")
save(word_lists, file = file.path(data_dir, "word_lists.rda"), compress = "xz")
save(french_examples, file = file.path(data_dir, "french_examples.rda"), compress = "xz")

message("French dictionary resources saved to data/ directory.")

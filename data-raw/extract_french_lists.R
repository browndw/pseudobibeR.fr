# Extract candidate French lexical lists from biber feature mapping CSV.
# Run with: Rscript data-raw/extract_french_lists.R

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tidyr)
})

csv_path <- file.path("..", "biber_features_equivalents_french.csv")
if (!file.exists(csv_path)) {
  stop("Could not locate feature mapping CSV at ", csv_path)
}

raw <- readr::read_csv(csv_path, show_col_types = FALSE, trim_ws = TRUE)

lexical_features <- c(
  "f_04_place_adverbials",
  "f_05_time_adverbials",
  "f_06_first_person_pronouns",
  "f_07_second_person_pronouns",
  "f_08_third_person_pronouns",
  "f_10_demonstrative_pronoun",
  "f_11_indefinite_pronoun",
  "f_45_conjuncts",
  "f_46_downtoners",
  "f_47_hedges",
  "f_48_amplifiers",
  "f_49_emphatics",
  "f_50_discourse_particles",
  "f_51_demonstratives",
  "f_52_modal_possibility",
  "f_53_modal_necessity",
  "f_54_modal_predictive",
  "f_55_verb_public",
  "f_56_verb_private",
  "f_57_verb_suasive",
  "f_58_verb_seem"
)

feature_rows <- raw |>
  dplyr::filter(Feature %in% lexical_features)

extract_terms <- function(text) {
  if (is.na(text) || !nzchar(text)) {
    return(character())
  }
  parts <- stringr::str_split_fixed(text, "=", 2)
  if (ncol(parts) < 2) {
    return(character())
  }
  rhs <- stringr::str_replace_all(parts[, 2], '"', "") |>
    stringr::str_replace_all("’", "'") |>
    stringr::str_replace_all("\u2019", "'")
  if (!nzchar(rhs)) {
    return(character())
  }
  items <- stringr::str_split(rhs, ",")[[1]]
  cleaned <- stringr::str_squish(items) |>
    stringr::str_remove("^_punct_\\s*") |>
    stringr::str_replace_all("\\s+'\\s+", "'") |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_remove("[.]+$") |>
    stringr::str_remove("\\s+$")
  cleaned <- cleaned[nzchar(cleaned)]
  unique(sort(cleaned))
}

feature_lists <- feature_rows |>
  dplyr::mutate(candidate_terms = purrr::map(`Sharoff's biberpy translations`, extract_terms)) |>
  dplyr::mutate(n_terms = purrr::map_int(candidate_terms, length))

output_dir <- file.path("data-raw", "probe-output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

feature_lists |>
  dplyr::select(Feature, candidate_terms) |>
  tidyr::unnest_longer(candidate_terms, values_to = "term") |>
  dplyr::filter(!is.na(term), nzchar(term)) |>
  readr::write_csv(file.path(output_dir, "french_feature_terms.csv"))

feature_lists |>
  dplyr::select(Feature, n_terms) |>
  readr::write_csv(file.path(output_dir, "french_feature_term_counts.csv"))

message("Extracted candidate lexical lists to ", normalizePath(output_dir, mustWork = FALSE))

#' Dictionaries defining text features
#'
#' For Biber features defined by matching text against dictionaries of word
#' patterns (such as third-person pronouns or conjunctions), or features that
#' can be found by matching patterns against text, this gives the dictionary of
#' patterns for each feature. These are primarily used internally by `biber()`,
#' but are exported so users can examine the feature definitions.
#'
#' @format A named list with one entry per feature. The name is the feature
#'   name, such as `f_33_pied_piping`; values give a list of terms or patterns.
#'   Patterns are matched to spaCy tokens using `quanteda::tokens_lookup()`
#'   using the `glob` valuetype.
"dict"

#' Lists of words defining text features
#'
#' For Biber features defined by matching texts against certain exact words,
#' rather than patterns, this list defines the exact words defining the
#' features. These lists are primarily used internally by `biber()`, but are
#' exported so users can examine the feature definitions.
#'
#' @format A named list with one entry per word list. Each entry is a vector of
#'   words.
"word_lists"

#' French example sentences illustrating features
#'
#' Example sentences demonstrating how individual features can appear in
#' French texts. Each row identifies the feature, the example sentence, and a
#' representative count for that sentence.
#'
#' @format A tibble with columns `feature` (feature identifier), `example`
#'   (example sentence), and `count` (numeric weight for the example).
"french_examples"

#' Samples of parsed text
#'
#' Examples of spaCy and udpipe tagging output from excerpts of several
#' public-domain texts. Can be passed to `biber()` to see examples of its
#' feature detection.
#'
#' @details Texts consist of early paragraphs from several public-domain books
#'   distributed by Project Gutenberg <https://gutenberg.org>. Document IDs are
#'   the Project Gutenberg book numbers.
#'
#' See `udpipe::udpipe_annotate()` and `spacyr::spacy_parse()` for
#'   further details on the data format produced by each package.
"udpipe_samples"

#' @rdname udpipe_samples
"spacy_samples"

#' Display names for feature columns
#'
#' A named character vector mapping shared feature identifiers (names) to the
#' French display names used in [biber()] and [audit_features()] output, e.g.
#' `f_15_gerunds` to `f_15_nominal_infinitives`. Each display name keeps its
#' `f_NN_` number, which identifies the same feature in the English, Spanish
#' and French packages. Features not listed keep their shared name. Built from
#' `data-raw/feature_names.yaml`.
#' @format A named character vector.
#' @seealso [biber()]
"feature_names"

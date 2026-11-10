#' Audit the occurrences behind feature counts
#'
#' Returns one row per occurrence counted by [biber()], with its sentence
#' context, so that counts can be inspected directly. Rows come from the same
#' extraction run as the counts: for every countable feature the number of
#' rows per document equals the raw count from
#' `biber(tokens, normalize = FALSE)`.
#'
#' @param tokens Annotated tokens, as accepted by [biber()].
#' @param feature One or more feature names: display names (see
#'   [feature_names], e.g. `"f_19_copula_main_verb"`) or shared identifiers
#'   (`"f_19_be_main_verb"`).
#' @param category A category letter (`"A"` to `"P"`), its full label (for
#'   example `"L. Modals"`), or a short alias such as `"Modals"`,
#'   `"Passives"` or `"Negation"`. Supply exactly one of `feature` and
#'   `category`.
#' @param sample_n Optional maximum number of rows to return, sampled at random.
#' @param seed Optional random seed used when sampling; the caller's random
#'   state is restored afterwards.
#' @param max_per_doc Optional maximum number of rows per document, applied
#'   before `sample_n`.
#' @param window Number of context tokens shown on each side of the match.
#' @param feature_names `"language"` (default) to report features with the
#'   French display names in [feature_names], or `"shared"` for the identifiers
#'   shared by the companion packages. `feature` accepts either name.
#' @return An `audit_features_result` tibble with columns `feature`,
#'   `category`, `doc_id`, `sentence_id`, `count` (always 1), `token_count`
#'   (sentence length), `match_type` (`"exact"`), `left`, `keyword`, `right`
#'   and `chunk` (the full sentence). Lexical metrics (f_43, f_44) cannot be
#'   audited.
#' @examples
#' audit_features(spacy_samples, feature = "f_19_be_main_verb", sample_n = 5)
#' if (requireNamespace("udpipe", quietly = TRUE)) {
#'   audit_features(udpipe_samples, category = "Modals", sample_n = 10, seed = 1)
#' }
#' @seealso [biber()]
#' @export
audit_features <- function(tokens, feature = NULL, category = NULL, sample_n = NULL,
                           seed = NULL, max_per_doc = NULL, window = 5,
                           feature_names = getOption("pseudobibeR.feature_names", "language")) {
  UseMethod("audit_features")
}

#' @rdname audit_features
#' @export
audit_features.spacyr_parsed <- function(tokens, feature = NULL, category = NULL,
                                         sample_n = NULL, seed = NULL,
                                         max_per_doc = NULL, window = 5,
                                         feature_names = getOption("pseudobibeR.feature_names", "language")) {
  validate_spacy_tokens(tokens)
  feature_names <- match_feature_names(feature_names)
  feature <- shared_feature_ids(feature)
  result <- audit_french_features(tokens, feature, category, sample_n, seed, max_per_doc, window)
  result$feature <- display_feature_names(result$feature, feature_names)
  result
}

#' @rdname audit_features
#' @export
audit_features.udpipe_connlu <- function(tokens, feature = NULL, category = NULL,
                                         sample_n = NULL, seed = NULL,
                                         max_per_doc = NULL, window = 5,
                                         feature_names = getOption("pseudobibeR.feature_names", "language")) {
  validate_udpipe_tokens(tokens)
  feature_names <- match_feature_names(feature_names)
  feature <- shared_feature_ids(feature)
  result <- audit_french_features(tokens, feature, category, sample_n, seed, max_per_doc, window)
  result$feature <- display_feature_names(result$feature, feature_names)
  result
}

validate_audit_integer <- function(value, name, minimum = 0, optional = FALSE) {
  if (optional && is.null(value)) return(invisible(NULL))
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < minimum || value > .Machine$integer.max ||
      value != floor(value)) {
    stop("'", name, "' must be a single integer >= ", minimum, ".", call. = FALSE)
  }
}

resolve_audit_features <- function(feature, category) {
  if (is.null(feature) == is.null(category)) {
    stop("Supply exactly one of 'feature' or 'category'.", call. = FALSE)
  }
  if (!is.null(category)) {
    if (!is.character(category) || length(category) != 1L || is.na(category)) {
      stop("'category' must be a single character string.", call. = FALSE)
    }
    key <- stringr::str_squish(stringr::str_to_lower(category))
    labels <- unname(.feature_categories)
    letters_ <- substr(labels, 1L, 1L)
    names_ <- stringr::str_to_lower(sub("^[A-P]\\. ", "", labels))
    code <- if (nchar(key) == 1L) {
      toupper(key)
    } else if (key %in% stringr::str_to_lower(labels)) {
      letters_[match(key, stringr::str_to_lower(labels))]
    } else if (key %in% names_) {
      letters_[match(key, names_)]
    } else if (key %in% names(.category_aliases)) {
      .category_aliases[[key]]
    } else {
      NA_character_
    }
    feature <- names(.feature_categories)[letters_ %in% code]
    if (!length(feature)) stop("Unknown category: ", category, call. = FALSE)
    feature <- setdiff(feature, .metric_features)
  }
  if (!is.character(feature) || !length(feature) || anyNA(feature) ||
      any(!feature %in% names(.feature_categories))) {
    stop("'feature' must contain known feature identifiers.", call. = FALSE)
  }
  if (any(feature %in% .metric_features)) {
    stop("Lexical summary metrics (f_43, f_44) are not supported by audit_features().",
         call. = FALSE)
  }
  unique(feature)
}

audit_french_features <- function(tokens, feature, category, sample_n, seed,
                                  max_per_doc, window) {
  feature <- resolve_audit_features(feature, category)
  validate_audit_integer(sample_n, "sample_n", 1, TRUE)
  validate_audit_integer(max_per_doc, "max_per_doc", 1, TRUE)
  validate_audit_integer(seed, "seed", 0, TRUE)
  validate_audit_integer(window, "window")
  prepared <- prepare_parsed_tokens(tokens)
  result <- parse_biber_features(prepared, "none", FALSE, traced = TRUE)
  evidence <- result$evidence[result$evidence$feature %in% feature, ]
  evidence <- evidence[order(match(evidence$doc_id, attr(prepared, "doc_order")),
                             evidence$sentence_id, evidence$token_id,
                             match(evidence$feature, names(.feature_categories))), ]
  rows <- lapply(seq_len(nrow(evidence)), function(i) {
    occurrence <- evidence[i, ]
    sentence <- prepared[prepared$doc_id == occurrence$doc_id &
                           prepared$sentence_id == occurrence$sentence_id, ]
    start <- match(occurrence$token_id, sentence$token_id_int)
    width <- length(strsplit(occurrence$token, " ", fixed = TRUE)[[1L]])
    end <- min(nrow(sentence), start + width - 1L)
    left <- if (start > 1L && window > 0) seq.int(max(1L, start - window), start - 1L) else integer()
    right <- if (end < nrow(sentence) && window > 0) {
      seq.int(end + 1L, min(nrow(sentence), end + window))
    } else {
      integer()
    }
    tibble::tibble(
      feature = occurrence$feature,
      category = substr(.feature_categories[[occurrence$feature]], 1L, 1L),
      doc_id = occurrence$doc_id, sentence_id = occurrence$sentence_id,
      count = 1L, token_count = nrow(sentence), match_type = "exact",
      left = paste(sentence$surface[left], collapse = " "),
      keyword = paste(sentence$surface[seq.int(start, end)], collapse = " "),
      right = paste(sentence$surface[right], collapse = " "),
      chunk = paste(sentence$surface, collapse = " ")
    )
  })
  out <- if (length(rows)) dplyr::bind_rows(rows) else tibble::tibble(
    feature = character(), category = character(), doc_id = character(),
    sentence_id = integer(), count = integer(), token_count = integer(),
    match_type = character(), left = character(), keyword = character(),
    right = character(), chunk = character()
  )
  if (!is.null(sample_n) || !is.null(max_per_doc)) {
    if (!is.null(seed)) {
      had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
      if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
      on.exit({
        if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
        else rm(".Random.seed", envir = .GlobalEnv)
      }, add = TRUE)
      set.seed(seed)
    }
    if (!is.null(max_per_doc)) {
      out <- out %>%
        dplyr::group_by(.data$doc_id) %>%
        dplyr::group_modify(~ dplyr::slice_sample(.x, n = min(max_per_doc, nrow(.x)))) %>%
        dplyr::ungroup()
    }
    if (!is.null(sample_n) && nrow(out) > sample_n) {
      out <- dplyr::slice_sample(out, n = sample_n)
    }
  }
  structure(out, class = c("audit_features_result", class(out)))
}

#' Print an audit result
#'
#' @param x An `audit_features_result`.
#' @param n Maximum number of rows to display.
#' @param ... Arguments passed to the tibble print method.
#' @return `x`, invisibly.
#' @export
print.audit_features_result <- function(x, n = 10, ...) {
  cat("audit_features_result:", nrow(x), "exact occurrences\n")
  print(tibble::as_tibble(x), n = n, ...)
  invisible(x)
}

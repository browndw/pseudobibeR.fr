# Token preparation shared by biber() and audit_features()

#' Prepare parsed tokens for feature extraction
#'
#' Converts UDPipe or spaCy annotations (or an equivalent data frame) to one
#' word row per UD word. Multiword-token container rows and spaCy SPACE rows
#' are removed, token/lemma/dependency labels are lower-cased, the original
#' surface form is kept in `surface`, and `match_token` holds the lower-cased
#' form with typographic apostrophes normalized for list matching. Documents
#' keep their order of first appearance.
#'
#' @param tokens Parsed token data frame or `udpipe_connlu` object.
#' @return A tibble of word rows with integer IDs and morphology columns.
#' @keywords internal
prepare_parsed_tokens <- function(tokens) {
  if (inherits(tokens, "udpipe_connlu")) {
    if (!requireNamespace("udpipe", quietly = TRUE)) {
      stop("Package 'udpipe' must be installed to extract features from udpipe-tagged text.",
           call. = FALSE)
    }
    tokens <- as.data.frame(tokens, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(tokens) || nrow(tokens) == 0L) {
    stop("'tokens' is empty (0 rows). Please provide parsed tokens.", call. = FALSE)
  }
  tokens <- tibble::as_tibble(as.data.frame(tokens, stringsAsFactors = FALSE))
  if ("upos" %in% names(tokens)) {
    tokens$pos <- tokens$upos
    tokens$tag <- if ("xpos" %in% names(tokens)) tokens$xpos else NA_character_
  }
  if (!"sentence_id" %in% names(tokens)) tokens$sentence_id <- 1L
  if (!"token_id" %in% names(tokens)) {
    tokens <- dplyr::mutate(
      dplyr::group_by(tokens, .data$doc_id, .data$sentence_id),
      token_id = dplyr::row_number()
    ) %>% dplyr::ungroup()
  }
  if (!"head_token_id" %in% names(tokens)) tokens$head_token_id <- NA_character_
  required <- c("doc_id", "token", "lemma", "pos", "tag", "dep_rel")
  missing <- setdiff(required, names(tokens))
  if (length(missing)) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  for (column in c("doc_id", "token_id", "token", "lemma", "pos", "tag",
                   "head_token_id", "dep_rel")) {
    tokens[[column]] <- as.character(tokens[[column]])
  }
  if (anyNA(tokens$doc_id) || any(!nzchar(trimws(tokens$doc_id)))) {
    stop("'doc_id' must contain non-missing, non-empty identifiers.", call. = FALSE)
  }
  doc_order <- unique(tokens$doc_id)
  sentence <- suppressWarnings(as.integer(as.character(tokens$sentence_id)))
  if (anyNA(sentence)) {
    stop("'sentence_id' must contain integer identifiers.", call. = FALSE)
  }
  tokens$sentence_id <- sentence
  container <- grepl("^[0-9]+[-.][0-9]+$", tokens$token_id)
  tokens <- tokens[!container, ]
  tokens$token_id_int <- suppressWarnings(as.integer(tokens$token_id))
  tokens$head_token_id_int <- suppressWarnings(as.integer(tokens$head_token_id))
  if (anyNA(tokens$token_id_int) || any(tokens$token_id_int < 1L)) {
    stop("'token_id' must contain positive integer word IDs.", call. = FALSE)
  }
  if (anyDuplicated(tokens[c("doc_id", "sentence_id", "token_id_int")])) {
    stop("Duplicate token keys (doc_id, sentence_id, token_id).", call. = FALSE)
  }
  tokens$token[is.na(tokens$token)] <- ""
  tokens$lemma <- dplyr::coalesce(tokens$lemma, tokens$token)
  tokens$pos <- dplyr::coalesce(tokens$pos, "X")
  if ("morph" %in% names(tokens)) {
    morph <- vapply(tokens$morph, function(x) {
      if (inherits(x, "python.builtin.object")) {
        if (!requireNamespace("reticulate", quietly = TRUE)) {
          stop("Install 'reticulate' to convert spaCy morphology.", call. = FALSE)
        }
        x <- reticulate::py_to_r(x)
      }
      if (is.null(x) || !length(x) || all(is.na(x))) NA_character_
      else paste(as.character(x), collapse = "|")
    }, character(1))
    morph <- dplyr::na_if(morph, "")
    tokens$feats <- if ("feats" %in% names(tokens)) {
      dplyr::coalesce(dplyr::na_if(as.character(tokens$feats), ""), morph)
    } else {
      morph
    }
  }
  if (!"feats" %in% names(tokens)) tokens$feats <- NA_character_
  tokens$feats <- as.character(tokens$feats)
  tokens$surface <- tokens$token
  tokens$token <- stringi::stri_trans_tolower(tokens$token)
  tokens$lemma <- stringi::stri_trans_tolower(tokens$lemma)
  tokens$dep_rel <- stringi::stri_trans_tolower(dplyr::coalesce(tokens$dep_rel, ""))
  tokens$pos[tokens$token == "\n"] <- "PUNCT"
  tokens <- tokens[tokens$pos != "SPACE", ]
  if (nrow(tokens) == 0L) {
    stop("No valid tokens found after filtering. Document may contain only whitespace.",
         call. = FALSE)
  }
  doc_order <- doc_order[doc_order %in% tokens$doc_id]
  tokens$tag <- dplyr::coalesce(dplyr::na_if(dplyr::na_if(tokens$tag, ""), "_"), tokens$pos)
  tokens$match_token <- normalize_terms(tokens$token)
  tokens$match_lemma <- normalize_terms(tokens$lemma)
  for (key in c("Tense", "VerbForm", "Mood", "PronType", "Voice", "Number", "Person")) {
    tokens[[paste0("morph_", tolower(key))]] <- extract_morph_value(tokens$feats, key)
  }
  tokens <- tokens[order(match(tokens$doc_id, doc_order), tokens$sentence_id,
                         tokens$token_id_int), ]
  attr(tokens, "doc_order") <- doc_order
  tokens
}

#' Add sentence-local neighbour columns
#'
#' Previous/next token attributes never cross sentence or document
#' boundaries; at a boundary they are `NA`.
#'
#' @param tokens Prepared tokens.
#' @return `tokens` with `prev_*`, `prev2_*`, `prev3_*`, `next_*` and `next2_*`
#'   columns for token, lemma, POS, dependency relation and verb form.
#' @keywords internal
add_token_context <- function(tokens) {
  fields <- c(token = "token", lemma = "lemma", pos = "pos", dep_rel = "dep_rel",
              verbform = "morph_verbform", prontype = "morph_prontype",
              id = "token_id_int")
  grouped <- dplyr::group_by(tokens, .data$doc_id, .data$sentence_id)
  for (name in names(fields)) {
    column <- fields[[name]]
    grouped <- dplyr::mutate(
      grouped,
      "prev_{name}" := dplyr::lag(.data[[column]]),
      "prev2_{name}" := dplyr::lag(.data[[column]], 2),
      "prev3_{name}" := dplyr::lag(.data[[column]], 3),
      "prev4_{name}" := dplyr::lag(.data[[column]], 4),
      "next_{name}" := dplyr::lead(.data[[column]]),
      "next2_{name}" := dplyr::lead(.data[[column]], 2),
      "next3_{name}" := dplyr::lead(.data[[column]], 3)
    )
  }
  grouped <- dplyr::mutate(
    grouped,
    position_in_sentence = dplyr::row_number(),
    sentence_length = dplyr::n()
  )
  out <- dplyr::ungroup(grouped)
  attr(out, "doc_order") <- attr(tokens, "doc_order")
  out
}

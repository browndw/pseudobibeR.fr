test_that("UDPipe french examples align with expected feature counts", {
  skip_if_not_installed("udpipe")
  skip_on_cran()

  tests_dir <- testthat::test_path()
  package_tests <- normalizePath(file.path(tests_dir, ".."), mustWork = FALSE)
  package_root <- normalizePath(file.path(package_tests, ".."), mustWork = FALSE)
  workspace_root <- normalizePath(file.path(package_root, ".."), mustWork = FALSE)

  candidate_paths <- c(
    testthat::test_path("..", "french-gsd-ud-2.5-191206.udpipe"),
    file.path(package_root, "tests", "french-gsd-ud-2.5-191206.udpipe"),
    file.path(workspace_root, "tests", "french-gsd-ud-2.5-191206.udpipe")
  )

  existing_models <- candidate_paths[file.exists(candidate_paths)]
  skip_if(length(existing_models) == 0, "UDPipe French model missing")

  model_path <- normalizePath(existing_models[[1]], mustWork = TRUE)

  model <- udpipe::udpipe_load_model(model_path)
  if ("udpipe_free_model" %in% getNamespaceExports("udpipe")) {
    on.exit(udpipe::udpipe_free_model(model))
  }

  sample_examples <- french_examples |>
    dplyr::group_by(feature) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  unsupported_features <- c("f_61_stranded_preposition", "f_62_split_infinitive")
  sample_examples <- dplyr::filter(sample_examples, !feature %in% unsupported_features)

  annotations <- udpipe::udpipe_annotate(
    model,
    x = sample_examples$example,
    doc_id = sample_examples$feature,
    parser = "default"
  )

  feature_counts <- biber(annotations, measure = "none", normalize = FALSE)

  missing_columns <- character()

  lookup_count <- function(feature_id) {
    doc_row <- dplyr::filter(feature_counts, doc_id == feature_id)
    expect_equal(nrow(doc_row), 1L, info = paste0("Missing doc row for ", feature_id))
    if (!feature_id %in% colnames(doc_row)) {
      missing_columns <<- unique(c(missing_columns, feature_id))
      return(0)
    }
    as.numeric(doc_row[[feature_id]])
  }

  comparison <- sample_examples |>
    dplyr::mutate(observed = purrr::map_dbl(.data$feature, lookup_count)) |>
    dplyr::mutate(matches = .data$observed == .data$count)

  mismatches <- dplyr::filter(comparison, !.data$matches)

  problems <- character()

  if (length(missing_columns) > 0) {
    problems <- c(
      problems,
      paste0("Missing feature columns: ", paste(missing_columns, collapse = ", "))
    )
  }

  if (nrow(mismatches) > 0) {
    details <- mismatches |>
      dplyr::transmute(
        detail = paste0(
          .data$feature,
          ": expected ", .data$count,
          ", observed ", .data$observed
        )
      ) |>
      dplyr::pull("detail")
    problems <- c(problems, paste(c("Feature mismatches:", details), collapse = "\n  "))
  }

  message_text <- if (length(problems) == 0) "" else paste(problems, collapse = "\n  ")
  testthat::expect_true(length(problems) == 0, info = message_text)
})

# Contributing to pseudobibeR.fr

Thank you for helping improve the package. Please report problems and
suggestions on [GitHub](https://github.com/browndw/pseudobibeR.fr/issues).

## Reporting issues

Please include:

- a short text that shows the problem and the counts you expected;
- the parser and model used (for example udpipe `french-gsd-ud-2.5-191206`
  or spaCy `fr_core_news_sm`), with their versions;
- your R and package versions.

`audit_features()` output for the feature in question is very helpful: it
shows exactly which words were counted.

Proposals to change what a feature counts are welcome. Please explain the
linguistic reasoning; accepted changes are recorded in the family decision
log (`CROSS_LANGUAGE_ALIGNMENT.md`) so that the three packages stay parallel.

## Package layout

- `R/`: `biber()` and `audit_features()` (`biber.R`, `audit_features.R`), token
  preparation, and feature detectors grouped by category (`features_*.R`).
  Each detector returns the occurrences it found; counts are tabulated from
  those occurrences.
- `data-raw/`: editable sources and build scripts (see
  [`data-raw/README.md`](data-raw/README.md)):
  - `dict.yaml`, `word_lists.yaml`: word lists and phrases used by detectors;
  - `french_examples.yaml`: example sentences with expected counts;
  - `feature_names.yaml`: French display names for output columns.
- `tests/testthat/`: unit tests on controlled annotations and live parser tests.
- `FEATURES_TECHNICAL.md`: the exact rule behind each feature. Update it when a
  rule changes.

## Rebuilding package data

After editing a YAML file, rebuild from the package root:

```r
source("data-raw/build_french_dictionaries.R")  # dict, word_lists, examples
source("data-raw/build_feature_names.R")        # feature_names
source("data-raw/build_french_samples.R")       # sample parses (only if needed)
```

`data-raw/french_edge_cases.yaml` holds difficult constructions used as a
regression baseline. After changing detectors or that file, regenerate the
baseline with `Rscript data-raw/generate_edge_case_fixture.R` and review every
changed count before committing. Commit the YAML sources together with the
regenerated files in `data/` and `tests/testthat/fixtures/`.

## Running tests and checks

```r
devtools::document()
devtools::test()
devtools::check()
```

Tests that parse text need the udpipe model at
`tests/models/french-gsd-ud-2.5-191206.udpipe`; they are skipped without it.
Tests use the shared feature identifiers
(`options(pseudobibeR.feature_names = "shared")` is set in a test helper).

Expected counts in examples and tests should come from linguistic judgment,
not from the extractor's own output.

# pseudobibeR.fr 0.0.0.94

## Major Changes

### Feature Auditing Workflow
* Added `audit_features()` as a collaborator-facing audit helper for parser output from both spaCy and UDPipe
* Supports auditing by individual feature or by feature category, with optional `sample_n`, `max_per_doc`, `seed`, and KWIC `window` controls
* Returns review-ready audit tables with document IDs, sentence IDs, feature labels, and reconstructed text chunks

### Exact KWIC Anchors
* Expanded exact token-span auditing beyond sentence-level fallback rows for a broad set of high-value features
* Added exact anchors for:
  - Nominal and stative features
  - Lexical classes such as conjuncts, hedges, amplifiers, and discourse particles
  - Passives, modals, and relative clauses
  - Clause-embedding and subordination features
  - Negation features
  - Coordination and split constructions
* Exact matches now expose `left`, `keyword`, and `right` context columns for faster manual review

## Documentation Improvements

* Added `audit_features()` examples to the Getting Started vignette
* Added a compact `print()` method for `audit_features_result` objects so collaborator-facing output is easier to scan by default
* Regenerated package documentation for the new audit API behavior and print method

## Internal Improvements

* Centralized parsed-token preparation in shared helpers so `biber()` and `audit_features()` use the same normalization path
* Reused shared UDPipe coercion for both extraction and auditing workflows
* Tightened exact-match and split-auxiliary handling to keep focused and full test runs warning-free

## Validation

* Added focused tests for audit exact anchors across lexical, clausal, negation, coordination, and split-construction features
* Full test suite passes locally after the audit expansion (`PASS 276, SKIP 3`)

---

# pseudobibeR.fr 0.0.0.93

## Major Changes

### Cross-Parser Validation
* Validated feature extraction across UDPipe and spaCy parsers
* Nearly identical MDA results (R² = 0.715 vs 0.703)
* Demonstrates robustness across NLP pipelines
* French Factor 1 aligns with Biber's English Dimension 1 (Interactional vs Information Production)

### Documentation Website
* Complete Quarto documentation site with:
  - Getting Started guide
  - Cross-Parser Validation vignette
  - Chambers–Le Baron corpus walkthrough
  - Feature categories with French equivalents and examples
  - Data sources with corpus composition tables

### Code Reorganization
* Modularized all parsing code into block functions (e.g., `block_contractions_fr`, `block_passives_fr`)
* Each block is self-contained and independently testable
* Improved maintainability and troubleshooting
* Comprehensive inline documentation explaining linguistic rationale for each block

## Bug Fixes

### Contraction Detection (f_59_contractions)
* **Fixed**: Over-counting due to confusion between contractions and elisions
* **Problem**: Was counting grammatical elisions (l', d', qu') as informal contractions
* **Solution**: Implemented POS-based filtering to distinguish:
  - **Elisions** (excluded): Grammatical function words with POS tags DET, ADP, PRON, ADV, SCONJ
  - **Contractions** (counted): Informal lexical forms with POS tags ADJ, NOUN, PROPN (e.g., p'tit, m'sieur)
* **Validation**: Analyzed 74K apostrophe tokens showing clear POS-based pattern
* **Cross-parser**: Works identically with both UDPipe and spaCy

## Documentation Improvements

* Added comprehensive feature descriptions in French (67 features)
* Token counts and composition tables for all corpora:
  - Chambers–Le Baron: 125,445 tokens, 10 disciplines
  - French Register Corpus: 1.6M tokens, 6 registers
* Cross-references between vignettes and data sources
* Implementation details with extensive code comments

## Infrastructure

* GitHub Actions workflows for:
  - Automated testing with UDPipe model download
  - Quarto documentation deployment (triggered on version tags)
  - CRAN-ready package releases
* All dependencies properly specified in DESCRIPTION
* Conditional evaluation in vignettes for reproducibility

---

# pseudobibeR.fr 0.0.0.92

Initial development version with core feature extraction framework.

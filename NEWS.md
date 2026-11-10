# pseudobibeR.fr (development version)

## Evidence-based pipeline

* Output columns use language-specific display names from
  `data-raw/feature_names.yaml` (D-54), e.g. `f_15_nominal_infinitives` for
  the shared identifier `f_15_gerunds`. Each name keeps its `f_NN_` number,
  which identifies the same feature in all three packages. Use
  `feature_names = "shared"` in `biber()`/`audit_features()`, or
  `options(pseudobibeR.feature_names = "shared")`, for the shared identifiers;
  `audit_features(feature = )` accepts either name.
* Every count is now the number of detected occurrences: each feature block
  records one evidence row per word or construction, and `audit_features()`
  returns exactly those rows (`count = 1`). For every countable feature, the
  rows per document equal the raw count from `biber(normalize = FALSE)`.
* Dictionary matching runs on the parser's own word rows, sentence by sentence,
  instead of re-tokenized text. Elided words (`l'`, `quelqu'un`) and typographic
  apostrophes are matched; a dictionary phrase takes precedence over words it
  contains (`en quelque sorte` is a hedge, not also an indefinite pronoun).
* Package resources are always taken from `pseudobibeR.fr`, regardless of
  objects named `dict` or `word_lists` elsewhere.
* Counts are normalized per 1,000 word tokens: UD word rows excluding
  punctuation, symbols, spaces and digit-final numerals (`42`), as in English.
  The output contains `doc_id` and the feature columns; documents keep their
  order of first appearance.
* Lexical diversity (f_43) uses lower-case word forms with their tag, excluding
  punctuation and numbers, without dictionary compounding. Default MATTR falls
  back to TTR for the whole corpus if any document is under 200 tokens;
  explicitly chosen TTR, CTTR and MSTTR are honored.
* Mean word length (f_44) now includes accented words (any Unicode letter).
  A document without alphabetic words gets `NA`, as in English and Spanish,
  rather than 0 (D-51).

## Accepted cross-language decisions applied

* f_01/f_03 count finite verbs and auxiliaries in the indicative and
  subjunctive (D-33): `a` in `il a mangé` is present tense, and the present
  subjunctive counts. The conditional (marked `Tense=Pres|Mood=Cnd` by UDPipe)
  and the imperative are not present tense.
* f_54 counts finite future and conditional forms (`viendra`, `aurait aimé`,
  `voudrais`) and aller in the present or imperfect governing an infinitive
  (`va partir`); previously only `sera`/`serait` and adjacent aller + infinitive
  were counted.
* Modal features count modal verbs and periphrases grammaticalized as modals
  (D-40): f_52 pouvoir, f_53 devoir/falloir, f_54 future, conditional and
  aller + infinitive. `avoir la possibilité de`, `être obligé de`, `être
  nécessaire de`, `risquer de` and `être susceptible de` are no longer counted,
  as their English and Spanish counterparts are not.
* f_15 counts infinitives functioning as nouns, in parallel with Spanish
  (D-23, D-43): with a determiner, possessive or attributive adjective (`le
  savoir`, `son rire`, `le boire et le manger`) or heading a subject before its
  verb (`Fumer tue`, `Lire est un plaisir`). The gérondif (`en travaillant`),
  previously counted here, is adverbial and is now counted only in f_25,
  including when UDPipe tags `en chantant` as a noun (which also removes it
  from f_16). Postposed subjects (`Il est important de se reposer`), objects
  and prepositional infinitives remain f_24.
* Scope alignments (D-56): f_50 counts discourse particles only at the start
  of a clause (sentence-initial or after punctuation), as Biber and English
  do, so adverbial *bien* and temporal *alors* no longer count; f_39 counts
  listed compound prepositions (*à cause de*, *grâce à*) once, as Spanish
  does; f_65 counts sentence-initial *et*, as Spanish does, and coordinated
  clauses in compound tenses or with a copula (*il est parti et elle est
  restée*), which were missed.
* Defects fixed (D-55):
  * Dictionary phrases containing *du/des/au/aux* never matched, because
    UDPipe splits them (*du coup* → *de le coup*); both forms now match
    (*du coup*, *au loin*, *au nord*, *du genre*).
  * f_20 counts *il n'y a*, *il y en a* and colloquial *y a*, and no longer
    counts locative *y* (*elle y a mangé*, *j'y ai pensé*).
  * f_38 no longer counts prepositions introducing an infinitive or gérondif
    (*facile de convaincre*, *avant de partir*, *en chantant*).
  * f_06–f_08 count all possessives: *mon/ma/mes/nos* and *son/sa/ses* were
    missing, as were *te* and *le mien/tien/sien*; the noun *son* and the verb
    *tiens* are not counted.
  * Multiword verb entries now match: *avoir l'air* (f_58), *se plaindre*
    (f_55), *donner des instructions* (f_57).
  * *Il fait froid* is recognized as impersonal (f_09) when UDPipe parses
    *faire* as a copula; f_37 no longer accepts *au moins que*.
* f_11 counts indefinite pronouns in pronoun use only, as in Spanish (D-52):
  the nouns `une personne` and `une chose`, determiner `quelque` and degree
  `tout petit` no longer count, and `quelque chose` counts once instead of
  twice. Pronominal `aucun`/`nul` (like Spanish ninguno, English "none") and
  `quelques-uns` (like Spanish alguno) are added.
* Demonstrative ce/c', ça, cela and ceci (D-50): f_10 now counts elided c'
  (`C'est vrai`, like "that's true"), not only ce; the ce of ce que/ce qui
  relatives (f_23, f_34) is not a demonstrative. f_08 counts personal
  pronouns only, so ce/ça, partitive en and generic on (like English "one")
  are no longer third-person pronouns. `C'est important de lire` stays f_09.
* f_63 counts avoir/être or modal pouvoir/devoir separated from its own
  participle or infinitive by an adverb (D-49): `a souvent mangé`, `n'a
  jamais vu` (like English "has never seen"), `peut facilement résoudre`
  (newly detected). The analytic negator alone (`n'a pas mangé`), the
  counterpart of English "not", no longer counts, nor does an inverted subject
  (`Est-elle déjà arrivée ?`), as in English and Spanish.
* f_62 counts preposition + adverb + infinitive only when the adverb normally
  follows the infinitive (D-28, D-48): `de vraiment comprendre`, `sans vraiment
  savoir`, `par toujours gagner`. Light adverbs that normally precede the
  infinitive (`pour mieux comprendre`, `sans trop savoir`) and negation alone
  (`de ne pas venir`) no longer count, and any preposition linked to the
  infinitive now qualifies (previously only à, de and pour).
* f_61 counts colloquial competitors of pied-piping, in parallel with the
  Spanish resumptive relatives (D-29, D-31, D-47): an orphan preposition with
  no complement (`la fille que je sors avec`), a resumptive tonic pronoun in
  the frames parler de, dépendre de, compter sur and se fier à (`un ami que je
  compte sur lui`), or resumptive en with parler, souvenir, rêver, occuper and
  dépendre (`le film que je t'en ai parlé`, competing with `dont`). One per
  relative; its que is no longer counted in f_30. Previously f_61 counted a
  preposition before qui/quoi, which is pied-piping and was also counted in
  f_33.
* f_60 counts reduced complement clauses that compete with a que-clause, as in
  Spanish (D-27, D-46): a bare infinitive or a finite clause without que under
  croire, penser, espérer, dire, affirmer and prétendre (`Je crois avoir
  compris`, `Il dit avoir vu le voleur`). Previously every complement clause
  without que counted, so ordinary infinitive complements (`Je veux partir`,
  `Il faut partir`, `Elle commence à comprendre`) were also counted as
  that-deletion. Infinitives also remain in f_24.
* f_59 counts colloquial reductions that compete with an available full form,
  as English contractions do and in parallel with Spanish (D-26, D-30, D-45):
  listed forms (`p'tit`, `m'sieur`, `chuis`, `j'sais`, `y'a`), subject `tu`
  reduced to `t'` (`t'as`, `t'avais`), subject `je` reduced to `j'` before a
  consonant (`j'pense`) and `il y a` without `il` (`y a`). Obligatory elision is
  not counted. Previously any apostrophe word tagged as a noun, verb or
  adjective counted, so results depended on tagging errors.
* Adverbial subordinators keep only unambiguous markers in f_35-f_37, as
  in English and Spanish (D-44): f_35 `parce que`; f_36 `bien que`,
  `quoique` (elided `quoiqu'` was missed); f_37 conditional `si` (including
  `même si`, like English "even if"), `à moins que` and `sauf si` (once, no
  longer also in f_38). `puisque`, `comme`, `encore que` and `à condition
  que` (once) are f_38; `car`, a coordinator like English "for", is not
  counted. Indirect-question `si` (`je me demande si`, `je ne sais pas si`)
  is no longer a conditional, nor a deleted `que` in f_60, and `que` inside
  these phrases is not an f_21 complementizer.
* f_42 is a residual category (D-15): adverbs inside place/time adverbials,
  downtoners, hedges and amplifiers are excluded; emphatics, conjuncts,
  discourse particles and negation are no longer excluded.
* Relatives form an exclusive partition (D-24/D-25), using the relative
  clause's attachment to a noun instead of the preceding word: qui (f_29), que
  (f_30), lequel-family subject/other (f_31/f_32), and pied-piping (f_33: dont,
  auquel/duquel, preposition + qui/lequel), which takes precedence. dont is no
  longer counted in both f_32 and f_33.
* f_12 excludes reflexive `se faire` (D-22); causative faire was already excluded.
* Dictionary and word-list entries get generated unaccented twins (`très` ->
  `tres`, `ça` -> `ca`), except where the twin is a different grammatical word
  (`là`/`la`, `sûr`/`sur`, `à`/`a`, including within phrases such as `tout à`);
  nominalization suffixes stay exact (D-34).

* Negation follows Tottie's synthetic/analytic distinction (D-41), parallel
  to Spanish. f_66 counts each negative word: aucun, nul, personne, rien,
  jamais, ni, aucunement, nullement, including in concord with ne (`Personne
  n'a jamais rien dit` = 3). f_67 counts the particle pas (also without ne,
  `c'est pas grave`), point or plus sharing a head with ne, and constituent non
  (`non négligeable`). ne alone (`ne ... que`, expletive ne) is not counted.
  personne, rien and jamais moved from f_67 to f_66; sans and guère are no
  longer negation (sans is deferred, as Spanish sin; guère is a downtoner).
  Articles (`une personne`, `un rien`) and non-negative jamais (`si jamais`,
  `à jamais`) are excluded.
* f_42 no longer counts the concord clitic ne, so `ne ... pas` adds one adverb,
  as English not and Spanish no do (D-42).

## Corrections that change results

* Occurrences are identified by document, sentence and word, so constructions
  in different sentences are no longer merged (f_02, f_17/f_18, f_63).
* f_02 no longer counts passive auxiliaries or nouns ending in -i/-u/-é
  (`C'est un ami`); the head must be a participle.
* f_06-f_08 are the union of dictionary forms and pronouns with the matching
  Person, counted once per word, instead of the larger of two document totals.
  Impersonal pronouns counted in f_09 are no longer also counted in f_08.
* f_09 matches the pronoun itself, not its lemma (UDPipe lemmatizes je, tu,
  nous and elle as il), and requires an impersonal predicate: weather verbs
  (including through a modal, `il pourrait pleuvoir`), faire with a weather
  complement, falloir, or an adjective, raising or impersonal verb with its own
  clausal complement. Referential `il dit que...` and `il semble fatigué` no
  longer count.
* f_10 counts demonstrative pronouns only (now including ça and ceci) rather
  than any pronoun in the general pronoun list; f_51 counts demonstrative
  determiners only.
* f_13 and f_23 exclude relative clauses, including pseudo-relatives
  (`je les ai vus qui arrivaient`); f_23 counts interrogatives introducing an
  embedded clause (direct questions are f_13) and *ce que/ce qui* free
  relatives that are a verb's argument (`Je ne sais pas ce qui s'est passé`),
  the French form of indirect "what" clauses.
* f_34 requires the relative to follow punctuation (`..., ce qui est
  étrange`); sentence-initial `Ce que tu dis est vrai` is not a sentence
  relative.
* f_20 counts each `il y a`, not one per sentence.
* f_21/f_22 use the dependency structure (the governing verb or adjective of
  the que-clause) instead of the preceding word.
* f_27/f_28 count participles modifying a noun without their own auxiliary or
  subject, so finite relative clauses are excluded.
* f_39 counts a multiword preposition once; f_40/f_41 use dependency relations,
  so postnominal attributive adjectives (`un livre utile`) and predicative
  adjectives after an adverb are counted; quantifier tout is not attributive.
* f_38 no longer recounts words of `bien que`, `à moins que` or `s'`.
* f_52-f_58 no longer add a dictionary count to the structural count.
* f_64 includes coordinated verbs sharing a subject.
* The donc/ensuite/vraiment disambiguation moves individual occurrences between
  features instead of adjusting document totals.
* `on` was missing from the pronoun list because YAML read it as `true`.

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

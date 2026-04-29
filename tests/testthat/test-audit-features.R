test_that("audit_features returns sentence chunks for a single feature", {
  tokens <- data.frame(
    doc_id = c("doc1", "doc1", "doc1", "doc1", "doc1", "doc1", "doc1", "doc1"),
    sentence_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    token_id = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L),
    token = c("Je", "suis", "heureux", ".", "Il", "y", "a", "."),
    lemma = c("je", "être", "heureux", ".", "il", "y", "avoir", "."),
    pos = c("PRON", "VERB", "ADJ", "PUNCT", "PRON", "PRON", "VERB", "PUNCT"),
    tag = c("PRON", "VERB", "ADJ", "PUNCT", "PRON", "PRON", "VERB", "PUNCT"),
    head_token_id = c(2L, 0L, 2L, 2L, 3L, 3L, 0L, 3L),
    dep_rel = c("nsubj", "root", "xcomp", "punct", "expl", "obj", "root", "punct"),
    morph = c("Person=1|PronType=Prs", "VerbForm=Fin|Tense=Pres", NA, NA,
              "PronType=Prs", NA, "VerbForm=Fin|Tense=Pres", NA),
    stringsAsFactors = FALSE
  )
  class(tokens) <- c("spacyr_parsed", "data.frame")

  result <- audit_features(tokens, feature = "f_19_be_main_verb")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$feature, "f_19_be_main_verb")
  expect_equal(result$category, "G")
  expect_equal(result$doc_id, "doc1")
  expect_equal(result$sentence_id, 1L)
  expect_equal(result$match_type, "exact")
  expect_equal(result$keyword, "suis")
  expect_match(result$chunk, "Je suis heureux")
})

test_that("audit_features expands categories and preserves per-feature counts", {
  tokens <- data.frame(
    doc_id = c("doc1", "doc1", "doc1", "doc1", "doc1", "doc1", "doc1", "doc1"),
    sentence_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L),
    token_id = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L),
    token = c("Je", "suis", "heureux", ".", "Il", "y", "a", "."),
    lemma = c("je", "être", "heureux", ".", "il", "y", "avoir", "."),
    pos = c("PRON", "VERB", "ADJ", "PUNCT", "PRON", "PRON", "VERB", "PUNCT"),
    tag = c("PRON", "VERB", "ADJ", "PUNCT", "PRON", "PRON", "VERB", "PUNCT"),
    head_token_id = c(2L, 0L, 2L, 2L, 3L, 3L, 0L, 3L),
    dep_rel = c("nsubj", "root", "xcomp", "punct", "expl", "obj", "root", "punct"),
    morph = c("Person=1|PronType=Prs", "VerbForm=Fin|Tense=Pres", NA, NA,
              "PronType=Prs", NA, "VerbForm=Fin|Tense=Pres", NA),
    stringsAsFactors = FALSE
  )
  class(tokens) <- c("spacyr_parsed", "data.frame")

  result <- audit_features(tokens, category = "G")

  expect_equal(sort(unique(result$feature)), c("f_19_be_main_verb", "f_20_existential_there"))
  expect_equal(nrow(result), 2)
  expect_true(all(result$count == 1))
  expect_true(all(result$match_type == "exact"))
})

test_that("audit_features returns exact KWIC rows for other nouns", {
  tokens <- data.frame(
    doc_id = c("doc1", "doc1", "doc1", "doc1", "doc1", "doc1"),
    sentence_id = c(1L, 1L, 1L, 1L, 1L, 1L),
    token_id = c(1L, 2L, 3L, 4L, 5L, 6L),
    token = c("Le", "chat", "observe", "la", "ville", "."),
    lemma = c("le", "chat", "observer", "le", "ville", "."),
    pos = c("DET", "NOUN", "VERB", "DET", "NOUN", "PUNCT"),
    tag = c("DET", "NOUN", "VERB", "DET", "NOUN", "PUNCT"),
    head_token_id = c(2L, 3L, 0L, 5L, 3L, 3L),
    dep_rel = c("det", "nsubj", "root", "det", "obj", "punct"),
    morph = c(NA, NA, "VerbForm=Fin|Tense=Pres", NA, NA, NA),
    stringsAsFactors = FALSE
  )
  class(tokens) <- c("spacyr_parsed", "data.frame")

  result <- audit_features(tokens, feature = "f_16_other_nouns", sample_n = 1, seed = 1)

  expect_equal(nrow(result), 1)
  expect_equal(result$match_type, "exact")
  expect_true(result$keyword %in% c("chat", "ville"))
  expect_true(nchar(result$left) >= 0)
  expect_true(nchar(result$right) >= 0)
})

test_that("audit_features supports random sampling", {
  tokens <- data.frame(
    doc_id = c(rep("doc1", 4), rep("doc2", 4)),
    sentence_id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
    token_id = c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L),
    token = c("Je", "suis", "calme", ".", "Tu", "es", "prêt", "."),
    lemma = c("je", "être", "calme", ".", "tu", "être", "prêt", "."),
    pos = c("PRON", "VERB", "ADJ", "PUNCT", "PRON", "VERB", "ADJ", "PUNCT"),
    tag = c("PRON", "VERB", "ADJ", "PUNCT", "PRON", "VERB", "ADJ", "PUNCT"),
    head_token_id = c(2L, 0L, 2L, 2L, 2L, 0L, 2L, 2L),
    dep_rel = c("nsubj", "root", "xcomp", "punct", "nsubj", "root", "xcomp", "punct"),
    morph = c("Person=1|PronType=Prs", "VerbForm=Fin|Tense=Pres", NA, NA,
              "Person=2|PronType=Prs", "VerbForm=Fin|Tense=Pres", NA, NA),
    stringsAsFactors = FALSE
  )
  class(tokens) <- c("spacyr_parsed", "data.frame")

  result <- audit_features(tokens, feature = "f_19_be_main_verb", sample_n = 1, seed = 1)

  expect_equal(nrow(result), 1)
  expect_true(result$doc_id %in% c("doc1", "doc2"))
})

test_that("audit_features rejects lexical summary features", {
  expect_error(
    audit_features(spacy_samples, feature = "f_43_type_token"),
    "not supported"
  )
})

test_that("audit_features returns exact lexical-class anchors", {
  tokens <- data.frame(
    doc_id = c(rep("doc1", 6), rep("doc2", 4), rep("doc3", 4), rep("doc4", 4), rep("doc5", 6)),
    sentence_id = c(rep(1L, 6), rep(1L, 4), rep(1L, 4), rep(1L, 4), rep(1L, 6)),
    token_id = c(1:6, 1:4, 1:4, 1:4, 1:6),
    token = c(
      "Donc", ",", "c", "est", "utile", ".",
      "Il", "est", "peut-être", "prêt",
      "C", "est", "vraiment", "utile",
      "Eh", "bien", ",", "oui",
      "Il", "part", ",", "ensuite", "revient", "."
    ),
    lemma = c(
      "donc", ",", "ce", "être", "utile", ".",
      "il", "être", "peut-être", "prêt",
      "ce", "être", "vraiment", "utile",
      "eh", "bien", ",", "oui",
      "il", "partir", ",", "ensuite", "revenir", "."
    ),
    pos = c(
      "ADV", "PUNCT", "PRON", "VERB", "ADJ", "PUNCT",
      "PRON", "VERB", "ADV", "ADJ",
      "PRON", "VERB", "ADV", "ADJ",
      "INTJ", "ADV", "PUNCT", "INTJ",
      "PRON", "VERB", "PUNCT", "ADV", "VERB", "PUNCT"
    ),
    tag = c(
      "ADV", "PUNCT", "PRON", "VERB", "ADJ", "PUNCT",
      "PRON", "VERB", "ADV", "ADJ",
      "PRON", "VERB", "ADV", "ADJ",
      "INTJ", "ADV", "PUNCT", "INTJ",
      "PRON", "VERB", "PUNCT", "ADV", "VERB", "PUNCT"
    ),
    head_token_id = c(4L, 4L, 4L, 0L, 4L, 4L, 2L, 0L, 4L, 2L, 2L, 0L, 4L, 2L, 2L, 0L, 2L, 2L, 2L, 0L, 2L, 2L, 4L, 2L),
    dep_rel = c(
      "advmod", "punct", "nsubj", "root", "xcomp", "punct",
      "nsubj", "root", "advmod", "xcomp",
      "nsubj", "root", "advmod", "xcomp",
      "discourse", "discourse", "punct", "root",
      "nsubj", "root", "punct", "advmod", "conj", "punct"
    ),
    morph = c(
      NA, NA, "PronType=Dem", "VerbForm=Fin|Tense=Pres", NA, NA,
      "PronType=Prs", "VerbForm=Fin|Tense=Pres", NA, NA,
      "PronType=Dem", "VerbForm=Fin|Tense=Pres", NA, NA,
      NA, NA, NA, NA,
      "PronType=Prs", "VerbForm=Fin|Tense=Pres", NA, NA, "VerbForm=Fin|Tense=Pres", NA
    ),
    stringsAsFactors = FALSE
  )
  class(tokens) <- c("spacyr_parsed", "data.frame")

  result <- audit_features(
    tokens,
    feature = c("f_45_conjuncts", "f_47_hedges", "f_48_amplifiers", "f_50_discourse_particles")
  )

  expect_true(all(result$match_type == "exact"))
  expect_true(all(c("f_45_conjuncts", "f_47_hedges", "f_48_amplifiers", "f_50_discourse_particles") %in% result$feature))
  expect_true(any(result$keyword == "ensuite"))
  expect_true(any(result$keyword == "Donc"))
  expect_true(any(result$keyword == "peut-être"))
  expect_true(any(result$keyword == "vraiment"))
  expect_true(any(result$keyword == "Eh bien"))
})

test_that("audit_features returns exact passive anchors", {
  tokens <- data.frame(
    doc_id = c(rep("doc1", 6), rep("doc2", 8)),
    sentence_id = c(rep(1L, 6), rep(1L, 8)),
    token_id = c(1:6, 1:8),
    token = c(
      "Le", "rapport", "a", "été", "rédigé", ".",
      "Le", "rapport", "a", "été", "rédigé", "par", "Marie", "."
    ),
    lemma = c(
      "le", "rapport", "avoir", "être", "rédiger", ".",
      "le", "rapport", "avoir", "être", "rédiger", "par", "Marie", "."
    ),
    pos = c(
      "DET", "NOUN", "AUX", "AUX", "VERB", "PUNCT",
      "DET", "NOUN", "AUX", "AUX", "VERB", "ADP", "PROPN", "PUNCT"
    ),
    tag = c(
      "DET", "NOUN", "AUX", "AUX", "VERB", "PUNCT",
      "DET", "NOUN", "AUX", "AUX", "VERB", "ADP", "PROPN", "PUNCT"
    ),
    head_token_id = c(2L, 5L, 5L, 5L, 0L, 5L, 2L, 5L, 5L, 5L, 0L, 5L, 6L, 5L),
    dep_rel = c(
      "det", "nsubj:pass", "aux", "aux:pass", "root", "punct",
      "det", "nsubj:pass", "aux", "aux:pass", "root", "case", "obl", "punct"
    ),
    morph = c(
      NA, NA, "VerbForm=Fin|Tense=Pres", "VerbForm=Part|Tense=Past", "VerbForm=Part|Tense=Past", NA,
      NA, NA, "VerbForm=Fin|Tense=Pres", "VerbForm=Part|Tense=Past", "VerbForm=Part|Tense=Past", NA, NA, NA
    ),
    stringsAsFactors = FALSE
  )
  class(tokens) <- c("spacyr_parsed", "data.frame")

  result <- audit_features(tokens, category = "F")

  expect_true(all(result$match_type == "exact"))
  expect_true(all(c("f_17_agentless_passives", "f_18_by_passives") %in% result$feature))
  expect_true(any(grepl("été rédigé", result$keyword)))
})

test_that("audit_features returns exact modal anchors", {
  tokens <- data.frame(
    doc_id = c(rep("doc1", 4), rep("doc2", 5), rep("doc3", 3)),
    sentence_id = c(rep(1L, 4), rep(1L, 5), rep(1L, 3)),
    token_id = c(1:4, 1:5, 1:3),
    token = c(
      "Il", "peut", "venir", ".",
      "Il", "va", "partir", "demain", ".",
      "Il", "faut", "."
    ),
    lemma = c(
      "il", "pouvoir", "venir", ".",
      "il", "aller", "partir", "demain", ".",
      "il", "falloir", "."
    ),
    pos = c(
      "PRON", "VERB", "VERB", "PUNCT",
      "PRON", "VERB", "VERB", "ADV", "PUNCT",
      "PRON", "VERB", "PUNCT"
    ),
    tag = c(
      "PRON", "VERB", "VERB", "PUNCT",
      "PRON", "VERB", "VERB", "ADV", "PUNCT",
      "PRON", "VERB", "PUNCT"
    ),
    head_token_id = c(2L, 0L, 2L, 2L, 2L, 0L, 2L, 2L, 2L, 2L, 0L, 2L),
    dep_rel = c(
      "nsubj", "root", "xcomp", "punct",
      "nsubj", "root", "xcomp", "advmod", "punct",
      "expl", "root", "punct"
    ),
    morph = c(
      "PronType=Prs", "VerbForm=Fin|Tense=Pres", "VerbForm=Inf", NA,
      "PronType=Prs", "VerbForm=Fin|Tense=Pres", "VerbForm=Inf", NA, NA,
      "PronType=Prs", "VerbForm=Fin|Tense=Pres", NA
    ),
    stringsAsFactors = FALSE
  )
  class(tokens) <- c("spacyr_parsed", "data.frame")

  result <- audit_features(tokens, category = "L")

  expect_true(all(result$match_type == "exact"))
  expect_true(all(c("f_52_modal_possibility", "f_53_modal_necessity", "f_54_modal_predictive") %in% result$feature))
  expect_true(any(result$keyword == "peut"))
  expect_true(any(result$keyword == "va partir"))
  expect_true(any(result$keyword == "faut"))
})

test_that("audit_features returns exact relative anchors", {
  tokens <- data.frame(
    doc_id = c(rep("doc1", 6), rep("doc2", 7), rep("doc3", 6)),
    sentence_id = c(rep(1L, 6), rep(1L, 7), rep(1L, 6)),
    token_id = c(1:6, 1:7, 1:6),
    token = c(
      "le", "chien", "qui", "court", ".", "!",
      "la", "personne", "dont", "je", "parle", ".", "!",
      "il", "part", ",", "ce", "qui", "surprend"
    ),
    lemma = c(
      "le", "chien", "qui", "courir", ".", "!",
      "le", "personne", "dont", "je", "parler", ".", "!",
      "il", "partir", ",", "ce", "qui", "surprendre"
    ),
    pos = c(
      "DET", "NOUN", "PRON", "VERB", "PUNCT", "PUNCT",
      "DET", "NOUN", "PRON", "PRON", "VERB", "PUNCT", "PUNCT",
      "PRON", "VERB", "PUNCT", "PRON", "PRON", "VERB"
    ),
    tag = c(
      "DET", "NOUN", "PRON", "VERB", "PUNCT", "PUNCT",
      "DET", "NOUN", "PRON", "PRON", "VERB", "PUNCT", "PUNCT",
      "PRON", "VERB", "PUNCT", "PRON", "PRON", "VERB"
    ),
    head_token_id = c(2L, 4L, 4L, 0L, 4L, 4L, 2L, 5L, 5L, 5L, 0L, 5L, 5L, 2L, 0L, 2L, 6L, 6L, 0L),
    dep_rel = c(
      "det", "nsubj", "nsubj", "acl:relcl", "punct", "punct",
      "det", "obl", "obl", "nsubj", "acl:relcl", "punct", "punct",
      "nsubj", "root", "punct", "obj", "nsubj", "acl:relcl"
    ),
    morph = c(
      NA, NA, "PronType=Rel", "VerbForm=Fin|Tense=Pres", NA, NA,
      NA, NA, "PronType=Rel", "PronType=Prs", "VerbForm=Fin|Tense=Pres", NA, NA,
      "PronType=Prs", "VerbForm=Fin|Tense=Pres", NA, "PronType=Dem", "PronType=Rel", "VerbForm=Fin|Tense=Pres"
    ),
    stringsAsFactors = FALSE
  )
  class(tokens) <- c("spacyr_parsed", "data.frame")

  result <- audit_features(tokens, feature = c("f_29_that_subj", "f_33_pied_piping", "f_34_sentence_relatives"))

  expect_true(all(result$match_type == "exact"))
  expect_true(all(c("f_29_that_subj", "f_33_pied_piping", "f_34_sentence_relatives") %in% result$feature))
  expect_true(any(result$keyword == "qui"))
  expect_true(any(result$keyword == "dont"))
  expect_true(any(result$keyword == "ce qui"))
})

test_that("audit_features returns exact negation and clause anchors", {
  tokens <- data.frame(
    doc_id = c(rep("doc1", 4), rep("doc2", 4), rep("doc3", 4), rep("doc4", 3), rep("doc5", 5)),
    sentence_id = c(rep(1L, 4), rep(1L, 4), rep(1L, 4), rep(1L, 3), rep(1L, 5)),
    token_id = c(1:4, 1:4, 1:4, 1:3, 1:5),
    token = c(
      "Je", "pense", "que", "viens",
      "Il", "ne", "vient", "pas",
      "Je", "pars", "parce", "que",
      "Aucun", "choix", ".",
      "Je", "pars", ",", "cependant", "."
    ),
    lemma = c(
      "je", "penser", "que", "venir",
      "il", "ne", "venir", "pas",
      "je", "partir", "parce", "que",
      "aucun", "choix", ".",
      "je", "partir", ",", "cependant", "."
    ),
    pos = c(
      "PRON", "VERB", "SCONJ", "VERB",
      "PRON", "PART", "VERB", "ADV",
      "PRON", "VERB", "SCONJ", "SCONJ",
      "DET", "NOUN", "PUNCT",
      "PRON", "VERB", "PUNCT", "SCONJ", "PUNCT"
    ),
    tag = c(
      "PRON", "VERB", "SCONJ", "VERB",
      "PRON", "PART", "VERB", "ADV",
      "PRON", "VERB", "SCONJ", "SCONJ",
      "DET", "NOUN", "PUNCT",
      "PRON", "VERB", "PUNCT", "SCONJ", "PUNCT"
    ),
    head_token_id = c(2L, 0L, 4L, 2L, 3L, 3L, 0L, 3L, 2L, 0L, 4L, 2L, 2L, 0L, 2L, 0L, 2L, 2L, 2L, 2L),
    dep_rel = c(
      "nsubj", "root", "mark", "ccomp",
      "nsubj", "advmod", "root", "advmod",
      "nsubj", "root", "mark", "fixed",
      "det", "root", "punct",
      "nsubj", "root", "punct", "mark", "punct"
    ),
    morph = c(
      "PronType=Prs", "VerbForm=Fin|Tense=Pres", NA, "VerbForm=Fin|Tense=Pres",
      "PronType=Prs", NA, "VerbForm=Fin|Tense=Pres", NA,
      "PronType=Prs", "VerbForm=Fin|Tense=Pres", NA, NA,
      NA, NA, NA,
      "PronType=Prs", "VerbForm=Fin|Tense=Pres", NA, NA, NA
    ),
    stringsAsFactors = FALSE
  )
  class(tokens) <- c("spacyr_parsed", "data.frame")

  result <- audit_features(tokens, feature = c("f_21_that_verb_comp", "f_35_because", "f_38_other_adv_sub", "f_66_neg_synthetic", "f_67_neg_analytic"))

  expect_true(all(result$match_type == "exact"))
  expect_true(all(c("f_21_that_verb_comp", "f_35_because", "f_38_other_adv_sub", "f_66_neg_synthetic", "f_67_neg_analytic") %in% result$feature))
  expect_true(any(result$keyword == "que"))
  expect_true(any(result$keyword == "parce que"))
  expect_true(any(result$keyword == "cependant"))
  expect_true(any(result$keyword == "Aucun"))
  analytic_keywords <- result$keyword[result$feature == "f_67_neg_analytic"]
  expect_true(any(grepl("ne", analytic_keywords) & grepl("pas", analytic_keywords)))
})

test_that("audit_features returns exact coordination and split anchors", {
  tokens <- data.frame(
    doc_id = c(rep("doc1", 4), rep("doc2", 4), rep("doc3", 4), rep("doc4", 5), rep("doc5", 5)),
    sentence_id = c(rep(1L, 4), rep(1L, 4), rep(1L, 4), rep(1L, 5), rep(1L, 5)),
    token_id = c(1:4, 1:4, 1:4, 1:5, 1:5),
    token = c(
      "avec", "qui", "viens", ".",
      "de", "vraiment", "comprendre", ".",
      "a", "probablement", "été", "vu",
      "pommes", "et", "oranges", ".", ".",
      "il", "vient", "et", "elle", "part"
    ),
    lemma = c(
      "avec", "qui", "venir", ".",
      "de", "vraiment", "comprendre", ".",
      "avoir", "probablement", "être", "voir",
      "pomme", "et", "orange", ".", ".",
      "il", "venir", "et", "elle", "partir"
    ),
    pos = c(
      "ADP", "PRON", "VERB", "PUNCT",
      "ADP", "ADV", "VERB", "PUNCT",
      "AUX", "ADV", "AUX", "VERB",
      "NOUN", "CCONJ", "NOUN", "PUNCT", "PUNCT",
      "PRON", "VERB", "CCONJ", "PRON", "VERB"
    ),
    tag = c(
      "ADP", "PRON", "VERB", "PUNCT",
      "ADP", "ADV", "VERB", "PUNCT",
      "AUX", "ADV", "AUX", "VERB",
      "NOUN", "CCONJ", "NOUN", "PUNCT", "PUNCT",
      "PRON", "VERB", "CCONJ", "PRON", "VERB"
    ),
    head_token_id = c(2L, 3L, 0L, 3L, 3L, 3L, 0L, 3L, 4L, 4L, 4L, 0L, 0L, 3L, 1L, 3L, 3L, 2L, 0L, 5L, 5L, 2L),
    dep_rel = c(
      "case", "nsubj", "root", "punct",
      "mark", "advmod", "xcomp", "punct",
      "aux", "advmod", "aux:pass", "root",
      "root", "cc", "conj", "punct", "punct",
      "nsubj", "root", "cc", "nsubj", "conj"
    ),
    morph = c(
      NA, "PronType=Rel", "VerbForm=Fin|Tense=Pres", NA,
      NA, NA, "VerbForm=Inf", NA,
      "VerbForm=Fin|Tense=Pres", NA, "VerbForm=Part|Tense=Past", "VerbForm=Part|Tense=Past",
      NA, NA, NA, NA, NA,
      "PronType=Prs", "VerbForm=Fin|Tense=Pres", NA, "PronType=Prs", "VerbForm=Fin|Tense=Pres"
    ),
    stringsAsFactors = FALSE
  )
  class(tokens) <- c("spacyr_parsed", "data.frame")

  result <- audit_features(tokens, feature = c("f_61_stranded_preposition", "f_62_split_infinitive", "f_63_split_auxiliary", "f_64_phrasal_coordination", "f_65_clausal_coordination"))

  expect_true(all(result$match_type == "exact"))
  expect_true(all(c("f_61_stranded_preposition", "f_62_split_infinitive", "f_63_split_auxiliary", "f_64_phrasal_coordination", "f_65_clausal_coordination") %in% result$feature))
  expect_true(any(grepl("avec qui", result$keyword)))
  expect_true(any(result$feature == "f_62_split_infinitive"))
  expect_true(any(result$feature == "f_63_split_auxiliary"))
  expect_true(any(result$feature == "f_64_phrasal_coordination"))
  expect_true(any(result$feature == "f_65_clausal_coordination"))
})

test_that("audit_features has a compact print helper", {
  result <- audit_features(spacy_samples, feature = "f_19_be_main_verb", sample_n = 1, seed = 1)

  output <- paste(capture.output(print(result, n = 1)), collapse = "\n")

  expect_s3_class(result, "audit_features_result")
  expect_match(output, "audit_features_result")
  expect_match(output, "exact|sentence")
})
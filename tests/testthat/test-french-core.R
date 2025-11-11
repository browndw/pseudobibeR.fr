test_that("synthetic French tokens cover f01-f08", {
  synthetic_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "doc1", 1L, 1L, "Nous", "nous", "PRON", "PRON", "nsubj", 3L, "PronType=Prs|Person=1|Number=Plur",
    "doc1", 1L, 2L, "avons", "avoir", "AUX", "AUX", "aux", 3L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "doc1", 1L, 3L, "fini", "finir", "VERB", "VERB", "root", NA_integer_, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Sing",
    "doc1", 1L, 4L, "loin", "loin", "ADV", "ADV", "advmod", 3L, NA_character_,
    "doc1", 2L, 5L, "Autrefois", "autrefois", "ADV", "ADV", "advmod", 7L, NA_character_,
    "doc1", 2L, 6L, "nous", "nous", "PRON", "PRON", "nsubj", 7L, "PronType=Prs|Person=1|Number=Plur",
    "doc1", 2L, 7L, "mangions", "manger", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=1|Tense=Imp|VerbForm=Fin",
    "doc1", 2L, 8L, ".", ".", "PUNCT", "PUNCT", "punct", 7L, NA_character_,
    "doc2", 1L, 1L, "Vous", "vous", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=2|Number=Plur",
    "doc2", 1L, 2L, "allez", "aller", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=2|Tense=Pres|VerbForm=Fin",
    "doc2", 1L, 3L, "demain", "demain", "ADV", "ADV", "advmod", 2L, NA_character_,
    "doc2", 1L, 4L, "et", "et", "CCONJ", "CCONJ", "cc", 2L, NA_character_,
    "doc2", 1L, 5L, "elle", "elle", "PRON", "PRON", "nsubj", 6L, "PronType=Prs|Person=3|Gender=Fem|Number=Sing",
    "doc2", 1L, 6L, "parle", "parler", "VERB", "VERB", "conj", 2L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "doc2", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 6L, NA_character_
  )

  synthetic_tokens$token <- stringr::str_to_lower(synthetic_tokens$token)
  synthetic_tokens$lemma <- stringr::str_to_lower(synthetic_tokens$lemma)
  synthetic_tokens$tag <- stringr::str_to_upper(synthetic_tokens$tag)

  class(synthetic_tokens) <- c("spacyr_parsed", class(synthetic_tokens))

  features <- biber(synthetic_tokens, measure = "none", normalize = FALSE)

  doc1 <- features[features$doc_id == "doc1", , drop = FALSE]
  doc2 <- features[features$doc_id == "doc2", , drop = FALSE]

  expect_equal(doc1$f_01_past_tense, 1)
  expect_equal(doc1$f_02_perfect_aspect, 1)
  expect_equal(doc1$f_03_present_tense, 0)
  expect_equal(doc1$f_04_place_adverbials, 1)
  expect_equal(doc1$f_05_time_adverbials, 1)
  expect_equal(doc1$f_06_first_person_pronouns, 2)
  expect_equal(doc1$f_07_second_person_pronouns, 0)
  expect_equal(doc1$f_08_third_person_pronouns, 0)

  expect_equal(doc2$f_01_past_tense, 0)
  expect_equal(doc2$f_02_perfect_aspect, 0)
  expect_equal(doc2$f_03_present_tense, 2)
  expect_equal(doc2$f_04_place_adverbials, 0)
  expect_equal(doc2$f_05_time_adverbials, 1)
  expect_equal(doc2$f_06_first_person_pronouns, 0)
  expect_equal(doc2$f_07_second_person_pronouns, 1)
  expect_equal(doc2$f_08_third_person_pronouns, 1)
})

test_that("spaCy morph column fills feats when absent", {
  morph_only_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~morph,
    "docA", 1L, 1L, "Nous", "nous", "PRON", "PRON", "nsubj", 3L, "PronType=Prs|Person=1|Number=Plur",
    "docA", 1L, 2L, "avons", "avoir", "AUX", "AUX", "aux", 3L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docA", 1L, 3L, "fini", "finir", "VERB", "VERB", "root", NA_integer_, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Sing",
    "docA", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docA", 2L, 5L, "Nous", "nous", "PRON", "PRON", "nsubj", 6L, "PronType=Prs|Person=1|Number=Plur",
    "docA", 2L, 6L, "travaillons", "travailler", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docA", 2L, 7L, "encore", "encore", "ADV", "ADV", "advmod", 6L, NA_character_,
    "docA", 2L, 8L, ".", ".", "PUNCT", "PUNCT", "punct", 6L, NA_character_,
    "docB", 1L, 1L, "Ils", "ils", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Number=Plur",
    "docB", 1L, 2L, "chantaient", "chanter", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Imp|VerbForm=Fin",
    "docB", 1L, 3L, "hier", "hier", "ADV", "ADV", "advmod", 2L, NA_character_,
    "docB", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  morph_only_tokens$doc_id <- stringr::str_to_lower(morph_only_tokens$doc_id)
  morph_only_tokens$token <- stringr::str_to_lower(morph_only_tokens$token)
  morph_only_tokens$lemma <- stringr::str_to_lower(morph_only_tokens$lemma)
  morph_only_tokens$tag <- stringr::str_to_upper(morph_only_tokens$tag)

  class(morph_only_tokens) <- c("spacyr_parsed", class(morph_only_tokens))

  features <- biber(morph_only_tokens, measure = "none", normalize = FALSE)

  docA <- features[features$doc_id == "doca", , drop = FALSE]
  docB <- features[features$doc_id == "docb", , drop = FALSE]

  expect_equal(docA$f_01_past_tense, 0)
  expect_equal(docA$f_02_perfect_aspect, 1)
  expect_equal(docA$f_03_present_tense, 1)

  expect_equal(docB$f_01_past_tense, 1)
  expect_equal(docB$f_02_perfect_aspect, 0)
  expect_equal(docB$f_03_present_tense, 0)
})

test_that("demonstrative pronouns require morph dem tag", {
  demo_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docC", 1L, 1L, "Cela", "cela", "PRON", "PRON", "nsubj", 2L, "PronType=Dem|Person=3|Number=Sing",
    "docC", 1L, 2L, "fonctionne", "fonctionner", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docC", 1L, 3L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docD", 1L, 1L, "Nous", "nous", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Plur",
    "docD", 1L, 2L, "avons", "avoir", "AUX", "AUX", "aux", 3L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docD", 1L, 3L, "fini", "finir", "VERB", "VERB", "root", NA_integer_, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Sing",
    "docD", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_
  )

  demo_tokens$doc_id <- stringr::str_to_lower(demo_tokens$doc_id)
  demo_tokens$token <- stringr::str_to_lower(demo_tokens$token)
  demo_tokens$lemma <- stringr::str_to_lower(demo_tokens$lemma)
  demo_tokens$tag <- stringr::str_to_upper(demo_tokens$tag)

  class(demo_tokens) <- c("spacyr_parsed", class(demo_tokens))

  features <- biber(demo_tokens, measure = "none", normalize = FALSE)

  docC <- features[features$doc_id == "docc", , drop = FALSE]
  docD <- features[features$doc_id == "docd", , drop = FALSE]

  expect_equal(docC$f_10_demonstrative_pronoun, 1)
  expect_equal(docC$f_06_first_person_pronouns, 0)

  expect_equal(docD$f_10_demonstrative_pronoun, 0)
  expect_equal(docD$f_06_first_person_pronouns, 1)
})

test_that("passive counts handle aux:pass and French agents", {
  passive_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docE", 1L, 1L, "Les", "le", "DET", "DET", "det", 2L, NA_character_,
    "docE", 1L, 2L, "résultats", "résultat", "NOUN", "NOUN", "nsubj:pass", 4L, NA_character_,
    "docE", 1L, 3L, "sont", "être", "AUX", "AUX", "aux:pass", 4L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docE", 1L, 4L, "publiés", "publier", "VERB", "VERB", "root", NA_integer_, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Plur|Voice=Pass",
    "docE", 1L, 5L, "souvent", "souvent", "ADV", "ADV", "advmod", 4L, NA_character_,
    "docE", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 4L, NA_character_,
    "docF", 1L, 1L, "Les", "le", "DET", "DET", "det", 2L, NA_character_,
    "docF", 1L, 2L, "documents", "document", "NOUN", "NOUN", "nsubj:pass", 4L, NA_character_,
    "docF", 1L, 3L, "ont", "avoir", "AUX", "AUX", "aux:pass", 4L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docF", 1L, 4L, "été", "être", "AUX", "AUX", "aux:pass", 5L, "VerbForm=Part|Tense=Past|Voice=Pass",
    "docF", 1L, 5L, "validés", "valider", "VERB", "VERB", "root", NA_integer_, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Plur|Voice=Pass",
    "docF", 1L, 6L, "par", "par", "ADP", "ADP", "case", 7L, NA_character_,
    "docF", 1L, 7L, "l'équipe", "équipe", "NOUN", "NOUN", "obl", 5L, NA_character_,
    "docF", 1L, 8L, ".", ".", "PUNCT", "PUNCT", "punct", 5L, NA_character_
  )

  passive_tokens$doc_id <- stringr::str_to_lower(passive_tokens$doc_id)
  passive_tokens$token <- stringr::str_to_lower(passive_tokens$token)
  passive_tokens$lemma <- stringr::str_to_lower(passive_tokens$lemma)
  passive_tokens$tag <- stringr::str_to_upper(passive_tokens$tag)

  class(passive_tokens) <- c("spacyr_parsed", class(passive_tokens))

  features <- biber(passive_tokens, measure = "none", normalize = FALSE)

  docE <- features[features$doc_id == "doce", , drop = FALSE]
  docF <- features[features$doc_id == "docf", , drop = FALSE]

  expect_equal(docE$f_17_agentless_passives, 1)
  expect_equal(docE$f_18_by_passives, 0)

  expect_equal(docF$f_17_agentless_passives, 0)
  expect_equal(docF$f_18_by_passives, 1)
})

test_that("être main verb excludes auxiliaries", {
  etre_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docG", 1L, 1L, "Ils", "ils", "PRON", "PRON", "nsubj", 3L, "PronType=Prs|Person=3|Number=Plur",
    "docG", 1L, 2L, "sont", "être", "AUX", "AUX", "cop", 3L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docG", 1L, 3L, "heureux", "heureux", "ADJ", "ADJ", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docG", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docH", 1L, 1L, "Ils", "ils", "PRON", "PRON", "nsubj:pass", 4L, "PronType=Prs|Person=3|Number=Plur",
    "docH", 1L, 2L, "sont", "être", "AUX", "AUX", "aux:pass", 4L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docH", 1L, 3L, "été", "être", "AUX", "AUX", "aux:pass", 4L, "VerbForm=Part|Tense=Past|Voice=Pass",
    "docH", 1L, 4L, "aidés", "aider", "VERB", "VERB", "root", NA_integer_, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Plur|Voice=Pass",
    "docH", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 4L, NA_character_
  )

  etre_tokens$doc_id <- stringr::str_to_lower(etre_tokens$doc_id)
  etre_tokens$token <- stringr::str_to_lower(etre_tokens$token)
  etre_tokens$lemma <- stringr::str_to_lower(etre_tokens$lemma)
  etre_tokens$tag <- stringr::str_to_upper(etre_tokens$tag)

  class(etre_tokens) <- c("spacyr_parsed", class(etre_tokens))

  features <- biber(etre_tokens, measure = "none", normalize = FALSE)

  docG <- features[features$doc_id == "docg", , drop = FALSE]
  docH <- features[features$doc_id == "doch", , drop = FALSE]

  expect_equal(docG$f_19_be_main_verb, 1)
  expect_equal(docH$f_19_be_main_verb, 0)
})

test_that("French complementizers map to verb and adjective complements", {
  que_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docI", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docI", 1L, 2L, "crois", "croire", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docI", 1L, 3L, "que", "que", "SCONJ", "SCONJ", "mark", 5L, NA_character_,
    "docI", 1L, 4L, "nous", "nous", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=1|Number=Plur",
    "docI", 1L, 5L, "partons", "partir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docI", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docJ", 1L, 1L, "Ils", "ils", "PRON", "PRON", "nsubj", 3L, "PronType=Prs|Person=3|Number=Plur",
    "docJ", 1L, 2L, "sont", "être", "AUX", "AUX", "cop", 3L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docJ", 1L, 3L, "heureux", "heureux", "ADJ", "ADJ", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docJ", 1L, 4L, "que", "que", "SCONJ", "SCONJ", "mark", 6L, NA_character_,
    "docJ", 1L, 5L, "vous", "vous", "PRON", "PRON", "nsubj", 6L, "PronType=Prs|Person=2|Number=Plur",
    "docJ", 1L, 6L, "veniez", "venir", "VERB", "VERB", "ccomp", 3L, "Mood=Sub|Number=Plur|Person=2|Tense=Pres|VerbForm=Fin",
    "docJ", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docK", 1L, 1L, "Il", "il", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Gender=Masc|Number=Sing",
    "docK", 1L, 2L, "dit", "dire", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docK", 1L, 3L, "qu'", "que", "SCONJ", "SCONJ", "mark", 5L, NA_character_,
    "docK", 1L, 4L, "il", "il", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=3|Gender=Masc|Number=Sing",
    "docK", 1L, 5L, "part", "partir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docK", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  que_tokens$doc_id <- stringr::str_to_lower(que_tokens$doc_id)
  que_tokens$token <- stringr::str_to_lower(que_tokens$token)
  que_tokens$lemma <- stringr::str_to_lower(que_tokens$lemma)
  que_tokens$tag <- stringr::str_to_upper(que_tokens$tag)

  class(que_tokens) <- c("spacyr_parsed", class(que_tokens))

  features <- biber(que_tokens, measure = "none", normalize = FALSE)

  docI <- features[features$doc_id == "doci", , drop = FALSE]
  docJ <- features[features$doc_id == "docj", , drop = FALSE]
  docK <- features[features$doc_id == "dock", , drop = FALSE]

  expect_equal(docI$f_21_that_verb_comp, 1)
  expect_equal(docI$f_22_that_adj_comp, 0)

  expect_equal(docJ$f_21_that_verb_comp, 0)
  expect_equal(docJ$f_22_that_adj_comp, 1)

  expect_equal(docK$f_21_that_verb_comp, 1)
  expect_equal(docK$f_22_that_adj_comp, 0)
})

test_that("French wh clauses counted for f_23", {
  wh_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docL", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docL", 1L, 2L, "sais", "savoir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docL", 1L, 3L, "qui", "qui", "PRON", "PRON", "nsubj", 4L, "PronType=Int|Number=Sing",
    "docL", 1L, 4L, "vient", "venir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docL", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docM", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docM", 1L, 2L, "dis", "dire", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docM", 1L, 3L, "que", "que", "SCONJ", "SCONJ", "mark", 5L, NA_character_,
    "docM", 1L, 4L, "tu", "tu", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=2|Number=Sing",
    "docM", 1L, 5L, "viens", "venir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docM", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  wh_tokens$doc_id <- stringr::str_to_lower(wh_tokens$doc_id)
  wh_tokens$token <- stringr::str_to_lower(wh_tokens$token)
  wh_tokens$lemma <- stringr::str_to_lower(wh_tokens$lemma)
  wh_tokens$tag <- stringr::str_to_upper(wh_tokens$tag)

  class(wh_tokens) <- c("spacyr_parsed", class(wh_tokens))

  features <- biber(wh_tokens, measure = "none", normalize = FALSE)

  docL <- features[features$doc_id == "docl", , drop = FALSE]
  docM <- features[features$doc_id == "docm", , drop = FALSE]

  expect_equal(docL$f_23_wh_clause, 1)
  expect_equal(docM$f_23_wh_clause, 0)
})

test_that("French infinitives and participles counted", {
  clause_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docN", 1L, 1L, "Elle", "elle", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Gender=Fem|Number=Sing",
    "docN", 1L, 2L, "veut", "vouloir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docN", 1L, 3L, "partir", "partir", "VERB", "VERB", "xcomp", 2L, "VerbForm=Inf",
    "docN", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docO", 1L, 1L, "Parlant", "parler", "VERB", "VERB", "advcl", 4L, "VerbForm=Part|Tense=Pres|Gender=Masc|Number=Sing",
    "docO", 1L, 2L, ",", ",", "PUNCT", "PUNCT", "punct", 1L, NA_character_,
    "docO", 1L, 3L, "il", "il", "PRON", "PRON", "nsubj", 4L, "PronType=Prs|Person=3|Gender=Masc|Number=Sing",
    "docO", 1L, 4L, "écoute", "écouter", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docO", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 4L, NA_character_,
    "docP", 1L, 1L, "Né", "naître", "VERB", "VERB", "advcl", 6L, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Sing",
    "docP", 1L, 2L, "en", "en", "ADP", "ADP", "case", 3L, NA_character_,
    "docP", 1L, 3L, "1990", "1990", "NUM", "NUM", "obl", 1L, "NumType=Card",
    "docP", 1L, 4L, ",", ",", "PUNCT", "PUNCT", "punct", 1L, NA_character_,
    "docP", 1L, 5L, "il", "il", "PRON", "PRON", "nsubj", 6L, "PronType=Prs|Person=3|Gender=Masc|Number=Sing",
    "docP", 1L, 6L, "vit", "vivre", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docP", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 6L, NA_character_,
    "docQ", 1L, 1L, "Les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docQ", 1L, 2L, "documents", "document", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docQ", 1L, 3L, "publiés", "publier", "VERB", "VERB", "acl", 2L, "VerbForm=Part|Tense=Past|Gender=Masc|Number=Plur",
    "docQ", 1L, 4L, "hier", "hier", "ADV", "ADV", "advmod", 3L, NA_character_,
    "docQ", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docR", 1L, 1L, "Les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docR", 1L, 2L, "étudiants", "étudiant", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docR", 1L, 3L, "travaillant", "travailler", "VERB", "VERB", "acl", 2L, "VerbForm=Part|Tense=Pres|Gender=Masc|Number=Plur",
    "docR", 1L, 4L, "tard", "tard", "ADV", "ADV", "advmod", 3L, NA_character_,
    "docR", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  clause_tokens$doc_id <- stringr::str_to_lower(clause_tokens$doc_id)
  clause_tokens$token <- stringr::str_to_lower(clause_tokens$token)
  clause_tokens$lemma <- stringr::str_to_lower(clause_tokens$lemma)
  clause_tokens$tag <- stringr::str_to_upper(clause_tokens$tag)

  class(clause_tokens) <- c("spacyr_parsed", class(clause_tokens))

  features <- biber(clause_tokens, measure = "none", normalize = FALSE)

  docN <- features[features$doc_id == "docn", , drop = FALSE]
  docO <- features[features$doc_id == "doco", , drop = FALSE]
  docP <- features[features$doc_id == "docp", , drop = FALSE]
  docQ <- features[features$doc_id == "docq", , drop = FALSE]
  docR <- features[features$doc_id == "docr", , drop = FALSE]

  expect_equal(docN$f_24_infinitives, 1)
  expect_equal(docN$f_25_present_participle, 0)
  expect_equal(docN$f_26_past_participle, 0)

  expect_equal(docO$f_24_infinitives, 0)
  expect_equal(docO$f_25_present_participle, 1)
  expect_equal(docO$f_26_past_participle, 0)

  expect_equal(docP$f_24_infinitives, 0)
  expect_equal(docP$f_25_present_participle, 0)
  expect_equal(docP$f_26_past_participle, 1)

  expect_equal(docQ$f_27_past_participle_whiz, 1)
  expect_equal(docQ$f_28_present_participle_whiz, 0)

  expect_equal(docR$f_27_past_participle_whiz, 0)
  expect_equal(docR$f_28_present_participle_whiz, 1)
})

test_that("French relative clauses counted", {
  relative_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docS", 1L, 1L, "les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docS", 1L, 2L, "etudiants", "etudiant", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docS", 1L, 3L, "qui", "qui", "PRON", "PRON", "nsubj", 4L, "PronType=Rel|Number=Plur",
    "docS", 1L, 4L, "arrivent", "arriver", "VERB", "VERB", "acl:relcl", 2L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docS", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docT", 1L, 1L, "le", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Sing|PronType=Art",
    "docT", 1L, 2L, "livre", "livre", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Sing",
    "docT", 1L, 3L, "que", "que", "PRON", "PRON", "obj", 5L, "PronType=Rel",
    "docT", 1L, 4L, "nous", "nous", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=1|Number=Plur",
    "docT", 1L, 5L, "lisons", "lire", "VERB", "VERB", "acl:relcl", 2L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docT", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docU", 1L, 1L, "les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docU", 1L, 2L, "projets", "projet", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docU", 1L, 3L, "lesquels", "lequel", "PRON", "PRON", "nsubj", 4L, "PronType=Rel|Number=Plur",
    "docU", 1L, 4L, "avancent", "avancer", "VERB", "VERB", "acl:relcl", 2L, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docU", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docV", 1L, 1L, "les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docV", 1L, 2L, "tests", "test", "NOUN", "NOUN", "root", NA_integer_, "Gender=Masc|Number=Plur",
    "docV", 1L, 3L, "lesquels", "lequel", "PRON", "PRON", "obj", 5L, "PronType=Rel|Number=Plur",
    "docV", 1L, 4L, "nous", "nous", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=1|Number=Plur",
    "docV", 1L, 5L, "choisissons", "choisir", "VERB", "VERB", "acl:relcl", 2L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docV", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docW", 1L, 1L, "les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docW", 1L, 2L, "salles", "salle", "NOUN", "NOUN", "root", NA_integer_, "Gender=Fem|Number=Plur",
    "docW", 1L, 3L, "dans", "dans", "ADP", "ADP", "case", 4L, NA_character_,
    "docW", 1L, 4L, "lesquelles", "lequel", "PRON", "PRON", "obl", 6L, "PronType=Rel|Number=Plur",
    "docW", 1L, 5L, "nous", "nous", "PRON", "PRON", "nsubj", 6L, "PronType=Prs|Person=1|Number=Plur",
    "docW", 1L, 6L, "travaillons", "travailler", "VERB", "VERB", "acl:relcl", 2L, "Mood=Ind|Number=Plur|Person=1|Tense=Pres|VerbForm=Fin",
    "docW", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  relative_tokens$doc_id <- stringr::str_to_lower(relative_tokens$doc_id)
  relative_tokens$token <- stringr::str_to_lower(relative_tokens$token)
  relative_tokens$lemma <- stringr::str_to_lower(relative_tokens$lemma)
  relative_tokens$tag <- stringr::str_to_upper(relative_tokens$tag)

  class(relative_tokens) <- c("spacyr_parsed", class(relative_tokens))

  features <- biber(relative_tokens, measure = "none", normalize = FALSE)

  docS <- features[features$doc_id == "docs", , drop = FALSE]
  docT <- features[features$doc_id == "doct", , drop = FALSE]
  docU <- features[features$doc_id == "docu", , drop = FALSE]
  docV <- features[features$doc_id == "docv", , drop = FALSE]
  docW <- features[features$doc_id == "docw", , drop = FALSE]

  expect_equal(docS$f_29_that_subj, 1)
  expect_equal(docS$f_31_wh_subj, 0)

  expect_equal(docT$f_30_that_obj, 1)
  expect_equal(docT$f_32_wh_obj, 0)

  expect_equal(docU$f_29_that_subj, 0)
  expect_equal(docU$f_31_wh_subj, 1)

  expect_equal(docV$f_30_that_obj, 0)
  expect_equal(docV$f_32_wh_obj, 1)

  expect_equal(docW$f_33_pied_piping, 1)
  expect_equal(docW$f_32_wh_obj, 0)
})

test_that("French adverbial subordinators counted", {
  sub_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docSR", 1L, 1L, "Ils", "ils", "PRON", "PRON", "nsubj", 3L, "PronType=Prs|Person=3|Number=Plur",
    "docSR", 1L, 2L, "se", "se", "PRON", "PRON", "expl", 3L, "PronType=Prs|Reflex=Yes",
    "docSR", 1L, 3L, "taisent", "taire", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docSR", 1L, 4L, ",", ",", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docSR", 1L, 5L, "ce", "ce", "PRON", "PRON", "obj", 7L, "PronType=Dem|Number=Sing",
    "docSR", 1L, 6L, "qui", "qui", "PRON", "PRON", "nsubj", 7L, "PronType=Rel|Number=Sing",
    "docSR", 1L, 7L, "est", "être", "VERB", "VERB", "ccomp", 3L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docSR", 1L, 8L, "surprenant", "surprenant", "ADJ", "ADJ", "xcomp", 7L, "Gender=Masc|Number=Sing",
    "docSR", 1L, 9L, ".", ".", "PUNCT", "PUNCT", "punct", 7L, NA_character_,
    "docBC", 1L, 1L, "Ils", "ils", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Number=Plur",
    "docBC", 1L, 2L, "partent", "partir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docBC", 1L, 3L, "parce", "parce", "SCONJ", "SCONJ", "mark", 6L, NA_character_,
    "docBC", 1L, 4L, "qu'", "que", "SCONJ", "SCONJ", "mark", 6L, NA_character_,
    "docBC", 1L, 5L, "il", "il", "PRON", "PRON", "nsubj", 6L, "PronType=Prs|Person=3|Number=Sing",
    "docBC", 1L, 6L, "pleut", "pleuvoir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docBC", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docBC", 2L, 8L, "Ils", "ils", "PRON", "PRON", "nsubj", 9L, "PronType=Prs|Person=3|Number=Plur",
    "docBC", 2L, 9L, "restent", "rester", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docBC", 2L, 10L, "car", "car", "CCONJ", "CCONJ", "cc", 9L, NA_character_,
    "docBC", 2L, 11L, "il", "il", "PRON", "PRON", "nsubj", 12L, "PronType=Prs|Person=3|Number=Sing",
    "docBC", 2L, 12L, "adore", "adorer", "VERB", "VERB", "conj", 9L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docBC", 2L, 13L, "la", "le", "DET", "DET", "det", 14L, "Definite=Def|Gender=Fem|Number=Sing|PronType=Art",
    "docBC", 2L, 14L, "pluie", "pluie", "NOUN", "NOUN", "obj", 12L, "Gender=Fem|Number=Sing",
    "docBC", 2L, 15L, ".", ".", "PUNCT", "PUNCT", "punct", 9L, NA_character_,
    "docCONC", 1L, 1L, "Bien", "bien", "ADV", "ADV", "mark", 4L, "Degree=Pos",
    "docCONC", 1L, 2L, "qu'", "que", "SCONJ", "SCONJ", "mark", 4L, NA_character_,
    "docCONC", 1L, 3L, "il", "il", "PRON", "PRON", "nsubj", 4L, "PronType=Prs|Person=3|Number=Sing",
    "docCONC", 1L, 4L, "soit", "être", "VERB", "VERB", "advcl", 8L, "Mood=Sub|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docCONC", 1L, 5L, "tard", "tard", "ADV", "ADV", "xcomp", 4L, NA_character_,
    "docCONC", 1L, 6L, ",", ",", "PUNCT", "PUNCT", "punct", 4L, NA_character_,
    "docCONC", 1L, 7L, "ils", "ils", "PRON", "PRON", "nsubj", 8L, "PronType=Prs|Person=3|Number=Plur",
    "docCONC", 1L, 8L, "continuent", "continuer", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docCONC", 1L, 9L, ".", ".", "PUNCT", "PUNCT", "punct", 8L, NA_character_,
    "docCONC", 2L, 10L, "Quoique", "quoique", "SCONJ", "SCONJ", "mark", 12L, NA_character_,
    "docCONC", 2L, 11L, "elle", "elle", "PRON", "PRON", "nsubj", 12L, "PronType=Prs|Person=3|Gender=Fem|Number=Sing",
    "docCONC", 2L, 12L, "doute", "douter", "VERB", "VERB", "advcl", 15L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docCONC", 2L, 13L, ",", ",", "PUNCT", "PUNCT", "punct", 12L, NA_character_,
    "docCONC", 2L, 14L, "elle", "elle", "PRON", "PRON", "nsubj", 15L, "PronType=Prs|Person=3|Gender=Fem|Number=Sing",
    "docCONC", 2L, 15L, "avance", "avancer", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docCONC", 2L, 16L, ".", ".", "PUNCT", "PUNCT", "punct", 15L, NA_character_,
    "docCONC", 3L, 17L, "Même", "même", "ADV", "ADV", "mark", 20L, NA_character_,
    "docCONC", 3L, 18L, "si", "si", "SCONJ", "SCONJ", "mark", 20L, NA_character_,
    "docCONC", 3L, 19L, "tu", "tu", "PRON", "PRON", "nsubj", 20L, "PronType=Prs|Person=2|Number=Sing",
    "docCONC", 3L, 20L, "pars", "partir", "VERB", "VERB", "advcl", 23L, "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docCONC", 3L, 21L, ",", ",", "PUNCT", "PUNCT", "punct", 20L, NA_character_,
    "docCONC", 3L, 22L, "je", "je", "PRON", "PRON", "nsubj", 23L, "PronType=Prs|Person=1|Number=Sing",
    "docCONC", 3L, 23L, "reste", "rester", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docCONC", 3L, 24L, ".", ".", "PUNCT", "PUNCT", "punct", 23L, NA_character_,
    "docCOND", 1L, 1L, "Si", "si", "SCONJ", "SCONJ", "mark", 3L, NA_character_,
    "docCOND", 1L, 2L, "tu", "tu", "PRON", "PRON", "nsubj", 3L, "PronType=Prs|Person=2|Number=Sing",
    "docCOND", 1L, 3L, "viens", "venir", "VERB", "VERB", "advcl", 6L, "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docCOND", 1L, 4L, ",", ",", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docCOND", 1L, 5L, "on", "on", "PRON", "PRON", "nsubj", 6L, "PronType=Ind|Number=Sing",
    "docCOND", 1L, 6L, "partira", "partir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Fut|VerbForm=Fin",
    "docCOND", 1L, 7L, ".", ".", "PUNCT", "PUNCT", "punct", 6L, NA_character_,
    "docCOND", 2L, 8L, "À", "à", "ADP", "ADP", "case", 9L, NA_character_,
    "docCOND", 2L, 9L, "moins", "moins", "ADV", "ADV", "mark", 12L, NA_character_,
    "docCOND", 2L, 10L, "que", "que", "SCONJ", "SCONJ", "mark", 12L, NA_character_,
    "docCOND", 2L, 11L, "tu", "tu", "PRON", "PRON", "nsubj", 12L, "PronType=Prs|Person=2|Number=Sing",
    "docCOND", 2L, 12L, "refuses", "refuser", "VERB", "VERB", "advcl", 15L, "Mood=Sub|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docCOND", 2L, 13L, ",", ",", "PUNCT", "PUNCT", "punct", 12L, NA_character_,
    "docCOND", 2L, 14L, "nous", "nous", "PRON", "PRON", "nsubj", 15L, "PronType=Prs|Person=1|Number=Plur",
    "docCOND", 2L, 15L, "irons", "aller", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=1|Tense=Fut|VerbForm=Fin",
    "docCOND", 2L, 16L, ".", ".", "PUNCT", "PUNCT", "punct", 15L, NA_character_,
    "docCOND", 3L, 17L, "À", "à", "ADP", "ADP", "case", 18L, NA_character_,
    "docCOND", 3L, 18L, "condition", "condition", "NOUN", "NOUN", "mark", 21L, NA_character_,
    "docCOND", 3L, 19L, "que", "que", "SCONJ", "SCONJ", "mark", 21L, NA_character_,
    "docCOND", 3L, 20L, "tu", "tu", "PRON", "PRON", "nsubj", 21L, "PronType=Prs|Person=2|Number=Sing",
    "docCOND", 3L, 21L, "répondes", "répondre", "VERB", "VERB", "advcl", 24L, "Mood=Sub|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docCOND", 3L, 22L, ",", ",", "PUNCT", "PUNCT", "punct", 21L, NA_character_,
    "docCOND", 3L, 23L, "je", "je", "PRON", "PRON", "nsubj", 24L, "PronType=Prs|Person=1|Number=Sing",
    "docCOND", 3L, 24L, "signe", "signer", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docCOND", 3L, 25L, ".", ".", "PUNCT", "PUNCT", "punct", 24L, NA_character_,
    "docMISC", 1L, 1L, "Il", "il", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=3|Number=Sing",
    "docMISC", 1L, 2L, "sourit", "sourire", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docMISC", 1L, 3L, "lorsque", "lorsque", "SCONJ", "SCONJ", "mark", 5L, NA_character_,
    "docMISC", 1L, 4L, "tu", "tu", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=2|Number=Sing",
    "docMISC", 1L, 5L, "arrives", "arriver", "VERB", "VERB", "advcl", 2L, "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docMISC", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  sub_tokens$doc_id <- stringr::str_to_lower(sub_tokens$doc_id)
  sub_tokens$token <- stringr::str_to_lower(sub_tokens$token)
  sub_tokens$lemma <- stringr::str_to_lower(sub_tokens$lemma)
  sub_tokens$tag <- stringr::str_to_upper(sub_tokens$tag)

  class(sub_tokens) <- c("spacyr_parsed", class(sub_tokens))

  features <- biber(sub_tokens, measure = "none", normalize = FALSE)

  docsr <- features[features$doc_id == "docsr", , drop = FALSE]
  docbc <- features[features$doc_id == "docbc", , drop = FALSE]
  docconc <- features[features$doc_id == "docconc", , drop = FALSE]
  doccond <- features[features$doc_id == "doccond", , drop = FALSE]
  docmisc <- features[features$doc_id == "docmisc", , drop = FALSE]

  expect_equal(docsr$f_34_sentence_relatives, 1)
  expect_equal(docsr$f_35_because, 0)

  expect_equal(docbc$f_35_because, 2)
  expect_equal(docbc$f_36_though, 0)

  expect_equal(docconc$f_36_though, 3)
  expect_equal(docconc$f_37_if, 1)

  expect_equal(doccond$f_37_if, 3)
  expect_equal(doccond$f_35_because, 0)

  expect_equal(docmisc$f_38_other_adv_sub, 1)
  expect_equal(docmisc$f_35_because, 0)
})

test_that("French complementizer omission and coordination features behave", {
  deletion_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docDEL", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docDEL", 1L, 2L, "pense", "penser", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docDEL", 1L, 3L, "il", "il", "PRON", "PRON", "nsubj", 4L, "PronType=Prs|Person=3|Number=Sing",
    "docDEL", 1L, 4L, "vient", "venir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docDEL", 1L, 5L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docQUE", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docQUE", 1L, 2L, "pense", "penser", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docQUE", 1L, 3L, "qu'", "que", "SCONJ", "SCONJ", "mark", 5L, NA_character_,
    "docQUE", 1L, 4L, "il", "il", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=3|Number=Sing",
    "docQUE", 1L, 5L, "vient", "venir", "VERB", "VERB", "ccomp", 2L, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docQUE", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  coord_tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docPHRAS", 1L, 1L, "Les", "le", "DET", "DET", "det", 2L, "Definite=Def|Number=Plur|PronType=Art",
    "docPHRAS", 1L, 2L, "chats", "chat", "NOUN", "NOUN", "nsubj", 5L, "Gender=Masc|Number=Plur",
    "docPHRAS", 1L, 3L, "et", "et", "CCONJ", "CCONJ", "cc", 4L, NA_character_,
    "docPHRAS", 1L, 4L, "chiens", "chien", "NOUN", "NOUN", "conj", 2L, "Gender=Masc|Number=Plur",
    "docPHRAS", 1L, 5L, "jouent", "jouer", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Plur|Person=3|Tense=Pres|VerbForm=Fin",
    "docPHRAS", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 5L, NA_character_,
    "docCLAUS", 1L, 1L, "Je", "je", "PRON", "PRON", "nsubj", 2L, "PronType=Prs|Person=1|Number=Sing",
    "docCLAUS", 1L, 2L, "chante", "chanter", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=1|Tense=Pres|VerbForm=Fin",
    "docCLAUS", 1L, 3L, "et", "et", "CCONJ", "CCONJ", "cc", 5L, NA_character_,
    "docCLAUS", 1L, 4L, "tu", "tu", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=2|Number=Sing",
    "docCLAUS", 1L, 5L, "danses", "danser", "VERB", "VERB", "conj", 2L, "Mood=Ind|Number=Sing|Person=2|Tense=Pres|VerbForm=Fin",
    "docCLAUS", 1L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_
  )

  all_tokens <- dplyr::bind_rows(deletion_tokens, coord_tokens)

  all_tokens$doc_id <- stringr::str_to_lower(all_tokens$doc_id)
  all_tokens$token <- stringr::str_to_lower(all_tokens$token)
  all_tokens$lemma <- stringr::str_to_lower(all_tokens$lemma)
  all_tokens$tag <- stringr::str_to_upper(all_tokens$tag)

  class(all_tokens) <- c("spacyr_parsed", class(all_tokens))

  features <- biber(all_tokens, measure = "none", normalize = FALSE)

  docdel <- features[features$doc_id == "docdel", , drop = FALSE]
  docque <- features[features$doc_id == "docque", , drop = FALSE]
  docphras <- features[features$doc_id == "docphras", , drop = FALSE]
  docclaus <- features[features$doc_id == "docclaus", , drop = FALSE]

  expect_equal(docdel$f_60_that_deletion, 1)
  expect_equal(docque$f_60_that_deletion, 0)

  expect_equal(docdel$f_61_stranded_preposition, 0)
  expect_equal(docque$f_62_split_infinitive, 0)

  expect_equal(docphras$f_64_phrasal_coordination, 1)
  expect_equal(docclaus$f_65_clausal_coordination, 1)
})

test_that("French impersonal, existential, and nominal features counted", {
  tokens <- tibble::tribble(
    ~doc_id, ~sentence_id, ~token_id, ~token, ~lemma, ~pos, ~tag, ~dep_rel, ~head_token_id, ~feats,
    "docIMP", 1L, 1L, "Il", "il", "PRON", "PRON", "expl", 2L, "PronType=Prs|Person=3|Number=Sing",
    "docIMP", 1L, 2L, "pleut", "pleuvoir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docIMP", 1L, 3L, ".", ".", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docIMP", 2L, 1L, "Il", "il", "PRON", "PRON", "expl", 3L, "PronType=Prs|Person=3|Number=Sing",
    "docIMP", 2L, 2L, "y", "y", "PRON", "PRON", "obj", 3L, "PronType=Prs",
    "docIMP", 2L, 3L, "a", "avoir", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docIMP", 2L, 4L, "trois", "trois", "NUM", "NUM", "nummod", 5L, "NumType=Card",
    "docIMP", 2L, 5L, "ans", "an", "NOUN", "NOUN", "obl", 3L, "Gender=Masc|Number=Plur",
    "docIMP", 2L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docNOM", 1L, 1L, "La", "le", "DET", "DET", "det", 2L, "Definite=Def|Gender=Fem|Number=Sing|PronType=Art",
    "docNOM", 1L, 2L, "création", "création", "NOUN", "NOUN", "nsubj", 3L, "Gender=Fem|Number=Sing",
    "docNOM", 1L, 3L, "progresse", "progresser", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docNOM", 1L, 4L, ".", ".", "PUNCT", "PUNCT", "punct", 3L, NA_character_,
    "docNOM", 2L, 1L, "En", "en", "ADP", "ADP", "mark", 2L, NA_character_,
    "docNOM", 2L, 2L, "travaillant", "travailler", "VERB", "VERB", "advcl", 5L, "VerbForm=Ger|Tense=Pres",
    "docNOM", 2L, 3L, ",", ",", "PUNCT", "PUNCT", "punct", 2L, NA_character_,
    "docNOM", 2L, 4L, "il", "il", "PRON", "PRON", "nsubj", 5L, "PronType=Prs|Person=3|Number=Sing",
    "docNOM", 2L, 5L, "progresse", "progresser", "VERB", "VERB", "root", NA_integer_, "Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin",
    "docNOM", 2L, 6L, ".", ".", "PUNCT", "PUNCT", "punct", 5L, NA_character_
  )

  tokens$doc_id <- stringr::str_to_lower(tokens$doc_id)
  tokens$token <- stringr::str_to_lower(tokens$token)
  tokens$lemma <- stringr::str_to_lower(tokens$lemma)
  tokens$tag <- stringr::str_to_upper(tokens$tag)

  class(tokens) <- c("spacyr_parsed", class(tokens))

  features <- biber(tokens, measure = "none", normalize = FALSE)

  docimp <- features[features$doc_id == "docimp", , drop = FALSE]
  docnom <- features[features$doc_id == "docnom", , drop = FALSE]

  expect_equal(docimp$f_09_pronoun_it, 2)
  expect_equal(docimp$f_12_proverb_do, 0)
  expect_equal(docimp$f_20_existential_there, 1)

  expect_equal(docnom$f_14_nominalizations, 1)
  expect_equal(docnom$f_15_gerunds, 1)
})

test_that("udpipe connector handles missing xpos", {
  skip_if_not_installed("udpipe")

  model_path <- test_path("..", "..", "..", "french-gsd-ud-2.5-191206.udpipe")
  skip_if_not(file.exists(model_path), "French UD model not available")

  model <- udpipe::udpipe_load_model(model_path)
  on.exit(rm(model), add = TRUE)

  annotation <- udpipe::udpipe_annotate(
    model,
    x = "Nous avons fini le travail.",
    doc_id = "ud_doc"
  )

  features <- biber(annotation, measure = "none", normalize = FALSE)

  doc <- features[features$doc_id == "ud_doc", , drop = FALSE]

  expect_equal(doc$f_01_past_tense, 0)
  expect_equal(doc$f_02_perfect_aspect, 1)
  expect_equal(doc$f_03_present_tense, 0)
  expect_equal(doc$f_06_first_person_pronouns, 1)
})

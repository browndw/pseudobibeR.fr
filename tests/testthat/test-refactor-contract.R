# Contract regressions for the evidence-based French pipeline.

clause <- function(doc_id, token, lemma, upos, heads, deps,
                   feats = rep(NA_character_, length(token)), sentence_id = 1L) {
  tokens <- data.frame(
    doc_id = doc_id, sentence_id = sentence_id, token_id = seq_along(token),
    token = token, lemma = lemma, pos = upos, tag = upos,
    morph = feats, head_token_id = heads, dep_rel = deps,
    stringsAsFactors = FALSE
  )
  class(tokens) <- c("spacyr_parsed", "data.frame")
  tokens
}

combine <- function(...) {
  tokens <- do.call(rbind, lapply(list(...), as.data.frame))
  class(tokens) <- c("spacyr_parsed", "data.frame")
  tokens
}

raw_counts <- function(tokens) biber(tokens, measure = "none", normalize = FALSE)

test_that("raw counts equal audit rows for every countable feature", {
  for (sample in list(udpipe_samples, spacy_samples)) {
    raw <- raw_counts(sample)
    features <- setdiff(grep("^f_", names(raw), value = TRUE), "f_44_mean_word_length")
    audit <- audit_features(sample, feature = features)
    for (feature in features) {
      rows <- audit[audit$feature == feature, ]
      observed <- tabulate(match(rows$doc_id, raw$doc_id), nbins = nrow(raw))
      expect_equal(observed, as.integer(raw[[feature]]), info = feature)
    }
  }
})

test_that("occurrence identity respects sentence boundaries", {
  perfect <- function(sentence_id) {
    clause("doc", c("j'", "ai", "fini"), c("je", "avoir", "finir"),
           c("PRON", "AUX", "VERB"), c(3, 3, 0), c("nsubj", "aux:tense", "root"),
           c("Person=1", "VerbForm=Fin|Tense=Pres", "VerbForm=Part|Tense=Past"),
           sentence_id = sentence_id)
  }
  tokens <- combine(perfect(1L), perfect(2L))
  raw <- raw_counts(tokens)
  expect_equal(raw$f_02_perfect_aspect, 2)
  expect_equal(nrow(audit_features(tokens, feature = "f_02_perfect_aspect")), 2L)
})

test_that("passive auxiliaries are passives, not perfect aspect", {
  tokens <- clause(
    "passive", c("le", "texte", "est", "lu", "par", "Marie"),
    c("le", "texte", "\u00eatre", "lire", "par", "Marie"),
    c("DET", "NOUN", "AUX", "VERB", "ADP", "PROPN"), c(2, 4, 4, 0, 6, 4),
    c("det", "nsubj:pass", "aux:pass", "root", "case", "obl:agent"),
    c(NA, NA, "VerbForm=Fin|Tense=Pres", "VerbForm=Part|Tense=Past", NA, NA)
  )
  raw <- raw_counts(tokens)
  expect_equal(raw$f_02_perfect_aspect, 0)
  expect_equal(raw$f_18_by_passives, 1)
  expect_equal(raw$f_17_agentless_passives, 0)
  expect_equal(audit_features(tokens, feature = "f_18_by_passives")$keyword, "est lu")
})

test_that("package resources are used regardless of masking objects", {
  tokens <- udpipe_samples
  expected <- raw_counts(tokens)
  local({
    dict <- quanteda::dictionary(list(f_04_place_adverbials = "zzz"))
    word_lists <- list(pronoun_matchlist = "zzz")
    expect_equal(pseudobibeR.fr::biber(tokens, measure = "none", normalize = FALSE), expected)
  })
  assign("dict", list(), envir = globalenv())
  on.exit(rm("dict", envir = globalenv()), add = TRUE)
  expect_equal(pseudobibeR.fr::biber(tokens, measure = "none", normalize = FALSE), expected)
})

test_that("packaged inventories keep YAML boolean words and canonical keys", {
  values <- c(unlist(word_lists, use.names = FALSE), unlist(as.list(dict), use.names = FALSE))
  expect_false(any(values %in% c("true", "false", "TRUE", "FALSE")))
  expect_true("on" %in% word_lists$pronoun_matchlist)
  expect_true("f_11_indefinite_pronouns" %in% names(dict))
  expect_true(all(c("ça", "ceci", "celui") %in% word_lists$demonstrative_matchlist))
  expect_false(any(c("je", "moi", "eux") %in% word_lists$demonstrative_matchlist))
})

test_that("normalization uses word tokens, excluding punctuation and numerals", {
  tokens <- clause("doc", c("dans", "la", "maison", "42", "."),
                   c("dans", "le", "maison", "42", "."),
                   c("ADP", "DET", "NOUN", "NUM", "PUNCT"), c(3, 3, 0, 3, 3),
                   c("case", "det", "root", "nummod", "punct"))
  rate <- biber(tokens, measure = "none", normalize = TRUE)
  expect_equal(rate$f_39_prepositions, 1000 / 3)
  expect_named(rate, c("doc_id", setdiff(names(pseudobibeR.fr:::.feature_categories),
                                          "f_43_type_token")))
})

test_that("exported functions accept parser annotations, not plain data frames", {
  plain <- as.data.frame(clause("doc", c("il", "dort"), c("il", "dormir"),
                                c("PRON", "VERB"), c(2, 0), c("nsubj", "root")))
  expect_error(biber(plain), "no applicable method")
  expect_error(audit_features(plain, feature = "f_03_present_tense"), "no applicable method")
})

test_that("ce que/ce qui complementing a verb count once in f_23", {
  complement <- clause(
    "complement", c("je", "sais", "ce", "qui", "arrive"), c("je", "savoir", "ce", "qui", "arriver"),
    c("PRON", "VERB", "PRON", "PRON", "VERB"), c(2, 0, 2, 5, 3),
    c("nsubj", "root", "obj", "nsubj", "acl:relcl"),
    c(NA, NA, "PronType=Dem", "PronType=Rel", NA)
  )
  sentential <- clause(
    "sentential", c("il", "part", ",", "ce", "qui", "surprend"),
    c("il", "partir", ",", "ce", "qui", "surprendre"),
    c("PRON", "VERB", "PUNCT", "PRON", "PRON", "VERB"), c(2, 0, 4, 2, 6, 4),
    c("nsubj", "root", "punct", "parataxis", "nsubj", "acl:relcl"),
    c(NA, NA, NA, "PronType=Dem", "PronType=Rel", NA)
  )
  subject <- clause(
    "subject", c("ce", "que", "tu", "dis", "est", "vrai"),
    c("ce", "que", "tu", "dire", "\u00eatre", "vrai"),
    c("PRON", "PRON", "PRON", "VERB", "AUX", "ADJ"), c(6, 4, 4, 1, 6, 0),
    c("nsubj", "obj", "nsubj", "acl:relcl", "cop", "root"),
    c("PronType=Dem", "PronType=Rel", NA, NA, NA, NA)
  )
  tokens <- combine(complement, sentential, subject)
  raw <- raw_counts(tokens)
  expect_equal(raw$f_23_wh_clause, c(1, 0, 0))
  expect_equal(raw$f_34_sentence_relatives, c(0, 1, 0))
  expect_equal(audit_features(tokens, feature = "f_23_wh_clause")$keyword, "ce qui")
})

test_that("explicit lexical-diversity measures are honored; default MATTR falls back", {
  tokens <- clause("doc", c("le", "chat", "dort"), c("le", "chat", "dormir"),
                   c("DET", "NOUN", "VERB"), c(2, 3, 0), c("det", "nsubj", "root"))
  expect_message(mattr <- biber(tokens, normalize = FALSE), "TTR")
  cttr <- biber(tokens, measure = "CTTR", normalize = FALSE)
  expect_equal(mattr$f_43_type_token, 1)
  expect_equal(cttr$f_43_type_token, 3 / sqrt(2 * 3))
})

test_that("mean word length includes accented words", {
  tokens <- clause("doc", c("un", "caf\u00e9"), c("un", "caf\u00e9"), c("DET", "NOUN"),
                   c(2, 0), c("det", "root"))
  expect_equal(raw_counts(tokens)$f_44_mean_word_length, 3)
})

test_that("documents keep their order of first appearance", {
  tokens <- combine(
    clause("b", c("il", "dort"), c("il", "dormir"), c("PRON", "VERB"), c(2, 0), c("nsubj", "root")),
    clause("a", c("il", "dort"), c("il", "dormir"), c("PRON", "VERB"), c(2, 0), c("nsubj", "root"))
  )
  expect_equal(raw_counts(tokens)$doc_id, c("b", "a"))
})

test_that("a dictionary phrase takes precedence over words it contains", {
  tokens <- clause("doc", c("c'", "est", "en", "quelque", "sorte", "difficile"),
                   c("ce", "\u00eatre", "en", "quelque", "sorte", "difficile"),
                   c("PRON", "AUX", "ADP", "DET", "NOUN", "ADJ"), c(6, 6, 5, 5, 6, 0),
                   c("nsubj", "cop", "case", "det", "obl", "root"))
  raw <- raw_counts(tokens)
  expect_equal(raw$f_47_hedges, 1)
  expect_equal(raw$f_11_indefinite_pronouns, 0)
})

test_that("UDPipe probes reflect the corrected definitions", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  texts <- c(
    ami = "C'est un ami.",
    passive = "Le g\u00e2teau est mang\u00e9 par le chat.",
    relative = "C'est la personne \u00e0 qui je parle.",
    question = "Le livre que tu lis est-il bon ?",
    ilya = "Il y a un chat et il y a un chien.",
    adjective = "Un caf\u00e9 utile.",
    weather = "Il fait froid ce matin.",
    referential = "Il fait ses devoirs.",
    raising = "Il semble que tu as raison.",
    predicative = "Il semble fatigu\u00e9.",
    modal_weather = "Il pourrait pleuvoir.",
    personal = "Elle pense qu'il viendra.",
    complement = "Elle pense que c'est une bonne id\u00e9e.",
    adj_complement = "Ils sont heureux que vous veniez.",
    reduced = "Les lettres \u00e9crites par ma grand-m\u00e8re sont pr\u00e9cieuses.",
    finite_relative = "C'est une r\u00e9ponse qu'il a donn\u00e9e hier.",
    free_relative = "Je ne sais pas ce qui s'est pass\u00e9.",
    free_object = "J'aime ce que tu fais.",
    pseudo_relative = "Je les ai vus qui arrivaient.",
    indirect_where = "Je sais o\u00f9 il habite."
  )
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = texts, doc_id = names(texts)))
  value <- function(doc, feature) raw[[feature]][raw$doc_id == doc]
  expect_equal(value("ami", "f_02_perfect_aspect"), 0)
  expect_equal(value("passive", "f_02_perfect_aspect"), 0)
  expect_equal(value("passive", "f_18_by_passives"), 1)
  expect_equal(value("relative", "f_23_wh_clause"), 0)
  expect_equal(value("relative", "f_32_wh_obj"), 0)
  expect_equal(value("question", "f_13_wh_question"), 0)
  expect_equal(value("question", "f_23_wh_clause"), 0)
  expect_equal(value("question", "f_30_that_obj"), 1)
  expect_equal(value("ilya", "f_20_existential_there"), 2)
  expect_equal(value("adjective", "f_40_adj_attr"), 1)
  expect_equal(value("weather", "f_09_pronoun_it"), 1)
  expect_equal(value("weather", "f_08_third_person_pronouns"), 0)
  expect_equal(value("referential", "f_09_pronoun_it"), 0)
  # il and the possessive ses, as English counts he and his.
  expect_equal(value("referential", "f_08_third_person_pronouns"), 2)
  expect_equal(value("raising", "f_09_pronoun_it"), 1)
  expect_equal(value("predicative", "f_09_pronoun_it"), 0)
  expect_equal(value("modal_weather", "f_09_pronoun_it"), 1)
  expect_equal(value("personal", "f_09_pronoun_it"), 0)
  expect_equal(value("personal", "f_08_third_person_pronouns"), 2)
  expect_equal(value("complement", "f_21_that_verb_comp"), 1)
  expect_equal(value("adj_complement", "f_22_that_adj_comp"), 1)
  expect_equal(value("adj_complement", "f_21_that_verb_comp"), 0)
  expect_equal(value("reduced", "f_27_past_participle_whiz"), 1)
  expect_equal(value("finite_relative", "f_27_past_participle_whiz"), 0)
  expect_equal(value("free_relative", "f_23_wh_clause"), 1)
  expect_equal(value("free_object", "f_23_wh_clause"), 1)
  expect_equal(value("pseudo_relative", "f_23_wh_clause"), 0)
  expect_equal(value("indirect_where", "f_23_wh_clause"), 1)
})

test_that("UDPipe probes reflect the ported accepted decisions (group B)", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  texts <- c(
    future = "Il viendra demain.",
    conditional = "Je voudrais un caf\u00e9.",
    aller = "Il va bient\u00f4t partir.",
    perfect = "Il a mang\u00e9.",
    subjunctive = "Je veux qu'il vienne.",
    qui = "Le chien noir de mon voisin qui aboie est malade.",
    que = "Les lettres que j'ai \u00e9crites sont perdues.",
    lequel = "Les candidats, lesquels r\u00e9ussissent, sont contents.",
    pied = "La maison dans laquelle il vit est belle.",
    dont = "Le livre dont je parle est utile.",
    adverbs = "Il travaille ici tr\u00e8s souvent et vraiment bien.",
    reflexive = "Il se le fait."
  )
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = texts, doc_id = names(texts)))
  value <- function(doc, feature) raw[[feature]][raw$doc_id == doc]
  expect_equal(value("future", "f_54_modal_predictive"), 1)
  expect_equal(value("future", "f_03_present_tense"), 0)
  expect_equal(value("conditional", "f_54_modal_predictive"), 1)
  expect_equal(value("conditional", "f_03_present_tense"), 0)
  expect_equal(value("aller", "f_54_modal_predictive"), 1)
  expect_equal(value("perfect", "f_03_present_tense"), 1)
  expect_equal(value("subjunctive", "f_03_present_tense"), 2)
  relative_bins <- c("f_29_that_subj", "f_30_that_obj", "f_31_wh_subj", "f_32_wh_obj",
                     "f_33_pied_piping", "f_34_sentence_relatives")
  expected_bin <- c(qui = "f_29_that_subj", que = "f_30_that_obj", lequel = "f_31_wh_subj",
                    pied = "f_33_pied_piping", dont = "f_33_pied_piping")
  for (doc in names(expected_bin)) {
    counts <- vapply(relative_bins, function(f) value(doc, f), numeric(1))
    expect_equal(unname(counts), as.numeric(relative_bins == expected_bin[[doc]]), info = doc)
  }
  expect_equal(value("adverbs", "f_42_adverbs"), 1)
  expect_equal(value("reflexive", "f_12_proverb_do"), 0)
})

test_that("unaccented input matches lexical inventories, but not contrastive forms", {
  tokens <- clause("doc", c("il", "est", "tres", "content", "la"),
                   c("il", "\u00eatre", "tres", "content", "la"),
                   c("PRON", "AUX", "ADV", "ADJ", "ADV"), c(4, 4, 4, 0, 4),
                   c("nsubj", "cop", "advmod", "root", "advmod"))
  raw <- raw_counts(tokens)
  expect_equal(raw$f_48_amplifiers, 1)
  expect_equal(raw$f_04_place_adverbials, 0)
  expect_true(all(c("tres", "tr\u00e8s") %in% dict[["f_48_amplifiers"]]))
  expect_false("tout_a" %in% dict[["f_49_emphatics"]])
  expect_false("ite" %in% word_lists$nominalization_suffixes)
})

test_that("French negation follows the synthetic/analytic partition (D-18)", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  cases <- tibble::tribble(
    ~text, ~f66, ~f67, ~f42,
    "Personne ne sait.", 1, 0, 0,
    "Je ne vois personne.", 1, 0, 0,
    "Une personne est venue.", 0, 0, 0,
    "Un rien l'amuse.", 0, 0, 0,
    "Ce n'est pas rien.", 1, 1, 1,
    "Si jamais tu viens, appelle-moi.", 0, 0, 0,
    "Je n'en veux plus.", 0, 1, 1,
    "Il est plus grand.", 0, 0, 1,
    "C'est pas grave.", 0, 1, 1,
    "Il ne fait que travailler.", 0, 0, 1,
    "Je crains qu'il ne vienne.", 0, 0, 0,
    "Un r\u00e9sultat non n\u00e9gligeable.", 0, 1, 1,
    "Ni Jean ni Marie ne viennent.", 2, 0, 0,
    "Il n'a gu\u00e8re dormi.", 0, 0, 0,
    "Il n'a pas mang\u00e9.", 0, 1, 1,
    "Personne n'a jamais rien dit.", 3, 0, 0
  )
  docs <- sprintf("neg_%02d", seq_len(nrow(cases)))
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = cases$text, doc_id = docs))
  expect_equal(raw$f_66_neg_synthetic, cases$f66)
  expect_equal(raw$f_67_neg_analytic, cases$f67)
  expect_equal(raw$f_42_adverbs, cases$f42)
})

test_that("adverbial subordinators keep only unambiguous markers in f_35-f_37 (D-44)", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  cases <- tibble::tribble(
    ~text, ~f35, ~f36, ~f37, ~f38, ~f21,
    "Je reste parce qu'il pleut.", 1, 0, 0, 0, 0,
    "Je reste, car il pleut.", 0, 0, 0, 0, 0,
    "Puisque tu insistes, je viens.", 0, 0, 0, 1, 0,
    "Comme il pleuvait, je suis rest\u00e9.", 0, 0, 0, 1, 0,
    "Il chante comme un oiseau.", 0, 0, 0, 0, 0,
    "Bien qu'il soit tard, je travaille.", 0, 1, 0, 0, 0,
    "Quoiqu'il dise, je pars.", 0, 1, 0, 0, 0,
    "Encore qu'il faille le v\u00e9rifier, c'est probable.", 0, 0, 0, 1, 0,
    "M\u00eame s'il pleut, je sors.", 0, 0, 1, 0, 0,
    "\u00c0 moins qu'il pleuve, je sors.", 0, 0, 1, 0, 0,
    "Je sors sauf s'il pleut.", 0, 0, 1, 0, 0,
    "Je viendrai \u00e0 condition que tu sois l\u00e0.", 0, 0, 0, 1, 0,
    "Je serais venu si tu m'avais appel\u00e9.", 0, 0, 1, 0, 0,
    "Je me demande s'il viendra.", 0, 0, 0, 0, 0,
    "Je ne sais pas si c'est vrai.", 0, 0, 0, 0, 0
  )
  docs <- sprintf("adv_%02d", seq_len(nrow(cases)))
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = cases$text, doc_id = docs))
  expect_equal(raw$f_35_because, cases$f35)
  expect_equal(raw$f_36_though, cases$f36)
  expect_equal(raw$f_37_if, cases$f37)
  expect_equal(raw$f_38_other_adv_sub, cases$f38)
  expect_equal(raw$f_21_that_verb_comp, cases$f21)
  # Indirect-question si is not a deleted que.
  expect_equal(raw$f_60_that_deletion[14:15], c(0, 0))
})

test_that("f_59 counts optional colloquial reductions, not obligatory elision (D-45)", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  cases <- tibble::tribble(
    ~text, ~f59,
    "J'ai faim.", 0,
    "C'\u00e9tait bien.", 0,
    "L'homme qu'il aime.", 0,
    "Aujourd'hui il pleut.", 0,
    "Il est p'tit lui.", 1,
    "Bonjour m'sieur.", 1,
    "Chuis fatigu\u00e9.", 1,
    "T'as vu \u00e7a ?", 1,
    "Tu sais, t'avais raison.", 1,
    "Tu t'es tromp\u00e9.", 0,
    "Je t'avais dit.", 0,
    "\u00c7a t'a plu ?", 0,
    "J'pense que oui.", 1,
    "J'habite ici.", 0,
    "Y a du monde.", 1,
    "Y'a personne.", 1,
    "Il n'y a rien.", 0,
    "Y a-t-il quelqu'un ?", 0
  )
  docs <- sprintf("ctr_%02d", seq_len(nrow(cases)))
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = cases$text, doc_id = docs))
  expect_equal(raw$f_59_contractions, cases$f59)
})

test_that("f_60 counts reduced complements under bounded governors (D-27, D-46)", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  cases <- tibble::tribble(
    ~text, ~f60,
    "Je crois avoir compris.", 1,
    "J'esp\u00e8re venir ce soir.", 1,
    "Je crois l'avoir vu.", 1,
    "Il dit avoir vu le voleur.", 1,
    "Il pr\u00e9tend avoir tout lu.", 1,
    "Je crois il est parti.", 1,
    "Je crois avoir compris et l'avoir not\u00e9.", 2,
    "Je crois qu'il est parti.", 0,
    "Il m'a dit de partir.", 0,
    "Je pense \u00e0 partir.", 0,
    "Je veux partir.", 0,
    "Il faut partir.", 0,
    "Je sais nager.", 0,
    "Je ne sais pas quoi dire.", 0
  )
  docs <- sprintf("red_%02d", seq_len(nrow(cases)))
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = cases$text, doc_id = docs))
  expect_equal(raw$f_60_that_deletion, cases$f60)
})

test_that("f_61 counts colloquial competitors of pied-piping, not pied-piping (D-47)", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  cases <- tibble::tribble(
    ~text, ~f61, ~f33, ~f30,
    "La fille avec qui je sors est gentille.", 0, 1, 0,
    "C'est le film dont je t'ai parl\u00e9.", 0, 1, 0,
    "C'est la fille que je sors avec.", 1, 0, 0,
    "Le gars que je travaille pour est parti.", 1, 0, 0,
    "C'est la personne que je t'ai parl\u00e9 d'elle.", 1, 0, 0,
    "C'est un ami que je compte sur lui.", 1, 0, 0,
    "C'est le film que je t'en ai parl\u00e9.", 1, 0, 0,
    "Le livre que j'ai lu avec plaisir est l\u00e0.", 0, 0, 1,
    "La femme que j'ai vue parlait d'elle.", 0, 0, 1,
    "Les g\u00e2teaux que j'en ai mang\u00e9 trois.", 0, 0, 1,
    "Je suis pour.", 0, 0, 0
  )
  docs <- sprintf("str_%02d", seq_len(nrow(cases)))
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = cases$text, doc_id = docs))
  expect_equal(raw$f_61_stranded_preposition, cases$f61)
  expect_equal(raw$f_33_pied_piping, cases$f33)
  expect_equal(raw$f_30_that_obj, cases$f30)
})

test_that("f_62 counts interruption by adverbs that normally follow the infinitive (D-48)", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  cases <- tibble::tribble(
    ~text, ~f62,
    "Il essaie de vraiment comprendre.", 1,
    "Il essaie de comprendre vraiment.", 0,
    "Les prix continuent de fortement augmenter.", 1,
    "Sans vraiment savoir pourquoi, il est parti.", 1,
    "Il a fini par toujours gagner.", 1,
    "Il essaie de vraiment le comprendre.", 1,
    "Il a promis de ne pas vraiment comprendre.", 1,
    "Il travaille pour mieux comprendre.", 0,
    "Il faut du temps pour bien le faire.", 0,
    "Sans trop savoir pourquoi, il est parti.", 0,
    "Il a promis de ne pas venir.", 0,
    "Il a d\u00e9cid\u00e9 de ne plus jamais revenir.", 0
  )
  docs <- sprintf("spl_%02d", seq_len(nrow(cases)))
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = cases$text, doc_id = docs))
  expect_equal(raw$f_62_split_infinitive, cases$f62)
})

test_that("f_63 excludes the analytic negator alone but counts negative adverbs (D-49)", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  cases <- tibble::tribble(
    ~text, ~f63,
    "Il n'a pas mang\u00e9.", 0,
    "Il a pas mang\u00e9.", 0,
    "Il n'a jamais vu la mer.", 1,
    "Il a souvent mang\u00e9 ici.", 1,
    "Il n'a pas encore fini.", 1,
    "Il peut facilement r\u00e9soudre le probl\u00e8me.", 1,
    "Il peut ne pas venir.", 0,
    "Ils sont objectivement montr\u00e9s.", 1,
    "Est-elle d\u00e9j\u00e0 arriv\u00e9e ?", 0
  )
  docs <- sprintf("aux_%02d", seq_len(nrow(cases)))
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = cases$text, doc_id = docs))
  expect_equal(raw$f_63_split_auxiliary, cases$f63)
})

test_that("ce/ça are demonstratives, not personal pronouns (D-50)", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  cases <- tibble::tribble(
    ~text, ~f08, ~f09, ~f10,
    "C'est vrai.", 0, 0, 1,
    "Ce n'est pas grave.", 0, 0, 1,
    "\u00c7a marche.", 0, 0, 1,
    "C'est important de lire.", 0, 1, 0,
    "Ce que tu dis est vrai.", 0, 0, 0,
    "Il est parti t\u00f4t, ce qui est dommage.", 1, 0, 0,
    "Celui-ci est rouge.", 0, 0, 1,
    "C'est Marie qui a gagn\u00e9.", 0, 0, 1,
    "On mange.", 0, 0, 0,
    "Il en veut.", 1, 0, 0,
    "Elle se lave.", 2, 0, 0
  )
  docs <- sprintf("dem_%02d", seq_len(nrow(cases)))
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = cases$text, doc_id = docs))
  expect_equal(raw$f_08_third_person_pronouns, cases$f08)
  expect_equal(raw$f_09_pronoun_it, cases$f09)
  expect_equal(raw$f_10_demonstrative_pronoun, cases$f10)
})

test_that("f_11 counts indefinites in pronoun use only (D-52)", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  cases <- tibble::tribble(
    ~text, ~f11,
    "Une personne est venue.", 0,
    "Personne n'est venu.", 1,
    "Je ne vois personne.", 1,
    "C'est une chose importante.", 0,
    "Quelque chose a chang\u00e9.", 1,
    "Il reste quelque temps.", 0,
    "Tout le monde rit.", 1,
    "Il a tout mang\u00e9.", 1,
    "Il est tout petit.", 0,
    "Chacun part.", 1,
    "Aucun n'est venu.", 1,
    "Je n'ai aucun livre.", 0,
    "Un rien l'amuse.", 0,
    "Quelques-uns sont partis.", 1
  )
  docs <- sprintf("ind_%02d", seq_len(nrow(cases)))
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = cases$text, doc_id = docs))
  expect_equal(raw$f_11_indefinite_pronouns, cases$f11)
})

test_that("existentials, adverbial subordinators, possessives and multiword verbs (D-55)", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  cases <- tibble::tribble(
    ~text, ~f20, ~f38, ~f06, ~f08, ~f55, ~f57, ~f58,
    "Il n'y a rien.", 1, 0, 0, 0, 0, 0, 0,
    "Il y en a trois.", 1, 0, 0, 0, 0, 0, 0,
    "Il y a eu un accident.", 1, 0, 0, 0, 0, 0, 0,
    "Elle y a mang\u00e9 hier.", 0, 0, 0, 1, 0, 0, 0,
    "Il est facile de convaincre Jean.", 0, 0, 0, 0, 0, 1, 0,
    "Avant de partir, il mange.", 0, 0, 0, 1, 0, 0, 0,
    "Pendant qu'il dort, je lis.", 0, 1, 1, 1, 0, 0, 0,
    "Mon fr\u00e8re et moi avons visit\u00e9 le mus\u00e9e.", 0, 0, 2, 0, 0, 0, 0,
    "Le son est fort.", 0, 0, 0, 0, 0, 0, 0,
    "Ce livre est le mien.", 0, 0, 1, 0, 0, 0, 0,
    "Il se plaint du bruit.", 0, 0, 0, 2, 1, 0, 0,
    "Il a donn\u00e9 des instructions.", 0, 0, 0, 1, 0, 1, 0,
    "Elle avait l'air heureuse.", 0, 0, 0, 1, 0, 0, 1
  )
  docs <- sprintf("d55_%02d", seq_len(nrow(cases)))
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = cases$text, doc_id = docs))
  expect_equal(raw$f_20_existential_there, cases$f20)
  expect_equal(raw$f_38_other_adv_sub, cases$f38)
  expect_equal(raw$f_06_first_person_pronouns, cases$f06)
  expect_equal(raw$f_08_third_person_pronouns, cases$f08)
  expect_equal(raw$f_55_verb_public, cases$f55)
  expect_equal(raw$f_57_verb_suasive, cases$f57)
  expect_equal(raw$f_58_verb_seem, cases$f58)
})

test_that("f_50 clause-initial, f_39 compound prepositions, f_65 initial et (D-56)", {
  model_path <- get_udpipe_model_path()
  skip_if(is.null(model_path), "UDPipe French model missing")
  model <- udpipe::udpipe_load_model(model_path)
  cases <- tibble::tribble(
    ~text, ~f50, ~f39, ~f65,
    "Il mange bien.", 0, 0, 0,
    "Il \u00e9tait alors jeune.", 0, 0, 0,
    "Eh bien, je ne sais pas.", 1, 0, 0,
    "Du coup, on y va.", 1, 1, 0,
    "Il reste \u00e0 cause de la pluie.", 0, 1, 0,
    "Gr\u00e2ce \u00e0 toi, il a r\u00e9ussi.", 0, 1, 0,
    "Et il est parti.", 0, 0, 1,
    "Et alors ?", 0, 0, 0,
    "Il est parti et elle est rest\u00e9e.", 0, 0, 1,
    "Il est grand et elle est petite.", 0, 0, 1
  )
  docs <- sprintf("d56_%02d", seq_len(nrow(cases)))
  raw <- raw_counts(udpipe::udpipe_annotate(model, x = cases$text, doc_id = docs))
  expect_equal(raw$f_50_discourse_particles, cases$f50)
  expect_equal(raw$f_39_prepositions, cases$f39)
  expect_equal(raw$f_65_clausal_coordination, cases$f65)
})

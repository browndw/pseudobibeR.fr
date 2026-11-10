# pseudobibeR.fr: Biber's Linguistic Features for French

[![R-CMD-check](https://github.com/browndw/pseudobibeR.fr/workflows/R-CMD-check/badge.svg)](https://github.com/browndw/pseudobibeR.fr/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

The **pseudobibeR.fr** package counts the 67 lexicogrammatical and functional
features described by Biber (1985, 1988) in French texts. These features
(tense, pronouns, passives, relative clauses, modals, negation and many more)
are widely used to compare registers, genres and text types with
multidimensional analysis.

It is the French companion of [pseudobibeR](https://github.com/browndw/pseudobibeR)
(English) and [pseudobibeR.es](https://github.com/browndw/pseudobibeR.es)
(Spanish). All three packages share the same interface and the same feature
numbers, so results can be compared across languages.

## Overview

The package does not tag texts itself. It reads the output of a parser,
[udpipe](https://bnosac.github.io/udpipe/en/) or
[spaCy](https://spacy.io/) (via [spacyr](https://spacyr.quanteda.io/)), and
counts features from the parts of speech, morphology and syntactic
dependencies it provides. The counts are only as good as the parse: unusual
spelling, informal punctuation or very specialized language can lower accuracy.

Most features carry over directly from English (passives, relative clauses,
modals). A few English features have no exact French equivalent. In those
cases the package counts the French construction that does the same job. For
example, f_15 counts infinitives used as nouns (*Fumer tue*), because French
uses the infinitive where English uses a gerund (*Smoking kills*). See
[Comparing languages](#comparing-languages).

## Installation

```r
# install.packages("remotes")
remotes::install_github("browndw/pseudobibeR.fr")
```

To install from a release file instead, download
`pseudobibeR.fr_<version>.tar.gz` from the
[releases page](https://github.com/browndw/pseudobibeR.fr/releases) and run:

```r
install.packages("/path/to/pseudobibeR.fr_<version>.tar.gz", repos = NULL, type = "source")
```

## Quick start

A typical workflow has three steps: read and clean the texts, parse them, and
count the features.

```r
library(readtext)
library(quanteda)
library(udpipe)
library(pseudobibeR.fr)

# 1. Read the texts and normalize typographic quotes (’ -> ')
docs <- readtext("corpus/*.txt", encoding = "UTF-8")
docs$text <- utf8::utf8_normalize(docs$text, map_quote = TRUE)
corp <- corpus(docs)

# 2. Parse with udpipe (the model is downloaded once)
model <- udpipe_download_model(language = "french-gsd")
ud_model <- udpipe_load_model(model$file_model)
parsed <- udpipe_annotate(ud_model, x = as.character(corp), doc_id = docnames(corp))

# 3. Count the features: one row per document
features <- biber(parsed)
```

**Normalizing apostrophes matters in French.** The udpipe French model does
not recognize the typographic apostrophe (’): *L’ami d’Anne* is parsed as a
single name, and *Il s’est levé* as a passive. With straight apostrophes both
are parsed correctly. `utf8::utf8_normalize(map_quote = TRUE)` makes this
conversion and keeps all accents. `biber()` only sees the parsed tokens, so
this step belongs before parsing.

### Using spaCy instead of udpipe

spaCy needs Python with a French model installed (for example
`fr_core_news_sm`; larger models parse more accurately). Request
morphology, which `biber()` uses for tense, mood and pronouns:

```r
library(spacyr)
spacy_initialize(model = "fr_core_news_sm")
parsed <- spacy_parse(corp, pos = TRUE, tag = TRUE, dependency = TRUE,
                      entity = FALSE, additional_attributes = "morph")
features <- biber(parsed)
spacy_finalize()
```

Both parsers work with the same detectors, but their accuracy differs on
individual constructions.

### Trying it with sample data

```r
library(pseudobibeR.fr)
biber(udpipe_samples)
biber(spacy_samples)
```

## Understanding the output

`biber()` returns a data frame with one row per document: a `doc_id` column
followed by the feature columns.

- **Rates per 1,000 words.** By default (`normalize = TRUE`) counts are
  divided by the number of words in the document and multiplied by 1,000, so
  documents of different lengths can be compared. Words exclude punctuation,
  symbols and numbers written in digits. Use `normalize = FALSE` for raw
  counts.
- **Two measures are not counts.** f_43 is the type/token ratio (lexical
  diversity) and f_44 is the mean word length in letters; neither is
  normalized. The `measure` argument selects the type/token ratio: `"MATTR"`
  (default), `"TTR"`, `"CTTR"`, `"MSTTR"` or `"none"` (which omits f_43).
  MATTR needs at least 200 words per document; if any document is shorter,
  plain TTR is used for every document and a message is shown.
- **Column names.** Columns use French-specific names, such as
  `f_09_impersonal` or `f_29_qui_subj`. Use `feature_names = "shared"` for the
  names shared with the English and Spanish packages (see
  [Comparing languages](#comparing-languages)).

## Checking results

`audit_features()` shows every occurrence behind a count, with its context:

```r
audit_features(parsed, feature = "f_18_agent_passives")
audit_features(parsed, category = "F", sample_n = 10, seed = 1)  # passives
```

It is a quick way to check a surprising value or to see what a feature
captures in your own texts.

## Linguistic features

The 67 features are grouped into the 16 categories used by Biber. Bold type in
the examples marks what is counted.

| Category | Features | Description |
|---|---|---|
| A. Tense and aspect | f_01–f_03 | Past, perfect, present |
| B. Place and time adverbials | f_04–f_05 | *ici*, *demain*… |
| C. Pronouns and pro-verbs | f_06–f_12 | Personal, demonstrative and indefinite pronouns; impersonal *il*; *le faire* |
| D. Questions | f_13 | Questions with an interrogative word |
| E. Nominal forms | f_14–f_16 | Nominalizations, nominal infinitives, other nouns |
| F. Passives | f_17–f_18 | Passives without and with an agent |
| G. Stative forms | f_19–f_20 | *être*, *il y a* |
| H. Subordination | f_21–f_38 | Complement, relative, participial and adverbial clauses |
| I. Prepositions, adjectives and adverbs | f_39–f_42 | |
| J. Lexical specificity | f_43–f_44 | Type/token ratio, word length |
| K. Lexical classes | f_45–f_51 | Connectors, hedges, intensifiers, discourse markers… |
| L. Modals | f_52–f_54 | Possibility, necessity, prediction |
| M. Specialized verb classes | f_55–f_58 | Communication, thinking, persuasion, *sembler* |
| N. Reduced forms | f_59–f_63 | Colloquial reductions and interrupted structures |
| O. Coordination | f_64–f_65 | Phrasal and clausal coordination |
| P. Negation | f_66–f_67 | Negative words and *pas* |

### Feature list

| Feature | Description | Description en français | Examples |
|---|---|---|---|
| **A. Tense and aspect** | | | |
| `f_01_past_tense` | Past tense (passé simple and imperfect) | Passé (passé simple et imparfait) | *Il **mangea** son repas.* *Nous **habitions** à Paris.* |
| `f_02_perfect_aspect` | Perfect aspect: *avoir/être* + participle | Aspect accompli : *avoir/être* + participe | *J'**ai** fini mon travail.* |
| `f_03_present_tense` | Present tense (indicative and subjunctive) | Présent (indicatif et subjonctif) | *Je **parle** français.* |
| **B. Place and time adverbials** | | | |
| `f_04_place_adverbials` | Place adverbials | Adverbiaux de lieu | *Les enfants jouent **dehors**.* |
| `f_05_time_adverbials` | Time adverbials | Adverbiaux de temps | *Je viendrai **demain**.* |
| **C. Pronouns and pro-verbs** | | | |
| `f_06_first_person_pronouns` | First-person pronouns | Pronoms de première personne | ***je**, **nous**, **me*** |
| `f_07_second_person_pronouns` | Second-person pronouns | Pronoms de deuxième personne | ***tu**, **vous**, **te*** |
| `f_08_third_person_pronouns` | Third-person personal pronouns | Pronoms personnels de troisième personne | ***il**, **elle**, **lui**, **leur*** |
| `f_09_impersonal` | Impersonal *il* (counterpart of English *it*): weather, *il faut*, *il est important de* | *il* impersonnel | ***Il** pleut.* ***Il** faut partir.* |
| `f_10_demonstrative_pronoun` | Demonstrative pronouns, including *c'est* | Pronoms démonstratifs, y compris *c'est* | ***C'**est vrai.* ***Celui-ci** est plus grand.* |
| `f_11_indefinite_pronouns` | Indefinite pronouns | Pronoms indéfinis | ***Quelqu'un** a frappé.* ***Personne** ne sait.* |
| `f_12_proverb_faire` | *faire* standing for an action already mentioned (counterpart of pro-verb *do*) | *faire* qui reprend une action déjà mentionnée | *Elle travaille dur, comme nous le **faisons** tous.* |
| **D. Questions** | | | |
| `f_13_wh_question` | Questions with an interrogative word | Questions avec un mot interrogatif | ***Qui** a pris mon stylo ?* ***Où** vas-tu ?* |
| **E. Nominal forms** | | | |
| `f_14_nominalizations` | Nouns derived with suffixes such as *-tion, -ment, -ité* | Nominalisations (*-tion, -ment, -ité*…) | *La **création** de cette loi…* |
| `f_15_nominal_infinitives` | Infinitives used as nouns (counterpart of English gerunds) | Infinitifs employés comme noms | ***Fumer** tue.* *Le **savoir** est une richesse.* |
| `f_16_other_nouns` | Other nouns | Autres noms | *la **table**, **Paris*** |
| **F. Passives** | | | |
| `f_17_agentless_passives` | Passives without an agent | Passifs sans complément d'agent | *Le repas **a été préparé** avec soin.* |
| `f_18_agent_passives` | Passives with an agent introduced by *par* | Passifs avec complément d'agent (*par*) | *Le roman a été écrit **par** Victor Hugo.* |
| **G. Stative forms** | | | |
| `f_19_copula_main_verb` | *être* as main verb | *être* comme verbe principal | *Elle **est** médecin.* |
| `f_20_existential` | Existential *il y a* | *il y a* existentiel | ***Il y a** beaucoup de monde.* |
| **H. Subordination** | | | |
| `f_21_que_verb_comp` | *que*-clauses after a verb | Complétives en *que* après un verbe | *Je crois **qu'**il viendra.* |
| `f_22_que_adj_comp` | *que*-clauses after an adjective | Complétives en *que* après un adjectif | *Je suis content **que** tu sois venu.* |
| `f_23_wh_clause` | Indirect questions and free relatives (*ce que, ce qui*) | Interrogatives indirectes et relatives sans antécédent | *Je ne sais pas **ce qui** s'est passé.* |
| `f_24_infinitives` | Infinitives used as verbs | Infinitifs verbaux | *Je veux **partir** maintenant.* |
| `f_25_present_participle` | Adverbial clauses with a present participle or gérondif | Propositions adverbiales au participe présent ou au gérondif | ***En travaillant** dur, tu réussiras.* |
| `f_26_past_participle` | Adverbial clauses headed by a past participle | Propositions adverbiales au participe passé | ***Construit** en un an, le bâtiment est impressionnant.* |
| `f_27_past_participle_reduced_relative` | Past participle modifying a noun (reduced relative clause) | Participe passé qui modifie un nom (relative réduite) | *Les lettres **écrites** par ma grand-mère sont précieuses.* |
| `f_28_present_participle_reduced_relative` | Present participle modifying a noun (reduced relative clause) | Participe présent qui modifie un nom (relative réduite) | *Les enfants **jouant** dans le parc sont bruyants.* |
| `f_29_qui_subj` | Relative clauses with *qui* (subject) | Relatives en *qui* (sujet) | *La femme **qui** habite ici est ma tante.* |
| `f_30_que_obj` | Relative clauses with *que* (object) | Relatives en *que* (complément) | *Le livre **que** j'ai lu était fascinant.* |
| `f_31_lequel_subj` | Relative clauses with *lequel*, as subject | Relatives en *lequel* (sujet) | *Les candidats, **lesquels** réussissent l'examen, obtiennent le poste.* |
| `f_32_lequel_obj` | Relative clauses with *lequel*, other functions (rare without a preposition) | Relatives en *lequel* (autres fonctions) | — |
| `f_33_pied_piping` | Relative clauses introduced by a preposition or *dont* | Relatives introduites par une préposition ou *dont* | *Les raisons **dont** il parle sont claires.* |
| `f_34_sentence_relatives` | Relative clauses that comment on a whole clause | Relatives qui reprennent toute une proposition | *Il a réussi, **ce qui** est remarquable.* |
| `f_35_causal` | Causal clauses with *parce que* | Subordonnées causales en *parce que* | *Je reste ici **parce qu'**il pleut.* |
| `f_36_concessive` | Concessive clauses with *bien que*, *quoique* | Subordonnées concessives | ***Bien qu'**il soit fatigué, il continue.* |
| `f_37_conditional` | Conditional clauses with *si*, *à moins que*, *sauf si* | Subordonnées conditionnelles | ***Si** tu viens, je serai content.* |
| `f_38_other_adv_sub` | Other adverbial subordinators | Autres subordonnants adverbiaux | *Il lit **pendant que** je cuisine.* |
| **I. Prepositions, adjectives and adverbs** | | | |
| `f_39_prepositions` | Prepositions | Prépositions | *Je vais **à** Paris **avec** mon ami.* |
| `f_40_adj_attr` | Adjectives modifying a noun | Adjectifs épithètes | *un livre **intéressant*** |
| `f_41_adj_pred` | Adjectives after a linking verb | Adjectifs attributs | *Le chat est **noir**.* |
| `f_42_adverbs` | Other adverbs (not counted in f_04, f_05, f_46–f_48) | Autres adverbes | *Elle parle **rapidement**.* |
| **J. Lexical specificity** | | | |
| `f_43_type_token` | Type/token ratio (lexical diversity) | Diversité lexicale | — |
| `f_44_mean_word_length` | Mean word length in letters | Longueur moyenne des mots | — |
| **K. Lexical classes** | | | |
| `f_45_conjuncts` | Connectors between sentences | Connecteurs | ***Cependant**, je préfère le thé.* |
| `f_46_downtoners` | Downtoners (weaken a statement) | Atténuateurs | *Il est **à peine** capable de marcher.* |
| `f_47_hedges` | Hedges (mark uncertainty) | Marqueurs d'approximation | *Il y avait **environ** cinquante personnes.* |
| `f_48_amplifiers` | Amplifiers (strengthen a statement) | Intensifieurs | *C'est **très** important.* |
| `f_49_emphatics` | Emphatics | Marqueurs d'emphase | *Je l'ai **vraiment** fait hier.* |
| `f_50_discourse_particles` | Discourse markers | Marqueurs discursifs | ***Eh bien**, je ne sais pas.* |
| `f_51_demonstratives` | Demonstratives before a noun | Déterminants démonstratifs | ***Ce** livre est intéressant.* |
| **L. Modals** | | | |
| `f_52_modal_possibility` | Possibility (*pouvoir*) | Possibilité (*pouvoir*) | *Je **peux** venir demain.* |
| `f_53_modal_necessity` | Necessity and obligation (*devoir*, *il faut*) | Nécessité et obligation | *Tu **dois** finir.* *Il **faut** partir.* |
| `f_54_modal_predictive` | Prediction: future, conditional, *aller* + infinitive | Prédiction : futur, conditionnel, *aller* + infinitif | *Il **sera** là.* *Je **vais** aller au marché.* |
| **M. Specialized verb classes** | | | |
| `f_55_verb_public` | Verbs of communication | Verbes de communication | *Elle a **annoncé** son départ.* |
| `f_56_verb_private` | Verbs of thinking and feeling | Verbes de pensée et de perception | *Je **pense** qu'il a raison.* |
| `f_57_verb_suasive` | Verbs of persuasion and request | Verbes de persuasion et de demande | *Je te **propose** de venir.* |
| `f_58_verb_seem` | *sembler*, *paraître* | *sembler*, *paraître* | *Il **semble** fatigué.* |
| **N. Reduced forms** | | | |
| `f_59_reduced_forms` | Colloquial reduced forms with a full alternative | Formes réduites familières | *Il est **p'tit**.* ***T'as** vu ça ?* |
| `f_60_reduced_complement` | Reduced complement in place of a *que*-clause, after verbs like *croire* and *dire* | Complétive réduite au lieu de *que* | *Je crois **avoir compris**.* *Il dit **avoir vu** le voleur.* |
| `f_61_stranded_resumptive` | Colloquial alternatives to a preposition + relative: orphan preposition or repeated pronoun | Préposition orpheline ou pronom de reprise | *C'est la fille que je sors **avec**.* |
| `f_62_split_infinitive` | Adverb between a preposition and an infinitive (adverbs that normally follow the verb) | Adverbe entre la préposition et l'infinitif | *Il est parti sans **vraiment** savoir pourquoi.* |
| `f_63_split_auxiliary` | Adverb between an auxiliary or modal and its verb | Adverbe entre l'auxiliaire et le verbe | *Il a **probablement** été vu.* |
| **O. Coordination** | | | |
| `f_64_phrasal_coordination` | Coordination of words or phrases | Coordination de syntagmes | *des pommes **et** des oranges* |
| `f_65_clausal_coordination` | Coordination of clauses | Coordination de propositions | *Il pleut **et** nous restons à l'intérieur.* |
| **P. Negation** | | | |
| `f_66_neg_synthetic` | Negative words | Mots négatifs | ***Personne** n'est venu.* *Il n'a **jamais** vu la mer.* |
| `f_67_neg_analytic` | Negation with *pas* (with or without *ne*) | Négation avec *pas* | *Je ne parle **pas** anglais.* |

For the exact rules behind each count, including exclusions and known parser
limitations, see the [technical reference](FEATURES_TECHNICAL.md).

## Comparing languages

The feature numbers (`f_01` … `f_67`) refer to the same feature in English,
Spanish and French. Only the column names differ, where a French name
describes the counted construction better. To combine or compare results
across languages, use the shared names:

```r
biber(parsed, feature_names = "shared")

# or for the whole session, in all three packages:
options(pseudobibeR.feature_names = "shared")
```

The [cross-linguistic feature table](https://github.com/browndw/pseudobibeR/blob/main/CROSS_LINGUISTIC_FEATURES.md)
shows how each feature is realized in the three languages.

## Further documentation

- [FEATURES_TECHNICAL.md](FEATURES_TECHNICAL.md): exact counting rules for
  each feature.
- [NEWS.md](NEWS.md): changes between versions.
- [CONTRIBUTING.md](CONTRIBUTING.md): editing word lists, rebuilding package
  data and running tests.

## Citation

Please cite Biber's original work and this package:

> Biber, D. (1985). Investigating macroscopic textual variation through
> multifeature/multidimensional analyses. *Linguistics*, 23(2), 337–360.
> <https://doi.org/10.1515/ling.1985.23.2.337>

> Biber, D. (1988). *Variation across speech and writing*. Cambridge
> University Press. <https://doi.org/10.1017/CBO9780511621024>

> Brown, D. W., Seghier, M., & Millour, A. (2026). *pseudobibeR.fr: Aggregate
> counts of French linguistic features*. R package.
> <https://github.com/browndw/pseudobibeR.fr>

## License

MIT © the package authors.

# pseudobibeR.fr feature table (French features)

Technical reference for Biber's (1988) 67 features as implemented in
`pseudobibeR.fr`. It describes the **actual behavior** of the detectors in
`R/*.R` on UDPipe `french-gsd-ud-2.5-191206` output, not only the theoretical
intent. Where the code and other documentation disagree, this file follows the
code and lists the disagreement at the end. Decision IDs (D-NN) refer to
`CROSS_LANGUAGE_ALIGNMENT.md` in the family repository.

The **English label** column is a functional label; the **French label** column
names the French construction. Inventories are cited by name
(`dict$f_04_place_adverbials`, `word_lists$orphan_prepositions`); see
`data-raw/dict.yaml` and `data-raw/word_lists.yaml` for full lists.

## Feature names

- Output columns use **French display names** by default (D-54), defined in
  `data-raw/feature_names.yaml` and exported as the `feature_names` dataset.
  Twenty features are renamed (for example `f_15_nominal_infinitives`); the
  others keep the shared identifier.
- `feature_names = "shared"` in `biber()` or `audit_features()` returns the
  identifiers shared by `pseudobibeR`, `pseudobibeR.es` and `pseudobibeR.fr`
  (for example `f_15_gerunds`). `options(pseudobibeR.feature_names = "shared")`
  changes the default. `audit_features(feature = )` accepts either name.
- The `f_NN` number identifies the **same feature in all three packages**.
  Detectors, evidence, tests and the decision log use the shared identifiers.
- In the tables below, the **Feature** column shows the display name, followed
  by the shared identifier in parentheses when the two differ.

## Invariants

1. **Output.** `biber()` accepts `udpipe_connlu` and `spacyr_parsed` objects
   only (D-39). It returns `doc_id` plus 67 feature columns (66 with
   `measure = "none"`), one row per document in order of first appearance.
2. **Counts are occurrences.** Each raw count is the number of evidence rows for
   the feature, deduplicated by document, sentence and word ID.
   `audit_features()` returns exactly those rows, so rows per document equal
   `biber(normalize = FALSE)`. Multiword constructions give one row anchored on
   their first word, with the full span shown as the keyword. Matching never
   crosses sentence boundaries. Features are not mutually exclusive unless a
   row says so.
3. **Normalization** (D-38). Counts are per 1,000 word tokens. The denominator
   is the number of UD word rows after removing multiword-token container rows
   (`du` counts through its components `de` + `le`), `PUNCT`, `SYM`, `SPACE` and
   any token ending in a digit (`42`, `3,5`). Function words, elided clitics
   (`l'`, `j'`) and spelled-out numerals count. f_43 and f_44 are never
   normalized. A document with no word tokens gets `NA` rates and a warning.
4. **Matching conventions.** Tokens and lemmas are lower-cased. Typographic
   apostrophes (U+2019, U+2018, U+02BC) are mapped to `'` for list matching.
   The data builders add unaccented twins of accented entries (`très` → `tres`),
   except contrastive forms in `word_lists$accent_contrastive_forms` (`là`/`la`,
   `sûr`/`sur`, `à`/`a`, `où`/`ou`) and nominalization suffixes (D-34).
5. **Pre-processing.** Normalize text before parsing with
   `utf8::utf8_normalize(text, map_quote = TRUE)` (D-36). In-package apostrophe
   tolerance only applies after parsing. With curly apostrophes, UDPipe
   French-GSD mis-segments the sentence (`J’ai vu l’homme` → `J’` PROPN, `l’`
   PROPN, `homme` ADP), which corrupts almost every feature.
6. Parser-sensitive distinctions require independent validation. Known UDPipe
   French-GSD limitations are marked ⚠️ and summarized at the end.

---

## A. Tense and aspect

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_01_past_tense` | Past tenses (indicative and subjunctive) | Temps du passé (indicatif et subjonctif) | Finite `VERB`/`AUX` (`VerbForm=Fin` or missing) with `Tense ∈ {Past, Imp, Pqp}`, any mood (D-33): *passé simple*, *imparfait* (including subjunctive), auxiliary of the *plus-que-parfait*. *Il mangea rapidement son repas.* (1); *Nous avions déjà terminé quand elle arriva.* (2). | The *passé composé* is not past here: *Le chat a marché* gives f_01 = 0, f_02 = 1, f_03 = 1 (D-33). The auxiliary `avions` counts in f_01 and f_02 (deliberate overlap). The conditional is f_54. |
| `f_02_perfect_aspect` | Perfect aspect (*avoir*/*être* + past participle) | Aspect accompli (auxiliaire *avoir*/*être* + participe passé) | One occurrence per participle governed by `avoir`/`être` in an `aux*` or `cop` relation (not `*pass`), anchored on the auxiliary. The head must be `VERB`/`AUX`/`ADJ`/`NOUN` with `VerbForm=Part` and not `Voice=Pass`. Covers *passé composé*, *plus-que-parfait*, *futur antérieur*, past conditional and pronominal forms (*Elle s'est levée*). *J'ai fini mon travail.* | Passive auxiliaries are excluded, but the tense auxiliary of a perfect passive counts (*Il a été vu*: f_02 = 1, f_17 = 1). Nouns in *-é/-i/-u* (*C'est un ami*) do not count. ⚠️ A participle tagged NOUN is lost (*Elle était partie avant midi* = 0). |
| `f_03_present_tense` | Present tense (indicative and subjunctive) | Présent (indicatif et subjonctif) | Finite `VERB`/`AUX` with `Tense=Pres` and `Mood` not `Cnd`/`Imp` (D-33). Includes the present subjunctive and the present auxiliary of the *passé composé* (`a` in *il a mangé*). *Je parle français couramment.*; *Demain, je pars à l'aube.* | ⚠️ UDPipe marks the conditional `Tense=Pres\|Mood=Cnd`; the mood filter keeps it out (it is f_54). Imperatives excluded. `va` in *va partir* counts here and in the f_54 span. |

## B. Place and time adverbials

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_04_place_adverbials` | Place adverbs and adverbial phrases | Adverbes et locutions de lieu | Maximal sentence-local matches of `dict$f_04_place_adverbials`, one row per phrase: `ici`, `là`, `dehors`, `ailleurs`, `partout`, `au-dessus`, `à l'étranger`, `loin d'ici`. *Les enfants jouent dehors.*; *Il habite loin d'ici.* | No POS filter: prepositional `devant`/`derrière` (*devant la maison*) also count, and also count in f_39. A longer match of another dictionary feature takes precedence. Matched words leave f_42 (D-15). |
| `f_05_time_adverbials` | Time adverbs and adverbial phrases | Adverbes et locutions de temps | Same mechanism with `dict$f_05_time_adverbials`: `hier`, `demain`, `maintenant`, `bientôt`, `toujours`, `souvent`, `jamais`, `plus tard`, `de nos jours`. *Je viendrai demain matin.* | `ensuite` after `,`/`;` (and not among the first two words) moves to f_45. `jamais` also counts in f_66. `toujours`, `souvent`, `jamais` therefore leave f_42. `une fois` matches any use (including *une fois que*). Not sense-disambiguated. |

## C. Pronouns and pro-verbs

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_06_first_person_pronouns` | First-person pronouns | Pronoms de la première personne | Union, one per word: forms in `dict$f_06_first_person_pronouns` (`je`, `me`, `m'`, `moi`, `moi-même`, `nous`, `nous-mêmes`, `notre`) plus any `PRON` with `Person=1` and `PronType=Prs`. *Je pense que nous devrions partir.* (2) | ⚠️ Possessive determiners `mon/ma/mes/nos` are not counted: they are not listed and UDPipe gives possessive `DET` no `Person` (*Mon frère et moi…* = 1). `notre` counts through the dictionary. Morphology makes the count robust to UDPipe lemmatizing `je`/`nous` as `il`. Generic `on` is not counted (D-50). |
| `f_07_second_person_pronouns` | Second-person pronouns | Pronoms de la deuxième personne | Dictionary forms (`tu`, `t'`, `toi`, `vous`, `vous-même`, possessives `ton`, `ta`, `tes`, `votre`, `vos`) plus `PRON` with `Person=2` and `PronType=Prs` (`te`). *Tu dis que vous êtes sortis ?* (2) | `vous` counts whether plural or polite singular. Second-person possessives count, unlike most first- and third-person ones. Reduced subject `t'` (*t'as*) also counts in f_59. |
| `f_08_third_person_pronouns` | Third-person personal pronouns | Pronoms personnels de la troisième personne | Dictionary forms (`il`, `ils`, `elle`, `elles`, `lui`, `eux`, `eux-mêmes`, `leur`, `leurs`, `les siens`) plus `PRON` with `Person=3` and `PronType=Prs` (object clitics, reflexive `se`), minus f_09 occurrences. *Elle lui a donné son livre.* (2) | `ce`/`ça`, partitive `en` and generic `on` are counted nowhere (D-50). `son/sa/ses` are not counted, but determiner `leur(s)` is (surface match). Impersonal `il` not caught by the f_09 rules stays here. |
| `f_09_impersonal` (`f_09_pronoun_it`) | Impersonal or expletive subject pronoun | Pronom sujet impersonnel ou explétif | `PRON` with surface form `il`, `ce`, `c'`, `cela` or `ça` that is (a) any `expl*` dependent, or (b) the `nsubj*` of: a weather verb (`pleuvoir`, `neiger`, `geler`, `grêler`, `venter`, `bruiner`, `tonner`), directly or through an `xcomp` (*il pourrait pleuvoir*); `faire` with a weather complement (`froid`, `chaud`, `beau`, `nuit`…); `falloir`; an `ADJ` with its own clausal complement (que-clause or infinitive marked `de`/`à`/`que`); or `sembler`, `paraître`, `demeurer`, `rester`, `suffire`, `convenir`, `arriver`, `importer`, `valoir`, `agir`, `advenir`, `manquer` with a clausal complement. *Il pleut depuis ce matin.*; *Il y a trois ans, nous habitions à Paris.* | Matched on the surface form, so `elle` (lemma `il` in UDPipe) never qualifies. Excluded from f_08 and f_10. Referential *il dit que…* and *il semble fatigué* do not count. ⚠️ Relies on the parser's `expl` label and clausal attachments: *Il est clair qu'il est censé.* gives 2. Overlaps: f_20 (`il y a`), f_22, f_53 (`il faut`), f_58 (`il semble que`). |
| `f_10_demonstrative_pronoun` | Demonstrative pronouns | Pronoms démonstratifs | `PRON` whose form is in `word_lists$demonstrative_matchlist` (`ce`, `c'`, `ça`, `cela`, `ceci`, `celui`, `celle(s)`, `ceux`, with `-ci`/`-là`) and `PronType=Dem` or no `PronType` (D-50). Clefts count. *C'est vrai.*; *Celui-ci est plus grand que celui-là.* (2) | Excludes f_09 occurrences and `ce`/`c'` directly before `que`/`qu'`/`qui`/`dont` (*Ce que tu dis est vrai* = 0; f_23/f_34). Determiners are f_51. |
| `f_11_indefinite_pronouns` | Indefinite pronouns | Pronoms indéfinis | Matches of `dict$f_11_indefinite_pronouns`, then a pronoun-use gate (D-52). Phrases count once (`quelque chose`, `tout le monde`, `n'importe qui/quoi`, `quelques-uns`). `quelqu'un`, `chacun`, `quiconque`, `autrui` count with any tag. `personne` counts as `PRON`, or as a determinerless NOUN in a sentence with `ne`. `rien` counts without a determiner. `tout`/`aucun`/`nul` count only as `PRON` outside a `det` relation, plus `tout` tagged ADV on a verb (not before `en`/`à`). *Quelqu'un a frappé à la porte.* | *Une personne est venue.* = 0. Overlap with f_66 (`personne`, `rien`, `aucun`, `nul`) is intended (D-52). *Il a tout mangé* counts here (and in f_63). `certains`, `plusieurs` are not listed. A longer phrase of another feature wins (`en quelque sorte` is f_47). |
| `f_12_proverb_faire` (`f_12_proverb_do`) | Pro-verb *faire* | Pro-verbe *faire* | Lexical `faire` (`VERB`/`AUX`, not in an `aux*` relation) with an `obj` pronoun from `word_lists$proverb_object_pronouns` (`le`, `l'`). *Elle travaille dur, comme nous le faisons tous.*; *…mais il ne l'a pas fait.* | Excludes causative `faire` (`aux:caus`) and `se faire` (D-22). No antecedent check: `le` referring to a noun also counts. Other proforms (*faire ça*, *en faire autant*) are not counted. |

## D. Questions

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_13_wh_question` | Direct wh-questions | Interrogatives partielles directes | Words with lemma `qui`, `que`, `quoi`, `où`, `quand`, `comment`, `pourquoi`, `combien`, `quel(le)(s)`, `lequel` family, tagged `ADV`/`PRON`/`DET`/`ADJ`, in a sentence containing `?`, and not inside a relative clause (head not an `acl*`). Position is free; each word counts. *Qui a pris mon stylo ?* | Requires a literal `?`. An embedded interrogative in a `?` sentence counts in f_13 and f_23 (*Tu sais où il est ?*). ⚠️ `quand` tagged SCONJ is lost (*Quand partez-vous ?* = 0). |

## E. Nominal forms

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_14_nominalizations` | Nominalizations | Nominalisations | `NOUN` whose lemma ends in an entry of `word_lists$nominalization_suffixes` (`-tion`, `-sion`, `-isation`, `-ment`, `-age`, `-ité`, `-ance`, `-ence`, `-ure`, `-eur`, `-esse`, `-erie`, `-ade`, `-trice`, and plurals), minus `nominalization_stoplist` (`moment`, `document`, `image`, `nature`, `été`…) and f_15 words. *La création de cette loi a pris plusieurs mois.* | A suffix heuristic, not derivational analysis: `-eur` also catches agent nouns (*professeur*). PROPN never counts. Lemma errors propagate. |
| `f_15_nominal_infinitives` (`f_15_gerunds`) | Infinitives functioning as nouns | Infinitifs nominaux | Infinitive-shaped words (`-er/-ir/-re/-oir`; `VERB`/`AUX`/`NOUN`/`PROPN`; `VerbForm=Inf` or missing; not `aux`/`cop`). NOUN/PROPN need `Inf` morphology or a known form (`nominal_infinitive_lemmas`, verb inventories). Nominal if (a) it has a `det` DET or `amod` ADJ and no verbal dependents (objects other than *de*-genitives, ADV `advmod` other than `ne`, verbal `aux`/`ccomp`/`xcomp`, subjects); (b) it is a `conj` of such a word; or (c) it is an `nsubj`/`csubj` placed before its head (D-23, D-43). *Fumer tue.*; *Le boire et le manger sont essentiels.* (2) | Excluded from f_14, f_16, f_24, f_60 and f_62. Postposed subjects (*Il est important de se reposer* = 0), objects and prepositional infinitives stay f_24. The *gérondif* is f_25 (D-43). Lexicalized nouns identical to a known infinitive count here, not in f_16 (*Le pouvoir corrompt.* = 1). |
| `f_16_other_nouns` | Other nouns | Autres noms | Every `NOUN`/`PROPN` without a hyphen, minus f_14, f_15 and recovered *gérondifs* (f_25). | Hyphenated nouns (*grand-mère*, *après-midi*) are counted nowhere unless they match f_14. ⚠️ Curly-apostrophe misparses add spurious PROPN rows. |

## F. Passives

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_17_agentless_passives` | Passives without an agent | Passifs sans complément d'agent | One occurrence per `VERB` heading an `aux:pass`/`auxpass` dependent with no agent (see f_18). Evidence is the span from the auxiliary to the participle. *Le repas a été préparé avec soin.*; *La maison sera vendue la semaine prochaine.* | Pronominal passives (*se vendre*) are not counted. ⚠️ Depends on the `aux:pass` label: UDPipe also gives it to stative *être* + participle (*La porte est restée fermée*, *Il est très fatigué*, *Bien qu'il soit fatigué* all = 1), and occasionally to intransitive *être* + participle (*Il est parti*). |
| `f_18_agent_passives` (`f_18_by_passives`) | Passives with an agent | Passifs avec complément d'agent | The same passive predicates with an agent: an `obl:agent`/`agent` dependent, an `obl*`/`nmod*` dependent whose `case` is `par`, or a `par` attached directly to the predicate. *Le roman a été écrit par Victor Hugo.* | Any `par` oblique qualifies, so path, means and manner phrases are false positives (*vu par la fenêtre*, *par hasard*). *de*-agents (*aimé de tous*) count only if labelled `obl:agent`. `par-dessus` is a different token. |

## G. Stative forms

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_19_copula_main_verb` (`f_19_be_main_verb`) | *être* as main verb | *Être* verbe principal (copule) | Every token with lemma `être` whose relation is not `aux*` (copula, root, other). *Elle est médecin.*; *Ce bruit est comme un avertissement.* | Tense and passive auxiliaries are excluded. Other linking verbs are not counted (`sembler`/`paraître` are f_58). ⚠️ *être* labelled `aux:pass` before a stative participle goes to f_17, not here. |
| `f_20_existential` (`f_20_existential_there`) | Existential *il y a* | Construction existentielle *il y a* | Lemma `avoir` (`VERB`/`AUX`) immediately preceded by lemma `y` and, two words back, a `PRON` with lemma `il`; any tense. *Il y a beaucoup de monde aujourd'hui.*; *Il y avait trois personnes dans la salle.* | ⚠️ Strict adjacency: *il n'y a pas*, *il y en a* and *y a-t-il* are not counted. Subjectless *y a* is f_59. ⚠️ UDPipe lemmatizes `elle`/`je` as `il`, so *Elle y a mis son livre* counts. No semantic check (*il y a trois ans* counts). Impersonal `il` also counts in f_09. |

## H. Subordination

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_21_que_verb_comp` (`f_21_that_verb_comp`) | *que*-clauses complementing a verb | Complétives en *que* régies par un verbe | `que`/`qu'` (`SCONJ`, `mark`) whose clause head is a `ccomp`/`xcomp`/`csubj` (or subtype) of a `VERB`/`AUX`. The clause head can be any POS. *Je crois qu'il viendra demain.*; *Elle pense que c'est une bonne idée.* | `que` inside `parce que`, `bien que`, `à moins que`, `à condition que` is excluded. Clauses on nouns (*l'idée que*) are counted nowhere; on adjectives they are f_22. Relative `que` is f_30. |
| `f_22_que_adj_comp` (`f_22_that_adj_comp`) | *que*-clauses complementing an adjective | Complétives en *que* régies par un adjectif | `que` (`SCONJ`, `mark`) whose clause attaches, by any non-`acl` relation, to an `ADJ`. *Je suis content que tu sois venu.*; *Il est improbable que cela arrive.* | Infinitival complements of adjectives (*facile de partir*) are not counted here (f_24; their `de` falls into f_38 ⚠️). Impersonal *il est ADJ que* also counts in f_09. |
| `f_23_wh_clause` | Indirect questions and verb-argument free relatives | Interrogatives indirectes et relatives libres compléments | (a) `qui`, `que`, `quoi`, `où`, `quand`, `comment`, `pourquoi`, `combien` or the `lequel` family, tagged `PRON`/`ADV`/`DET`/`ADJ`, in an `obj`/`obl`/`nsubj`/`iobj`/`advmod`/`mark`/`det` relation, whose head heads a `ccomp`/`csubj`/`xcomp`/`obj`/`nsubj`/`obl`/`advcl` clause. Relative-tagged `qui`/`que`/`quoi`/`lequel` are excluded (pseudo-relatives); `où` is kept. (b) `ce` + `que`/`qu'`/`qui`, not sentence-initial or after punctuation, with `ce` in an argument relation to a `VERB`: one span (D-37). *Je ne sais pas ce qui s'est passé.* | Excludes sentence relatives (f_34) and free relatives under a copula or preposition. `ce dont` is not included. Indirect-question `si` is counted nowhere. An embedded wh-word in a `?` sentence also counts in f_13. Because `advcl` is accepted, an ADV-tagged wh-word heading an adverbial clause can count. ⚠️ Imperative + clitic parses fail (*Dis-moi ce que tu veux*, *Explique-moi pourquoi tu es parti* = 0). |
| `f_24_infinitives` | Verbal infinitives | Infinitifs verbaux | `VERB`/`AUX` with `VerbForm=Inf`, minus f_15. Includes modal complements, prepositional and purpose infinitives, postposed subjects and auxiliary infinitives. *Je veux partir maintenant.*; *Il commence à douter.* | Overlap with f_60 and f_62 is intended. ⚠️ Infinitives tagged NOUN without f_15 evidence are missed. |
| `f_25_present_participle` | Adverbial present participles and *gérondif* | Participe présent et gérondif adverbiaux | A present participle (`VerbForm=Ger`, or `Part` with `Tense=Pres/Imp`) in `advcl`/`ccomp` that is clause-initial, follows punctuation, or follows `en` (`mark`/`case`). Plus ⚠️ *gérondifs* tagged NOUN: NOUN after `en`, ending in `-ant`, in `obl`/`advcl`/`nmod`, without a determiner, not in `gerondif_noun_stoplist` (`enfant`, `restaurant`, `pendant`…). *En travaillant dur, tu réussiras.*; *Il est parti en chantant.* | Recovered NOUN *gérondifs* are removed from f_16. A medial participle without `en` or punctuation is not counted. ⚠️ *En se remplissant la bouche de biscuits, Joe est sorti en courant.* gives 1, not 2. |
| `f_26_past_participle` | Adverbial past participles | Participe passé en construction adverbiale | A past participle (`VERB`/`AUX`/`ADJ` with `VerbForm=Part` and past or no tense) in `advcl`/`ccomp`, or in `acl` when clause-initial or after punctuation; it must be clause-initial, after punctuation, or after `en`. *Construit en un an, le bâtiment est impressionnant.*; *Épuisé, il s'est endormi aussitôt.* | f_27 occurrences are excluded. Medial absolute constructions without punctuation are missed. |
| `f_27_past_participle_reduced_relative` (`f_27_past_participle_whiz`) | Postnominal past participles (reduced relatives) | Participes passés postnominaux (relatives réduites) | A past participle in a plain `acl` relation (not `acl:relcl`) to a `NOUN`/`PROPN`, with no `aux`/`cop`/`nsubj` dependent, not clause-initial and not after punctuation. *Les lettres écrites par ma grand-mère sont précieuses.* | Comma-delimited participles go to f_26. ⚠️ Participles tagged `ADJ amod` are f_40. ⚠️ *La maison construite l'année dernière…* is parsed with *construite* as main verb (= 0). |
| `f_28_present_participle_reduced_relative` (`f_28_present_participle_whiz`) | Postnominal present participles | Participes présents postnominaux | Same rule with a present participle. *Les enfants jouant dans le parc sont bruyants.*; *La personne tenant le micro est le directeur.* | Active in French, unlike the Spanish compatibility zero (D-35). Verbal adjectives without participle morphology (`ADJ`) are f_40/f_41. |
| `f_29_qui_subj` (`f_29_that_subj`) | *qui* subject relatives | Relatives en *qui* (sujet) | Relative `qui` (`PronType=Rel`, or the lemma in an argument/`mark` relation) in a clause attached as `acl*` to a `NOUN`/`PROPN`, not after a preposition. French marks the role by form. *Le chien qui m'a mordu a été retrouvé.* | Exclusive partition (D-24, D-25): f_33 first, then f_34, then f_29–f_32. Clefts (*C'est Marie qui…*) count when parsed as a relative on the noun. Free relatives (*celui qui*, *ce qui*) are excluded. `où` relatives (*la ville où j'habite*) are counted in no relative bin. |
| `f_30_que_obj` (`f_30_that_obj`) | *que* object/complement relatives | Relatives en *que* (objet/complément) | Relative `que`/`qu'` with the same recognition, minus relatives whose `que` starts an f_61 match. *Le livre que j'ai lu était fascinant.* | Same partition and exclusions as f_29. ⚠️ Curly apostrophes break `qu'` (see invariant 5). |
| `f_31_lequel_subj` (`f_31_wh_subj`) | *lequel* subject relatives | Relatives en *lequel* (sujet) | `lequel`/`laquelle`/`lesquels`/`lesquelles` without a preceding preposition, with an `nsubj*` relation, in a nominal relative. *Les candidats, lesquels réussissent l'examen obtiennent le poste.* | Rare, formal. Role comes from the parser. |
| `f_32_lequel_obj` (`f_32_wh_obj`) | *lequel* non-subject relatives | Relatives en *lequel* (non sujet) | The same family, any other relation, without a preceding preposition. | Very rare without a preposition; `data-raw/french_examples.yaml` has no positive example. `dont` is f_33 only (D-24). Prepositional `lequel` is f_33. |
| `f_33_pied_piping` | Pied-piped relatives | Relatives à préposition antéposée | Nominal relative with `dont`, a contracted form (`auquel`, `auxquel(le)s`, `duquel`, `desquel(le)s`), or `ADP` + `qui`/`quoi`/`lequel` family. One per relative pronoun. *Les raisons dont il parle sont claires.*; *Voici le lieu dans lequel nous nous sommes rencontrés.* | Takes precedence over f_29–f_32 (D-24). Non-nominal antecedents (*ce dont*) are excluded. Colloquial competitors are f_61 (D-29, D-47). |
| `f_34_sentence_relatives` | Sentence relatives | Relatives à antécédent phrastique | A relative pronoun immediately after `ce`, `cela`, `ceci`, `celui`, `celle(s)` or `ceux`, itself after punctuation; span from the anchor to the pronoun. *Il a réussi son examen, ce qui est remarquable.* | Sentence-initial *Ce que tu dis est vrai* is not f_34. No check that the antecedent is a clause: *…, celui qui parle* counts (and *celui* is also f_10). Excluded from f_29/f_30. |
| `f_35_causal` (`f_35_because`) | Causal subordinator | Subordonnant causal *parce que* | `parce` followed by `que`/`qu'`, one span (D-44). *Je reste à la maison parce qu'il pleut.* | `car` (a coordinator, like *for*) is not counted (*Il est parti tôt car…* = 0). `puisque`, `comme` are f_38. |
| `f_36_concessive` (`f_36_though`) | Concessive subordinators | Subordonnants concessifs | `quoique`/`quoiqu'` (`SCONJ`); `bien` + `que`/`qu'` tagged `SCONJ`, one span (D-44). *Bien qu'il soit fatigué, il continue à travailler.*; *Quoiqu'il dise, je partirai.* | `même si` is f_37; `encore que`, `alors que` are f_38. ⚠️ The `bien` of *bien que* also counts in f_50 (no cross-check between the dictionary and this block). |
| `f_37_conditional` (`f_37_if`) | Conditional subordinators | Subordonnants conditionnels | `si`/`s'` (`SCONJ`, `mark`), including *même si*; `sauf si` and `à moins que` as one span each (D-44). *Si tu viens, je serai content.*; *Je sortirai sauf s'il pleut.* | Indirect-question `si` is excluded when its clause is `ccomp`/`xcomp` of a listed question/knowledge verb (`demander`, `savoir`, `ignorer`, `voir`, `dire`…): *Je me demande s'il viendra.* = 0. ⚠️ UDPipe also attaches some postposed conditionals as `ccomp`; they are kept unless the governor is listed. `à condition que` is f_38. |
| `f_38_other_adv_sub` | Other adverbial subordinators | Autres subordonnants adverbiaux | Every `SCONJ`/`ADP`/`ADV` with a `mark` relation that is not `que`, `parce`, `quoique` or `si` and lies outside the f_35–f_37 phrases, plus `à condition que` (one span). *Tandis que je travaillais, elle dormait.*; *Comme il pleuvait, je suis resté.* | ⚠️ There is no inventory: infinitival markers labelled `mark` also count (*Il est facile de convaincre Jean.* = 1; frozen in the edge-case fixture). Includes `quand`, `lorsque`, `puisque`, `comme`, `encore que`. |

## I. Prepositional phrases, adjectives and adverbs

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_39_prepositions` | Prepositions | Prépositions | Tokens in a `case` relation that are `ADP` or head a `fixed` ADP (a fixed multiword preposition counts once). Contractions count through their `de`/`à` component. *Je vais à Paris avec mon ami.* (2) | `de`/`à` labelled `mark` before infinitives are not counted. ⚠️ A multiword preposition counts once only if parsed with `fixed`: *à cause de* is parsed as `à` + NOUN `cause` + `de` (f_39 = 2, plus one f_16 noun). Overlaps with f_04 (`devant`). |
| `f_40_adj_attr` | Attributive adjectives | Adjectifs épithètes | `ADJ` in `amod`, without a hyphen, lemma not `tout`, plus `ADJ` conjuncts of a counted adjective. Pre- and postnominal. *C'est une belle maison.*; *J'ai acheté un livre intéressant.* | Hyphenated adjectives are excluded. Disjoint from f_41. |
| `f_41_adj_pred` | Predicative adjectives | Adjectifs attributs | `ADJ` heading a `cop` dependent, or `ADJ` in `xcomp`/`acomp` of a verb in `word_lists$linking_matchlist` (`être`, `devenir`, `rester`, `demeurer`, `sembler`, `paraître`, `avoir`), plus coordinated adjectives; minus f_40. *Le chat est noir.*; *Cette histoire est vraie.* | ⚠️ Participial adjectives whose *être* is labelled `aux:pass` are not here (f_17). |
| `f_42_adverbs` | Adverbs (residual) | Adverbes (catégorie résiduelle) | `ADV` tokens except `ne`/`n'` (D-42), minus any adverb inside an f_04, f_05, f_46, f_47 or f_48 occurrence (D-15). *Elle parle très rapidement.* (1; `très` is f_48) | Emphatics, conjuncts, discourse particles and negation are not excluded: *ne … pas* adds 1 (`pas`). `PART` tokens are not counted. *Il travaille toujours avec soin.* = 0 (`toujours` is f_05). ⚠️ Mis-tagged ADV count (`vas` in *Où vas-tu ?*). |

## J. Lexical specificity

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_43_type_token` | Lexical diversity | Diversité lexicale | Types are lower-cased surface forms joined to the fine tag (`xpos`, or UPOS when missing or `_`). `PUNCT`/`SYM`/`SPACE` and digit-final tokens are removed; no lemmatization, no dictionary compounding. **MATTR** by default (`quanteda.textstats::textstat_lexdiv()`); if any document has fewer than 200 retained tokens, the whole corpus uses TTR (with a message). Explicit `"TTR"`, `"CTTR"`, `"MSTTR"` are honored; `"none"` drops the column. | One measure per corpus. The token population equals the normalization denominator. Empty documents get `NA` and a warning. Tag granularity affects types. Not normalized; not auditable. |
| `f_44_mean_word_length` | Mean word length | Longueur moyenne des mots | Mean number of characters of tokens matching `^\p{L}+$` (letters only, accents included), unrounded (D-51). | Elided and apostrophe forms (`l'`, `aujourd'hui`), hyphenated words, numerals and punctuation are excluded. `NA` for a document without such tokens. Decomposed (NFD) accents fail the letter test; `utf8_normalize()` composes them (NFC). Not normalized; not auditable. |

## K. Lexical classes

These are surface inventories, not sense-disambiguated annotation. Matching
is case-insensitive and sentence-local on the parser's word rows. A match
contained in a longer match of another dictionary feature (f_04, f_05, f_11,
f_45–f_50) is dropped. After matching, three words are moved by position:
`donc` (f_45 → f_50), `ensuite` (f_05 → f_45), `vraiment` (f_49 → f_48).

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_45_conjuncts` | Conjuncts (linking adverbials) | Connecteurs | `cependant`, `toutefois`, `néanmoins`, `donc`, `ainsi`, `puis`, `de plus`, `par conséquent`, `par exemple`, `par ailleurs`, `à savoir`; plus `ensuite` after `,`/`;`. *J'aime le café. Cependant, je préfère le thé.* | `donc` in the first two or last two words of a sentence, or in a question, moves to f_50. `en revanche`, `pourtant`, `en effet` are not listed. `ainsi` counts in any use (*ainsi que*). |
| `f_46_downtoners` | Downtoners | Atténuateurs de degré | `à peine`, `presque`, `un peu`, `légèrement`, `guère`, `seulement`, `simplement`, `pratiquement`, `partiellement`. *Il est à peine capable de marcher.* | `guère` is a downtoner, not negation (D-41). `un peu` also matches quantifier *un peu de*. Removed from f_42. |
| `f_47_hedges` | Hedges | Enclosures (modalisateurs d'approximation) | `environ`, `peut-être`, `à peu près`, `plus ou moins`, `en quelque sorte`, `quelque peu`, `du genre`, `quelque chose comme`. *Il y avait environ cinquante personnes.* | `quelque chose comme` takes precedence over f_11 `quelque chose`. Removed from f_42. |
| `f_48_amplifiers` | Amplifiers | Intensifieurs | `très`, `absolument`, `complètement`, `totalement`, `parfaitement`, `fortement`, `tout à fait`, colloquial `super`, `hyper`, `ultra`, `grave`, `de ouf`, `fort`; plus `vraiment` next to an `ADJ`/`ADV`. *C'est très important.*; *C'est vraiment magnifique !* | No POS filter: adjectival *grave* (*C'est grave.* = 1) and *fort* count. Removed from f_42. |
| `f_49_emphatics` | Emphatics | Emphatiques | `vraiment`, `bien sûr`, `certes`, `effectivement`, `évidemment`, `beaucoup`, `tellement`, `sûr`, `tout à`. *Je l'ai vraiment fait hier.*; *Il a beaucoup travaillé pour réussir.* | `sûr` matches the adjective (*T'es sûr de toi ?*). `tout à` matches *tout à coup*, *tout à l'heure*; *tout à fait* is f_48 by precedence. Not removed from f_42. |
| `f_50_discourse_particles` | Discourse particles | Particules discursives | `alors`, `bien`, `bref`, `eh bien`, `du coup`, `en fait`, `de toute façon`, `voilà`, `euh`, `hein`, `ha`, `hé`, `ho`, `ô`; plus `donc` at a sentence edge or in a question. *Eh bien, je ne sais pas quoi dire.* | ⚠️ No position or POS filter: every `bien` (*c'est bien*, *bien que*) and `alors` (*alors que*) counts unless inside a longer match of another feature (*bien sûr* → f_49). `bon` is not listed (*Bon, on y va maintenant?* = 0). |
| `f_51_demonstratives` | Demonstrative determiners | Déterminants démonstratifs | Surface `ce`, `cet`, `cette`, `ces` in a `det` relation, tagged `DET` or `PRON`. *Ce livre est intéressant.*; *Cette maison est la mienne.* | Hard-coded; `dict$f_51_demonstratives` is not used. Pronouns are f_10. |

## L. Modals

D-40: modal features count modal verbs and grammaticalized periphrases only.
*Avoir la possibilité de*, *être obligé/nécessaire de*, *risquer de* and *être
susceptible de* are not counted.

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_52_modal_possibility` | Possibility modal | Modal de possibilité (*pouvoir*) | Every `VERB`/`AUX` with lemma `pouvoir`, any form, with or without an infinitive. *Je peux venir demain.*; *Il se pourrait que je vienne demain.* | No complement check (*Je n'en peux plus* counts). Conditional `pourrait` also counts in f_54. |
| `f_53_modal_necessity` | Necessity modals | Modaux de nécessité (*devoir*, *falloir*) | Every `VERB`/`AUX` with lemma `devoir` or `falloir`. *Tu dois finir tes devoirs.*; *Il faut partir maintenant.* | Epistemic *devoir* (*il doit être 5h*) and lexical 'owe' (*il me doit de l'argent*) also count. The `il` of *il faut* is f_09. |
| `f_54_modal_predictive` | Future, conditional and *aller* + infinitive | Futur, conditionnel et *aller* + infinitif | (a) Finite `VERB`/`AUX` with `Tense=Fut` or `Mood=Cnd`, one per word (the auxiliary in compound forms: *aurait aimé*); (b) finite `aller` in the present or imperfect with an infinitive `xcomp` after it, recorded as the span from `aller` to the infinitive. *Il sera là dans une heure.*; *Je vais aller au marché.*; *Elle partirait si elle le pouvait.* | Motion `aller` with a purpose infinitive is not separated. Future/conditional `pouvoir`/`devoir` also count in f_52/f_53. `va` also counts in f_03. `dict$f_54_modal_predictive` is empty. |

## M. Specialized verb classes

One occurrence per `VERB`/`AUX` whose lemma is a **single-word** entry of the
class dictionary. Multiword entries are never matched. No complement or mood
condition applies.

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_55_verb_public` | Public (speech-act) verbs | Verbes de communication publique | `affirmer`, `annoncer`, `déclarer`, `expliquer`, `répondre`, `promettre`, `reconnaître`, `signaler`, `écrire`, `parler`… *Il a affirmé que c'était vrai.*; *Elle a annoncé son départ.* | ⚠️ `dire` is not listed (*Il dit la vérité* = 0), although it governs f_60. `se_plaindre` never matches. |
| `f_56_verb_private` | Private (cognition) verbs | Verbes de cognition | `penser`, `croire`, `comprendre`, `espérer`, `supposer`, `trouver`, `sentir`, `apprendre`, `décider`, `oublier`… *Je pense qu'il a raison.*; *Elle croit en ses capacités.* | ⚠️ `savoir` is not listed. Polysemous verbs (`trouver`, `sentir`, `apprendre`) count in every sense. |
| `f_57_verb_suasive` | Suasive verbs | Verbes suasifs | `proposer`, `ordonner`, `demander`, `exiger`, `recommander`, `suggérer`, `conseiller`, `accepter`, `inviter`, `persuader`… *Je te propose de venir avec nous.*; *Il a ordonné que la porte soit fermée.* | `demander` counts in its question sense too (*je me demande si*). `donner_des_instructions` never matches. |
| `f_58_verb_seem` | Verbs of seeming | Verbes d'apparence | `sembler`, `paraître`, `apparaître`, any use. *Il semble fatigué aujourd'hui.*; *Elle paraît heureuse.* | ⚠️ `avoir l'air` is listed but never matched (*Il a l'air content* = 0). `apparaître` also covers 'come into view'. *Il semble que* also counts in f_09. |

## N. Reduced forms and dispreferred structures

D-30: these features count competing, less formal or marked realizations. The
mechanism differs by language; register is a rationale, not a guarantee for
each occurrence.

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_59_reduced_forms` (`f_59_contractions`) | Colloquial reductions | Formes réduites familières | One span per reduction (D-26, D-30, D-45): (a) longest match of `word_lists$colloquial_contractions` (`p'tit(e)(s)`, `m'sieur`, `m'dame`, `chuis`, `ch'suis`, `j'suis`, `chais`, `j'sais`, `y'a`); (b) subject `t'` before a finite singular verb with no subject before it (skipping `ne`), or joined `t'es`/`t'as`/`t'étais`/`t'avais`/`t'auras`/`t'seras`; (c) `j'` before a consonant other than `h` (*j'pense*); (d) `y` + `a`/`avait`/`aura`/`aurait` (or `y en a`) not after `il`. *T'es sûr de toi ?*; *Y a du monde ce soir.* | Obligatory elision (*j'ai*, *l'homme*, *c'est*, *qu'il*, *aujourd'hui*) is never counted (*J'ai vu l'homme qu'il aime.* = 0). Object/reflexive `te` is excluded (*je t'aime*, *tu t'es trompé*). *y a-t-il* is excluded. Dropped `ne` is not a reduction here (*c'est pas* is f_67). |
| `f_60_reduced_complement` (`f_60_that_deletion`) | Reduced complement clauses | Complétives réduites (sans *que*) | A `ccomp`/`xcomp` clause head of a `VERB` in `word_lists$reduced_complement_predicates` (`croire`, `penser`, `espérer`, `dire`, `affirmer`, `prétendre`), with no introducer (no `mark`/`case` dependent, `SCONJ`, `que` or wh-word), headed by a finite/infinitive `VERB`/`AUX` or by a word with such an `aux`/`cop`. Coordinated complements inherit. One per clause head (D-27, D-46). *Je crois avoir compris.*; *Je pense il viendra demain.*; *Il dit avoir vu le voleur.* | *Il m'a dit de partir*, *Je veux partir* and *On suppose ça a fonctionné* = 0 (introducer; other governors). f_15 words excluded. Overlap with f_24 is intended. `prétendre` is included, unlike Spanish `pretender`. |
| `f_61_stranded_resumptive` (`f_61_stranded_preposition`) | Orphan prepositions and resumptive relatives | Prépositions orphelines et relatives résomptives | For `que`/`qu'` directly after a `NOUN`/`PROPN`, scan forward to the next punctuation or `que`/`qui`/`dont`. Count the first of: (a) a word in `orphan_prepositions` (`avec`, `pour`, `sans`, `contre`, `dessus`, `dessous`, `dedans`) not followed by a complement; (b) a preposition + tonic `lui`/`elle`/`eux`/`elles` after the relative's first verb, matching `resumptive_prepositional_frames` (`parler_de`, `dépendre_de`, `compter_sur`, `fier_à`); (c) `PRON en` up to three words before a verb in `resumptive_en_verbs` (`parler`, `souvenir`, `rêver`, `occuper`, `dépendre`). One per relative (D-29, D-47). *C'est la fille que je sors avec.*; *C'est le film que je t'en ai parlé.* | A linear window, not a dependency check. The matched `que` leaves f_30. Pied-piping is f_33 only (*la personne avec qui…* = 0). |
| `f_62_split_infinitive` | Preposition–infinitive split by an adverb | Infinitif prépositionnel disjoint | A verbal infinitive (not f_15) preceded by `ADV` words (clitic pronouns may intervene) and then an `ADP` linked to it (head, or `mark`/`case`/`fixed` relation). At least one adverb must be neither negation (`ne`, `pas`, `plus`, `jamais`, `rien`, `point`, `guère`, `non`) nor in `preinfinitival_light_adverbs` (`bien`, `mal`, `mieux`, `trop`, `peu`, `assez`, `beaucoup`, `tant`, `tout`). Span from the preposition (D-28, D-48). *J'essaie de vraiment comprendre le problème.*; *Il est parti sans vraiment savoir pourquoi.* | *pour mieux comprendre*, *de ne pas venir*, *de ne jamais oublier* = 0. `toujours`, `encore`, `déjà` qualify. Bare infinitives after verbs are not covered. Independent of f_24/f_63. |
| `f_63_split_auxiliary` | Split auxiliary | Auxiliaire disjoint | `avoir`/`être`, or modal `pouvoir`/`devoir`, followed by one or more `ADV`/`PART` (clitics also allowed after a modal) and then its own participle (`avoir`/`être`) or infinitive (modal), linked by dependency. At least one adverb other than `ne`/`n'`/`pas`/`point`/`plus` (D-49). One per auxiliary. *Il a probablement été vu hier.*; *Il n'a jamais vu la mer.*; *Il peut facilement résoudre le problème.* | *Il n'a pas mangé* = 0. Intervening inverted subjects block a match. Other verbs (*veut vraiment partir*) are excluded. ⚠️ Stative *être* + adverb + participle counts (*Il est très fatigué* = 1). `tout` counts (*Il a tout mangé*). |

## O. Coordination

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_64_phrasal_coordination` | Phrasal coordination | Coordination de syntagmes | A `CCONJ` in `cc` whose head is a `conj` with the same UPOS (`NOUN`, `PROPN`, `ADJ`, `ADV`, `VERB`, `AUX`) as the first conjunct, when that second conjunct has no subject of its own. The coordinator is counted. *J'ai acheté des pommes et des oranges.*; *C'est grand et beau.* | Verbs sharing a subject count here (*Il lit et écrit.*). Mixed NOUN/PROPN pairs are not counted. Subject detection also uses subject-pronoun lemmas attached as `obj`/`iobj`. |
| `f_65_clausal_coordination` | Clausal coordination | Coordination de propositions | A `CCONJ` in `cc` whose head is a `conj` `VERB`/`AUX` with its own subject and not `Inf`/`Ger`/`Part`. *Il pleut et nous restons à l'intérieur.*; *Elle travaille et lui étudie.* | ⚠️ Sentence-initial *Et* (attached to the root) is not counted. Coordinated copular clauses (*Il est grand et elle est petite*) count in neither f_64 nor f_65. Second clauses in compound tenses are headed by a participle and are excluded. |

## P. Negation

| Feature | English label | French label | What is counted | Exclusions, overlaps and limitations |
|---|---|---|---|---|
| `f_66_neg_synthetic` | Synthetic negation | Négation synthétique (mots négatifs) | Each token whose lemma or form is in `word_lists$neg_synthetic_determiners` (`aucun(e)(s)`, `nul(le)(s)`, `personne`, `rien`, `jamais`, `ni`, `aucunement`, `nullement`), not tagged NOUN/PROPN/VERB/AUX, with no article. Concord counts each word (D-18, D-41). NOUN `personne` counts in a sentence with `ne`. *Aucune réponse n'est bonne.*; *Il parle jamais de ça.* | *une personne*, *un rien*, *si jamais*, *à jamais*, *plus que jamais* are excluded. `sans` is deferred; `guère` is f_46. Overlaps: f_11 (D-52), f_05 (`jamais`), f_64 (`ni`). |
| `f_67_neg_analytic` | Analytic negation | Négation analytique (particule) | `ADV`/`PART` `pas` (with or without `ne`); `point` or `plus` sharing a head with `ne`/`n'`; `non` in `advmod`. The particle is counted, not `ne` (D-41). *Je ne parle pas anglais.*; *Je suis pas sortie hier.*; *Il n'en veut plus.* | `ne` alone (*ne … que*, expletive `ne`) = 0. Comparative `plus` is excluded. A standalone reply *Non.* is excluded unless parsed `advmod`. `pas` also counts in f_42. |

---

## Known limitations (UDPipe French-GSD 2.5)

These reflect the parser model, not the extractor. They are documented, not
patched with surface-form lists.

- **Pronoun lemmas.** UDPipe lemmatizes `je`, `tu`, `nous` and `elle` as `il`
  (and possessive `mon`, `nos` as `son`). f_06–f_08 use morphology and
  surface forms, and f_09 matches the surface form. f_20 is lemma-based, so
  *Elle y a…* and *J'y ai…* are counted as *il y a*.
- **Curly apostrophes.** `’` breaks segmentation and tagging (invariant 5).
  Use `utf8::utf8_normalize(text, map_quote = TRUE)` before parsing (D-36).
  It also composes accents, which f_44 needs.
- **Interrogative `où` tagged `PronType=Rel`.** f_23 keeps `où`, and f_13 does
  not check `PronType`. Relative `où` is counted in no relative bin.
- **Conditional tagged `Tense=Pres|Mood=Cnd`.** f_03 filters on mood; f_54
  counts `Mood=Cnd`.
- **Gérondif tagged NOUN** (*en chantant* → NOUN `obl`). Recovered for f_25 by
  `gerondif_noun_rows_fr()` and removed from f_16.
- **Participles.** Participles tagged NOUN (*était partie*) are lost to f_02.
  Stative *être* + participle often gets `aux:pass` and inflates f_17 and f_63.
- **Possessive determiners** carry no `Person` feature, so morphology cannot
  recover `mon/ma/mes/nos/son/sa/ses`.
- **Imperative + clitic** (*Dis-moi*, *Explique-moi*) disturbs the parse of the
  following complement (f_23).
- **Multiword prepositions** are not always `fixed` (*à cause de*), which
  changes f_39 and f_16.
- **spaCy.** `spacyr_parsed` input (`fr_core_news_sm`) is accepted; spaCy
  morphology is read from the `morph` column. The observations above were
  checked on UDPipe only.

## Discrepancies noted while drafting

The code is described above. These sources disagree with it.

1. **`R/biber.R` roxygen, f_65** says "e.g., sentence-initial *et*". The code
   requires a `conj` predicate with its own subject; *Et il est parti.* gives 0.
2. **`R/biber.R` roxygen, f_32** lists `dont`. `dont` is f_33 only (code, D-24,
   NEWS).
3. **`R/biber.R` roxygen, f_50** gives `bon` as an example. `bon` is not in
   `dict$f_50_discourse_particles` (*Bon, on y va* = 0).
4. **`R/biber.R` roxygen, f_66/f_67** are outdated. f_66 is described as
   "determiners or adjectives (*aucun*, *nul*)"; the code counts all of
   `neg_synthetic_determiners`. f_67 is described as "*ne* … adverb (*ne … pas*,
   *ne … jamais*)"; the code counts `pas` without `ne`, puts `jamais` in f_66
   and never counts `ne` (D-41).
5. **f_38 documentation** (`block_clause_embedding_fr()`, `biber.R`) lists
   adverbial subordinators, but the code counts any unlisted `mark`, including
   infinitival `de`/`à`/`pour`. *Il est facile de convaincre Jean.* gives
   f_38 = 1, and `tests/testthat/fixtures/french_edge_case_features.csv`
   freezes such counts.
6. **f_20** roxygen and NEWS say "each *il y a*". The adjacency rule misses
   *il n'y a*, *il y en a*, and it counts *elle/j' + y + avoir* because of the
   UDPipe `il` lemma.
7. **Possessive coverage, f_06–f_08** is inconsistent. `notre` (f_06),
   `ton/ta/tes/votre/vos` (f_07) and `leur/leurs` (f_08) are in the
   dictionaries; `mon/ma/mes/nos` and `son/sa/ses` are not. Morphology cannot
   fill the gap (no `Person` on possessive `DET`).
8. **Multiword verb-class entries** never match, because
   `dictionary_to_lemmas()` returns single-word entries only:
   `se_plaindre` (f_55), `donner_des_instructions` (f_57), `avoir_l_'_air`
   (f_58).
9. **`data-raw/dict.yaml` comments** are outdated. The header lists
   double-counting overlaps (`devoir` in f_53 and f_54, `risquer` in f_52 and
   f_54) that no longer exist. Comments cite `count_ensuite_conjunct_fr()`,
   `count_donc_discourse_fr()` and `count_vraiment_amplifier_fr()`, which are
   replaced by `disambiguate_stance_fr()`. The f_54 comment lists *risquer de*
   and *être susceptible de*, which D-40 removed. `dict$f_51_demonstratives`
   lists pronouns (`ça`, `celui`…) and is unused by the code.
10. **`data-raw/french_examples.yaml` expectations** contradict the code or
    decisions: f_01 *Le chat a marché…* and *…il est parti le lendemain*
    expect 1 (D-33 gives 0); f_06 *Mon frère et moi…* expects 2 (gives 1);
    f_42 *Il travaille toujours avec soin.* expects 1 (D-15 gives 0); f_50
    *Bon, on y va maintenant?* expects 1 (gives 0). Parser misses also differ
    from expectations: f_02 *Elle était partie*, f_09 *Il est clair qu'il est
    censé* (2), f_13 *Quand partez-vous ?*, f_17 *La porte est restée fermée*
    (1), f_23 *Dis-moi…*/*Explique-moi…*, f_25 *En se remplissant…* (1), f_27
    *La maison construite…*. Only the first example per feature is tested.
11. **`block_adj_prep_adv_fr()` roxygen** says a multiword preposition counts
    once. That holds only when the parser uses `fixed` (*à cause de* gives 2).
12. **f_37** also accepts `au moins que` (the code tests for a preceding `à` or
    `au`). This is undocumented and probably unintended.
13. **`word_lists$impersonal_verbs`** contains `s'agir` and `y_avoir`, which can
    never equal a lemma, and `faire`, which the code removes. The effective
    weather list is `pleuvoir`, `neiger`, `geler`, `grêler`, `venter`,
    `bruiner`, `tonner`.
14. **`tests/testthat/test-french-examples.R`** gives the reason for the *Dis-moi
    ce que tu veux.* known miss as "free relatives deferred". D-37 has
    since activated them; the miss is a parse failure.

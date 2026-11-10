#' Feature -> Biber category mapping (internal)
#'
#' Named vector mapping each of the 67 output columns to its category, in
#' output order. Labels double as `audit_features()` category names.
#'
#' @keywords internal
#' @noRd
.feature_categories <- c(
  f_01_past_tense              = "A. Tense and aspect markers",
  f_02_perfect_aspect          = "A. Tense and aspect markers",
  f_03_present_tense           = "A. Tense and aspect markers",
  f_04_place_adverbials        = "B. Place and time adverbials",
  f_05_time_adverbials         = "B. Place and time adverbials",
  f_06_first_person_pronouns   = "C. Pronouns and pro-verbs",
  f_07_second_person_pronouns  = "C. Pronouns and pro-verbs",
  f_08_third_person_pronouns   = "C. Pronouns and pro-verbs",
  f_09_pronoun_it              = "C. Pronouns and pro-verbs",
  f_10_demonstrative_pronoun   = "C. Pronouns and pro-verbs",
  f_11_indefinite_pronouns     = "C. Pronouns and pro-verbs",
  f_12_proverb_do              = "C. Pronouns and pro-verbs",
  f_13_wh_question             = "D. Questions",
  f_14_nominalizations         = "E. Nominal forms",
  f_15_gerunds                 = "E. Nominal forms",
  f_16_other_nouns             = "E. Nominal forms",
  f_17_agentless_passives      = "F. Passives",
  f_18_by_passives             = "F. Passives",
  f_19_be_main_verb            = "G. Stative forms",
  f_20_existential_there       = "G. Stative forms",
  f_21_that_verb_comp          = "H. Subordination features",
  f_22_that_adj_comp           = "H. Subordination features",
  f_23_wh_clause               = "H. Subordination features",
  f_24_infinitives             = "H. Subordination features",
  f_25_present_participle      = "H. Subordination features",
  f_26_past_participle         = "H. Subordination features",
  f_27_past_participle_whiz    = "H. Subordination features",
  f_28_present_participle_whiz = "H. Subordination features",
  f_29_that_subj               = "H. Subordination features",
  f_30_that_obj                = "H. Subordination features",
  f_31_wh_subj                 = "H. Subordination features",
  f_32_wh_obj                  = "H. Subordination features",
  f_33_pied_piping             = "H. Subordination features",
  f_34_sentence_relatives      = "H. Subordination features",
  f_35_because                 = "H. Subordination features",
  f_36_though                  = "H. Subordination features",
  f_37_if                      = "H. Subordination features",
  f_38_other_adv_sub           = "H. Subordination features",
  f_39_prepositions            = "I. Prepositional phrases, adjectives and adverbs",
  f_40_adj_attr                = "I. Prepositional phrases, adjectives and adverbs",
  f_41_adj_pred                = "I. Prepositional phrases, adjectives and adverbs",
  f_42_adverbs                 = "I. Prepositional phrases, adjectives and adverbs",
  f_43_type_token              = "J. Lexical specificity",
  f_44_mean_word_length        = "J. Lexical specificity",
  f_45_conjuncts               = "K. Lexical classes",
  f_46_downtoners              = "K. Lexical classes",
  f_47_hedges                  = "K. Lexical classes",
  f_48_amplifiers              = "K. Lexical classes",
  f_49_emphatics               = "K. Lexical classes",
  f_50_discourse_particles     = "K. Lexical classes",
  f_51_demonstratives          = "K. Lexical classes",
  f_52_modal_possibility       = "L. Modals",
  f_53_modal_necessity         = "L. Modals",
  f_54_modal_predictive        = "L. Modals",
  f_55_verb_public             = "M. Specialized verb classes",
  f_56_verb_private            = "M. Specialized verb classes",
  f_57_verb_suasive            = "M. Specialized verb classes",
  f_58_verb_seem               = "M. Specialized verb classes",
  f_59_contractions            = "N. Reduced forms and dispreferred structures",
  f_60_that_deletion           = "N. Reduced forms and dispreferred structures",
  f_61_stranded_preposition    = "N. Reduced forms and dispreferred structures",
  f_62_split_infinitive        = "N. Reduced forms and dispreferred structures",
  f_63_split_auxiliary         = "N. Reduced forms and dispreferred structures",
  f_64_phrasal_coordination    = "O. Co-ordination",
  f_65_clausal_coordination    = "O. Co-ordination",
  f_66_neg_synthetic           = "P. Negation",
  f_67_neg_analytic            = "P. Negation"
)

#' Additional audit category aliases (internal)
#'
#' Accepted in addition to category letters and full labels.
#'
#' @keywords internal
#' @noRd
.category_aliases <- c(
  "subordination" = "H",
  "coordination" = "O",
  "co-ordination" = "O",
  "lexical classes" = "K",
  "modals" = "L",
  "negation" = "P",
  "passives" = "F",
  "questions" = "D",
  "nominal forms" = "E",
  "stative forms" = "G",
  "lexical specificity" = "J",
  "specialized verb classes" = "M"
)

#' Features always output as zero (internal)
#'
#' Every French feature currently has a detector.
#'
#' @keywords internal
#' @noRd
.zero_output_features <- character(0)

#' Document-level metrics (internal)
#'
#' Not occurrence counts; never normalized and not auditable.
#'
#' @keywords internal
#' @noRd
.metric_features <- c("f_43_type_token", "f_44_mean_word_length")

#' Features counted from evidence rows (internal)
#'
#' Each raw count equals the number of evidence rows for the feature.
#'
#' @keywords internal
#' @noRd
.evidence_features <- setdiff(names(.feature_categories),
                              c(.zero_output_features, .metric_features))

#' Surface-dictionary features (internal)
#'
#' Counted from maximal sentence-local dictionary phrase matches.
#'
#' @keywords internal
#' @noRd
.dict_only_features <- c(
  "f_04_place_adverbials", "f_05_time_adverbials", "f_11_indefinite_pronouns",
  "f_45_conjuncts", "f_46_downtoners", "f_47_hedges", "f_48_amplifiers",
  "f_49_emphatics", "f_50_discourse_particles"
)

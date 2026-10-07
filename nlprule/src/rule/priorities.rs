//! Rule priorities used by `CleanOverlappingFilter`
//! (`Language.getPriorityForId`), ported from the LT 6.5 language classes.
//! Only the languages whose class overrides the map are listed; any id not
//! found has priority 0.

fn en_static(id: &str) -> Option<i32> {
    Some(match id {
        "I_E" => 10,
        "CHILDISH_LANGUAGE" => 8,
        "RUDE_SARCASTIC" | "FOR_NOUN_SAKE" | "YEAR_OLD_HYPHEN" => 6,
        "MISSING_HYPHEN" | "WRONG_APOSTROPHE" => 5,
        "YOU_GOOD" | "DOS_AND_DONTS" | "IF_YOU_FURTHER_QUESTIONS" => 3,
        "ABBREVIATION_PUNCTUATION" | "READ_ONLY_ACCESS_HYPHEN"
        | "MAKE_OR_BREAK_HYPHEN" | "LINKED_IN" | "GOOD_FLUCK"
        | "PROFANITY_TYPOS" | "FEDEX" => 2,
        "T_HE" | "I_A_M" | "ACCESS_EXCESS" | "PRP_ABLE_TO" | "WEE_WE"
        | "CAN_MISSPELLING" | "FOR_THE_MOST_PART2" | "FACE_TO_FACE_HYPHEN"
        | "RUN_ON" | "ON_THE_LOOK_OUT" | "APOSTROPHE_IN_DAYS"
        | "SAFE_GUARD_COMPOUND" | "EVEN_HANDED_HYPHEN" | "GET_TOGETHER_HYPHEN"
        | "GOT_HERE" | "PICTURE_PERFECT_HYPHEN" | "SEEM_SEEN" | "SAVE_SAFE"
        | "DROP_DEAD_HYPHEN" | "HEAR_HERE" | "THE_FRENCH" | "A_HEADS_UP"
        | "UNITES_UNITED" | "THIS_MISSING_VERB" | "YOURE" | "LIFE_COMPOUNDS"
        | "DRIVE_THROUGH_HYPHEN" | "CAUSE_COURSE" | "THANK_YOUR" | "AN_AND"
        | "HER_S" | "ONE_TO_MANY_HYPHEN" | "COVID_19" | "RATHER_NOT_VB"
        | "PIECE_COMPOUNDS" | "OTHER_WISE_COMPOUND" | "ON_EXCEL" | "ALL_NN"
        | "SHOW_COMPOUNDS" | "PRP_AREA" | "IF_VB_PCT" | "CAUSE_BECAUSE"
        | "MAY_MANY" | "BOUT_TO" | "HAVE_HAVE" | "LUV" | "DAT" | "MAC_OS"
        | "BESTEST" | "OFF_OF" | "SHELL_COMPOUNDS" | "HANDS_ON_HYPHEN"
        | "PROFITS_WARNINGS" | "QUIET_QUITE" | "A_OK" | "I_A" | "PRP_NO_VB"
        | "GAVE_HAVE" | "THERE_FORE" | "FOLLOW_UP" | "IT_SOMETHING"
        | "NO_KNOW" | "WILL_BASED_ON" | "DON_T_AREN_T" | "WILL_BECOMING"
        | "WOULD_NEVER_VBN" | "MONEY_BACK_HYPHEN" | "WORLDS_BEST"
        | "STEP_COMPOUNDS" | "WON_T_TO" | "WAN_T" | "THE_US" | "THE_IT"
        | "THANK_YOU_MUCH" | "TO_DO_HYPHEN" | "A_NUMBER_NNS" | "A_HUNDREDS"
        | "NOW_A_DAYS" | "COUPLE_OF_TIMES" | "A_WINDOWS" | "A_SCISSOR"
        | "A_SNICKERS" | "A_NNS_BEST_NN" | "BACHELORS" | "WERE_WEAR"
        | "NEITHER_NOR" | "FOR_AWHILE" | "A_BUT" | "BORN_IN" | "DO_TO"
        | "CURIOS_CURIOUS" | "INCORRECT_POSSESSIVE_APOSTROPHE"
        | "THIS_YEARS_POSSESSIVE_APOSTROPHE" | "SPURIOUS_APOSTROPHE"
        | "BE_NOT_BE_JJ" | "IN_THIS_REGARDS" | "IT_SEAMS" | "NO_WHERE"
        | "APOSTROPHE_VS_QUOTE" | "ALL_OF_SUDDEN" | "COMMA_PERIOD"
        | "COMMA_CLOSING_PARENTHESIS" | "ELLIPSIS" | "HERE_HEAR"
        | "MISSING_POSS_APOS" | "DO_HE_VERB" | "LIGATURES" | "APPSTORE"
        | "INCORRECT_CONTRACTIONS" | "DONT_T" | "WHATS_APP"
        | "NON_STANDARD_COMMA" | "NON_ENGLISH_CHARACTER_IN_A_WORD"
        | "WONT_CONTRACTION" | "THAN_THANK" | "IT_IF" | "FINE_TUNE_COMPOUNDS"
        | "WHAT_IS_YOU" | "SUPPOSE_TO" | "CONFUSION_GONG_GOING" | "SEEN_SEEM"
        | "PROFANITY" | "PROFANITY_XML" | "THE_THEM" | "THERE_THEIR"
        | "TO_WORRIED_ABOUT" | "IT_IS_DEPENDING_ON" | "TO_NIGHT_TO_DAY"
        | "IRREGARDLESS" | "MD_APOSTROPHE_VB" | "ULTRA_HYPHEN"
        | "THINK_BELIEVE_THAT" | "HAS_TO_APPROVED_BY" | "WANNA"
        | "LOOK_FORWARD_TO" | "LOOK_SLIKE" | "A3FT" | "HYPHEN_TO_EN"
        | "ADVERB_WORD_ORDER_10_TEMP" => 1,
        "EVERY_NOW_AND_THEN" => 0,
        "MD_VBD" | "PRP_PRP" | "IS_LIKELY_TO_BE" | "EN_DIACRITICS_REPLACE_ORTHOGRAPHY"
        | "MISSING_COMMA_BETWEEN_DAY_AND_YEAR" | "FASTLY" | "WHO_NOUN"
        | "ANYWAYS" | "MISSING_GENITIVE" | "EN_UNPAIRED_BRACKETS"
        | "WAKED_UP" | "NEEDS_FIXED" | "SENT_START_NNP_COMMA" | "SENT_START_NN_DT"
        | "DT_PDT" | "MD_VB_AND_NOTVB" | "BLACK_SEA" | "A_TO" | "MANY_NN"
        | "WE_BE" | "A_LOT_OF_NN" | "ORDER_OF_WORDS_WITH_NOT"
        | "ADVERB_WORD_ORDER" | "HAVE_VB_DT" | "MD_PRP" | "IT_IS_2"
        | "A_RB_NN" | "DT_RB_IN" | "VERB_NOUN_CONFUSION" | "NOUN_VERB_CONFUSION"
        | "PLURAL_VERB_AFTER_THIS" | "BE_RB_BE" | "IT_ITS"
        | "ENGLISH_WORD_REPEAT_RULE" | "DT_JJ_NO_NOUN" | "AGREEMENT_SENT_START"
        | "PREPOSITION_VERB" | "EN_A_VS_AN" | "CD_NN" | "CD_NNU"
        | "ATD_VERBS_TO_COLLOCATION" | "ORDINAL_NUMBER_MISSING_ORDINAL_INDICATOR"
        | "ADVERB_OR_HYPHENATED_ADJECTIVE" | "GOING_TO_VBD"
        | "MISSING_PREPOSITION" | "CHARACTER_APOSTROPHE_WORD" | "SINGLE_CHARACTER"
        | "BE_TO_VBG" | "NON3PRS_VERB" | "DT_NN_VBG" | "NNS_THAT_ARE_JJ"
        | "DID_FOUND_AMBIGUOUS" | "BE_I_BE_GERUND" | "VBZ_VBD"
        | "SUPERLATIVE_THAN" | "UNLIKELY_OPENING_PUNCTUATION" | "MD_DT_JJ"
        | "I_IF" | "NOUNPHRASE_VB_RB_DT" | "SENT_START_NN_NN_VB"
        | "VB_A_JJ_NNS" | "DUPLICATION_OF_IS_VBZ" | "METRIC_UNITS_EN_IMPERIAL"
        | "IF_THEN_COMMA" | "COMMA_COMPOUND_SENTENCE" | "COMMA_COMPOUND_SENTENCE_2"
        | "BE_VBG_BE" | "PRP_VB_VB" | "FOR_ANY_CLARIFICATIONS"
        | "PLEASE_LET_ME_KNOW" | "UNNECESSARY_CAPITALIZATION"
        | "CONFUSION_OF_A_JJ_NNP_NNS_PRP" | "PLURALITY_CONFUSION_OF_NNS_OF_NN"
        | "NP_TO_IS" | "REPEATED_VERBS" => -1,
        "NNP_COMMA_QUESTION" | "THE_CC" | "PRP_VBG" | "CANT_JJ" | "WOULD_A"
        | "I_AM_VB" | "VBP_VBP" => -2,
        "GONNA_TEMP" | "A_INFINITIVE" | "INDIAN_ENGLISH" | "DO_PRP_NOTVB" => -3,
        "GONNA" | "WHATCHA" | "DONTCHA" | "GOTCHA" | "OUTTA" | "Y_ALL"
        | "GIMME" | "LEMME" | "ID_CASING" => -4,
        "POSSESSIVE_APOSTROPHE" => -10,
        "MD_PRP_QUESTION_MARK" => -11,
        "PRP_RB_NO_VB" | "MD_JJ" | "HE_VERB_AGR" | "MD_BASEFORM" | "IT_VBZ"
        | "PRP_THE" | "PRP_JJ" | "SINGULAR_NOUN_VERB_AGREEMENT"
        | "SINGULAR_AGREEMENT_SENT_START" | "VB_TO_NN_DT" | "SUBJECTVERBAGREEMENT_2"
        | "THE_SENT_END" | "DT_NN_ARE_AME"
        | "COLLECTIVE_NOUN_VERB_AGREEMENT_VBP" | "SUBJECT_VERB_AGREEMENT"
        | "VERB_APOSTROPHE_S" | "WHERE_MD_VB" | "SENT_START_PRPS_JJ_NN_VBP"
        | "TO_AFTER_MODAL_VERBS" | "SINGULAR_NOUN_ADV_AGREEMENT" | "BE_VBP_IN"
        | "BE_VBG_NN" | "THE_NNS_NN_IS" | "IF_DT_NN_VBZ" | "PRP_MD_NN" => -12,
        "HAVE_PART_AGREEMENT" | "BEEN_PART_AGREEMENT" => -13,
        "BE_WITH_WRONG_VERB_FORM" => -14,
        "TWO_CONNECTED_MODAL_VERBS" | "PRP_NO_ADVERB_VERB"
        | "MISSING_TO_BETWEEN_BE_AND_VB" | "IN_DT_IN" | "MISSING_SUBJECT"
        | "HAVE_TO_NOTVB" | "PLEASE_DO_NOT_THE_CAT" | "VB_TO_JJ" | "CC_PRP_ARTICLE" => -15,
        "BE_MD" => -20,
        "WANT_TO_NN" | "QUESTION_WITHOUT_VERB" | "PRP_VB" | "PRP_VB_NN" => -25,
        "BE_NN" | "BE_VB_OR_NN" | "DO_DT_NN_BE" | "PRONOUN_NOUN" => -26,
        "ETC_PERIOD" | "COULD_YOU_NOT_NEEDED" => -49,
        "SEEMS_TO_BE" => -51,
        "MD_NN" | "I_THINK_FEEL" | "KNOW_AWARE_REDO" => -60,
        "EN_REDUNDANCY_REPLACE" => -510,
        "EN_PLAIN_ENGLISH_REPLACE" => -511,
        "REP_PASSIVE_VOICE" | "FOUR_NN" => -599,
        "THREE_NN" | "SENT_START_NUM" | "PASSIVE_VOICE" | "EG_NO_COMMA"
        | "IE_NO_COMMA" | "REASON_WHY" => -600,
        "TOO_LONG_SENTENCE" => -997,
        "TOO_LONG_PARAGRAPH" => -998,
        "ALL_UPPERCASE" => -1000,
        _ => return None,
    })
}


fn fr_static(id: &str) -> Option<i32> {
    Some(match id {
        "ACCORD_EXCEPTIONS" | "EXPRESSIONS_VU" | "SA_CA_SE" | "SIL_VOUS_PLAIT"
        | "QUASI_NOM" | "MA" | "SON_SONT" | "JE_TES" | "A_INFINITIF"
        | "ON_ONT" | "LEURS_LEUR" | "DU_DU" | "ACCORD_CHAQUE" | "J_N2"
        | "CEST_A_DIRE" | "FAIRE_VPPA" | "D_N_E_OU_E" | "GENS_ACCORD"
        | "VIRGULE_EXPRESSIONS_FIGEES" | "TRAIT_UNION" | "PLURIEL_AL2"
        | "FR_SPLIT_WORDS_HYPHEN" => 100,
        "PAS_DE_TRAIT_UNION" => 50,
        "A_VERBE_INFINITIF" | "DE_OU_DES" | "EMPLOI_EMPLOIE" | "VOIR_VOIRE"
        | "D_VPPA" | "EST_CE_QUE" => 20,
        "CONFUSION_PARLEZ_PARLER" | "ACCORD_TOUT_LE" | "ESPACE_UNITES"
        | "BYTES" | "Y_A" | "COTE" | "PEUTETRE" | "A_A_ACCENT"
        | "A_ACCENT_A" | "A_A_ACCENT2" | "A_ACCENT" | "JE_M_APPEL"
        | "ACCORD_R_PERS_VERBE" | "JE_SUI" | "R_VAVOIR_VINF" | "AN_EN"
        | "APOS_M" | "ACCORD_PLURIEL_ORDINAUX" | "SUJET_AUXILIAIRE"
        | "ADJ_ADJ_SENT_END" | "OU_PAS" | "PLACE_DE_LA_VIRGULE"
        | "PAS_DE_SOUCIS" => 10,
        "SE_CE" | "J_N" | "TE_NV2" | "INTERROGATIVE_DIRECTE" | "V_J_A_R"
        | "TRES_TRES_ADJ" | "IMP_PRON" => -10,
        "TOO_LONG_PARAGRAPH" => -15,
        "TE_NV" | "PREP_VERBECONJUGUE" | "LA_LA2" | "FRENCH_WORD_REPEAT_RULE"
        | "PAS_DE_VERBE_APRES_POSSESSIF_DEMONSTRATIF" | "VIRGULE_VERBE" => -20,
        "VERBES_FAMILIERS" => -25,
        "VERB_PRONOUN" | "IL_VERBE" | "A_LE" | "ILS_VERBE"
        | "AGREEMENT_POSTPONED_ADJ" | "MULTI_ADJ" | "PARENTHESES"
        | "REP_ESSENTIEL" | "CONFUSION_AL_LA" => -50,
        "LE_COVID" => -60,
        "FR_SPELLING_RULE" | "VIRG_INF" => -100,
        "ET_SENT_START" | "MAIS_SENT_START" => -151,
        "EN_CE_QUI_CONCERNE" | "EN_MEME_TEMPS" | "ET_AUSSI" | "MAIS_AUSSI" => -152,
        "ELISION" | "POINT" => -200,
        "REPETITIONS_STYLE" | "POINTS_SUSPENSIONS_SPACE" => -250,
        "UPPERCASE_SENTENCE_START" => -300,
        "FRENCH_WHITESPACE_STRICT" | "FRENCH_WORD_REPEAT_BEGINNING_RULE" => -350,
        "TOUT_MAJUSCULES" | "VIRG_NON_TROUVEE" | "POINTS_2" | "MOTS_INCOMP"
        | "FRENCH_WHITESPACE" | "MOT_TRAIT_MOT" => -400,
        _ => return None,
    })
}

fn es_static(id: &str) -> Option<i32> {
    Some(match id {
        "ES_SIMPLE_REPLACE_MULTIWORDS" | "LOS_MAPUCHE" | "TE_TILDE" | "DE_TILDE"
        | "PLURAL_SEPARADO" | "PERSONAJES_FAMOSOS" => 50,
        "NO_SEPARADO" | "PARTICIPIO_MS" | "VERBO_MODAL_INFINITIVO" | "EL_NO_TILDE" => 40,
        "SE_CREO" => 35,
        "DEGREE_CHAR" | "LO_LOS" | "ETCETERA" | "P_EJ" | "AGREEMENT_ADJ_NOUN_AREA" => 30,
        "SE_CREO2" => 25,
        "PRONOMBRE_SIN_VERBO" | "AGREEMENT_DET_ABREV" | "MUCHO_NF"
        | "AGREEMENT_DET_NOUN_EXCEPTIONS" => 25,
        "TYPOGRAPHY" | "PRIMER_PRIMERA" | "CONFUSION_ES_SE" => 20,
        "AGREEMENT_DET_NOUN" => 15,
        "HALLA_HAYA" | "VALLA_VAYA" | "SI_AFIRMACION" | "TE_TILDE2"
        | "AGREEMENT_DET_ADJ" => 10,
        "SEPARADO" => 1,
        "ES_SPLIT_WORDS" | "U_NO" | "EL_TILDE" => -10,
        "SINGLE_CHARACTER" | "TOO_LONG_PARAGRAPH" => -15,
        "PREP_VERB" => -20,
        "SUBJUNTIVO_FUTURO" | "SUBJUNTIVO_PASADO" | "SUBJUNTIVO_PASADO2"
        | "AGREEMENT_ADJ_NOUN" | "AGREEMENT_PARTICLE_NOUN"
        | "AGREEMENT_POSTPONED_ADJ" | "MULTI_ADJ" => -30,
        "SUBJUNTIVO_INCORRECTO" | "COMMA_SINO" | "COMMA_SINO2" | "VOSEO" => -40,
        "REPETITIONS_STYLE" => -50,
        "MORFOLOGIK_RULE_ES" => -100,
        "PHRASE_REPETITION" | "SPANISH_WORD_REPEAT_RULE" => -150,
        "UPPERCASE_SENTENCE_START" => -200,
        "ES_QUESTION_MARK" => -250,
        _ => return None,
    })
}


fn pt_static(id: &str) -> Option<i32> {
    Some(match id {
        "FRAGMENT_TWO_ARTICLES" => 50,
        "DEGREE_MINUTES_SECONDS" => 30,
        "INTERJECTIONS_PUNTUATION" => 20,
        "CONFUSION_POR_PÔR_V2" | "PARONYM_POLITICA_523" | "PARONYM_PRONUNCIA_262"
        | "PARONYM_CRITICA_397" | "PARONYM_INICIO_169" | "LP_PARONYMS"
        | "PARONYM_MUSICO_499_bis" | "NA_NÃO" | "VERB_COMMA_CONJUNCTION" => 10,
        "HOMOPHONE_AS_CARD" => 5,
        "TODOS_FOLLOWED_BY_NOUN_PLURAL" => 3,
        "TODOS_FOLLOWED_BY_NOUN_SINGULAR" => 2,
        "AUSENCIA_VIRGULA" | "EMAIL" => 1,
        "UNPAIRED_BRACKETS" => -5,
        "PROFANITY" => -6,
        "PT_BARBARISMS_REPLACE" | "BARBARISMS_PT_PT_V3" => -10,
        "PT_PT_SIMPLE_REPLACE" => -11,
        "PT_REDUNDANCY_REPLACE" => -12,
        "PT_WORDINESS_REPLACE" => -13,
        "PT_CLICHE_REPLACE" => -17,
        "INTERNET_ABBREVIATIONS" => -24,
        "CHILDISH_LANGUAGE" => -25,
        "ARCHAISMS" => -26,
        "INFORMALITIES" => -27,
        "BIASED_OPINION_WORDS" => -31,
        "PT_AGREEMENT_REPLACE" => -35,
        "CONTA_TO" => -44,
        "PT_DIACRITICS_REPLACE" | "DIACRITICS" | "PT_COMPOUNDS_POST_REFORM"
        | "AUX_VERBO" | "ENSINO_A_DISTANCIA" | "OQ_O_QUE_ORTHOGRAPHY"
        | "PT_ENGLISH_CONTRACTION_ORTHOGRAPHY" | "EMAIL_SEM_HIFEN" => -45,
        "PRETERITO_PERFEITO" | "PT_BR_SIMPLE_REPLACE" => -51,
        "CRASE_CONFUSION" | "NAO_MILITARES" | "NA_QUELE" | "NOTAS_FICAIS" => -54,
        "GENERAL_VERB_AGREEMENT_ERRORS" => -55,
        "GENERAL_NUMBER_AGREEMENT_ERRORS"
        | "GENERAL_GENDER_NUMBER_AGREEMENT_ERRORS" => -56,
        "FINAL_STOPS" => -75,
        "FAZER_USO_DE-USAR-RECORRER" => -90,
        "FORMAL_T_V_DISTINCTION" => -100,
        "FORMAL_T_V_DISTINCTION_ALL" => -101,
        "REPEATED_WORDS" => -210,
        "PT_WIKIPEDIA_COMMON_ERRORS" => -500,
        "FILLER_WORDS_PT" => -990,
        "TOO_LONG_SENTENCE" => -997,
        "TOO_LONG_PARAGRAPH" => -998,
        "READABILITY_RULE_SIMPLE_PT" => -1100,
        "READABILITY_RULE_DIFFICULT_PT" => -1101,
        "UNKNOWN_WORD" => -2000,
        _ => return None,
    })
}


fn de_static(id: &str) -> Option<i32> {
    Some(match id {
        "DE_PROHIBITED_PHRASE" => 11,
        "WRONG_SPELLING_PREMIUM_INTERNAL" | "OLD_SPELLING_RULE" | "DE_COMPOUNDS"
        | "E_MAIL_SIGNATUR" | "TELEFON_NR" | "IRGEND_COMPOUND" => 10,
        "DA_DURCH" | "BEI_GOOGLE" | "EINE_ORIGINAL_RECHNUNG_TEST"
        | "VON_SEITEN_RECOMMENDATION" | "AUFFORDERUNG_SIE" | "WEIS_ICH"
        | "VONSTATTEN_GEHEN" => 2,
        "VERWANDET_VERWANDTE" => 1,
        "DA_VOR" | "DAS_WUENSCHE_ICH" | "KLEINSCHREIBUNG_MAL"
        | "VERINF_DAS_DASS_SUB" | "IM_ALTER" | "DAS_ALTER"
        | "VER_INF_PKT_VER_INF" | "DASS_MIT_VERB" | "AB_TEST" | "BZGL_ABK"
        | "DURCH_WACHSEN" | "RUNDUM_SORGLOS_PAKET"
        | "MIT_FREUNDLICHEN_GRUESSE" | "OK" | "EINE_ORIGINAL_RECHNUNG"
        | "WAEHRUNGSANGABEN_CHF" => 1,
        "FALSCHES_ANFUEHRUNGSZEICHEN" | "VER_KOMMA_PRO_RIN"
        | "VER_INF_VER_INF" | "DE_COMPOUND_COHERENCY" | "GEFEATURED"
        | "NUMBER_SUB" | "MFG" | "VER123_VERAUXMOD" | "DE_AGREEMENT"
        | "DE_AGREEMENT2" | "KOMMA_NEBEN_UND_HAUPTSATZ"
        | "FALSCHES_RELATIVPRONOMEN" | "AKZENT_STATT_APOSTROPH"
        | "BEENDE_IST_SENTEND" | "VER_ADJ_ZU_SCHLAFEN" | "MIO_PUNKT"
        | "AUSLASSUNGSPUNKTE_LEERZEICHEN"
        | "IM_ERSCHEINUNG_SPELLING_RULE" | "SPACE_BEFORE_OG"
        | "VERSEHENTLICHERWEISE" | "VERMOD_SKIP_VER_PKT" | "N_NETTER_TYP" => -1,
        "EINZELBUCHSTABE_PREMIUM" | "ART_IND_ADJ_SUB" | "KATARI"
        | "SCHOENE_WETTER" | "MEIN_KLEIN_HAUS" | "UNPAIRED_BRACKETS"
        | "DE_UNPAIRED_QUOTES" | "ICH_GLAUBE_FUER_EUCH"
        | "OBJECT_AGREEMENT" | "ICH_INF_PREMIUM" | "MEHRERE_WOCHE_PREMIUM"
        | "DOPPELTER_NOMINATIV" | "KUDAMM"
        | "ALTERNATIVEN_FUER_ANGLIZISMEN" | "DOPPELUNG_VER_MOD_AUX"
        | "ANGLIZISMEN" | "ANGLIZISMUS_PA_MIT_ED" | "MEINSTE" | "ICH_LIEBS"
        | "WENNS_UND_ABERS" | "ABERS_SATZANFANG_SPELLING_RULE" | "VERNEB"
        | "ZAHL_IM_WORT_SPELLING_RULE" => -2,
        "GERMAN_SPELLER_RULE" | "AUSTRIAN_GERMAN_SPELLER_RULE"
        | "SWISS_GERMAN_SPELLER_RULE" => -3,
        "DE_VERBAGREEMENT" | "PUNKT_ENDE_DIREKTE_REDE"
        | "LEERZEICHEN_NACH_VOR_ANFUEHRUNGSZEICHEN"
        | "ZEICHENSETZUNG_DIREKTE_REDE" | "GROSSSCHREIBUNG_WOERTLICHER_REDE"
        | "IM_IHM_SPELLING_RULE" | "IN_UNKNOWNKLEIN_VER"
        | "SEHR_GEEHRTER_NAME" | "DE_PHRASE_REPETITION"
        | "FRAGEZEICHEN_NACH_DIREKTER_REDE" | "PUNCTUATION_PARAGRAPH_END"
        | "F_ANSTATT_PH_2" => -4,
        "DAS_WETTER_IST" | "VEREIZ_VERINF_PKT" | "WER_STARK_SCHWITZ"
        | "VERBEN_PRAEFIX_AUS" | "ANFUEHRUNG_VERSCHACHTELT"
        | "SATZBAU_AN_DEN_KOMMT" | "SUBJECT_VERB_AGREEMENT" => -5,
        "SAGT_SAGT" => -9,
        "VON_LEBENSLAEUFE_SPELLING_RULE" | "VER_WER_VER_3" | "PA_WAS"
        | "ICH_GEHE_DU_BLEIBST" | "PROPERNOMSIN_VERIMPSIN"
        | "DIESE_HABE_ER_BELEIDIGTE" | "VER123_VERAUXMOD_TEST1"
        | "ZUSAMMENGESETZTE_VERBEN" => -12,
        "PRP_VER_PRGK" | "COMMA_IN_FRONT_RELATIVE_CLAUSE" | "SAGT_RUFT"
        | "KANNST_WERDEN" => -13,
        "KOMMA_ZWISCHEN_HAUPT_UND_NEBENSATZ_2" | "MAN_SIEHT_SEHR_SCHOEN"
        | "BEI_VERB" | "MODALVERB_FLEKT_VERB" | "DATIV_NACH_PRP"
        | "DAT_ODER_AKK_NACH_PRP" | "SENT_START_SIN_PLU"
        | "SENT_START_PLU_SIN" | "VER_INFNOMEN" => -14,
        "GERMAN_WORD_REPEAT_RULE" | "TOO_LONG_PARAGRAPH" | "ALL_UPPERCASE" => -15,
        "NUR_LEDIGLICH" => -16,
        "COMMA_BEHIND_RELATIVE_CLAUSE" | "DOPPELUNG_MODALVERB"
        | "VER_DOPPELUNG" | "DEF_ARTIKEL_INDEF_ADJ" | "PRP_ADJ_AGREEMENT"
        | "SIE_WOLLTEN_SIND" | "ART_ADJ_SOL" | "WURDEN_WORDEN_1"
        | "WAR_WAHR" => -52,
        "KOMMA_ZWISCHEN_HAUPT_UND_NEBENSATZ" | "VERB_IST" | "WAR_WERDEN"
        | "INF_VER_MOD_SPELLING_RULE" | "DOPPELTES_VERB" => -53,
        "VERB_FEM_SUBST" | "SUBJUNKTION_KOMMA_2" => -54,
        "DOPPELUNG_GLEICHES_VERB" => -55,
        "FEHLENDES_NOMEN" | "REPETITIONS_STYLE" => -60,
        "GERMAN_WORD_REPEAT_BEGINNING_RULE" => -61,
        _ => return None,
    })
}

/// Port of `Language.getPriorityForId` for the languages we build.
pub(crate) fn priority_for_id(lang: Option<&str>, id: &str) -> i32 {
    let lang = match lang {
        Some(l) => l,
        None => return 0,
    };
    // suggestion sources are "CATEGORY/RULE_ID[/match]" (built-ins are
    // bare): normalize to the rule id so language priority maps match
    let id = id.split('/').nth(1).unwrap_or(id);
    match lang {
        "en" => {
            if id.starts_with("CONFUSION_RULE") {
                return -20;
            }
            if id.starts_with("MORFOLOGIK_RULE_EN") {
                return -10;
            }
            en_static(id).unwrap_or(0)
        }
        "fr" => {
            if id.starts_with("AI_FR_HYDRA_LEO")
                || id == "AI_FR_GGEC_REPLACEMENT_ORTHOGRAPHY"
            {
                return -101;
            }
            if id.starts_with("FR_COMPOUNDS") {
                return 500;
            }
            if id.starts_with("FR_MULTITOKEN_SPELLING") {
                return -90;
            }
            if id.starts_with("FR_SIMPLE_REPLACE") {
                return 150;
            }
            if id.starts_with("grammalecte_") {
                return -150;
            }
            match id {
                "SON" => -5,
                "CAT_TYPOGRAPHIE" | "CAT_TOURS_CRITIQUES"
                | "CAT_HOMONYMES_PARONYMES" => 20,
                "CAR" => -50,
                _ => fr_static(id).unwrap_or(0),
            }
        }
        "es" => es_static(id).unwrap_or(0),
        "pl" => {
            // Polish.getPriorityForId: ZDANIA_ZLOZONE must not override more
            // important rules (e.g. UPPERCASE_SENTENCE_START at sentence start)
            // (suggestion sources are "CATEGORY/RULE_ID[/match]", builtins bare)
            let rule_id = id.split('/').nth(1).unwrap_or(id);
            if rule_id == "ZDANIA_ZLOZONE" {
                return -1;
            }
            0
        }
        "ca" => {
            // Catalan.getPriorityForId (110 entries)
            if matches!(id, "CONFUSIONS2" | "DEU_NI_DO"
            ) {
                return 80;
            }
            if matches!(id, "FER_LOGIN" | "L_OK"
            ) {
                return 70;
            }
            if matches!(id, "INCORRECT_EXPRESSIONS" | "PERSONATGES_FAMOSOS" | "CONEIXO_CONEC" | "COMETES_INCORRECTES" | "OFERTAR_OFERIR"
            ) {
                return 50;
            }
            if matches!(id, "DESDE_UN" | "MOTS_NO_SEPARATS" | "REPETEAD_ELEMENTS" | "ESPERANT_US_AGRADI" | "LO_NEUTRE" | "ESPAIS_SOBRANTS"
            ) {
                return 40;
            }
            if matches!(id, "ELA_GEMINADA" | "CONFUSIONS_PRONOMS_FEBLES" | "PRONOMS_FEBLES_TEMPS_VERBAL"
            ) {
                return 35;
            }
            if matches!(id, "CA_SPLIT_WORDS" | "ET_AL" | "PRONOMS_FEBLES_COLLOQUIALS" | "CONCORDANCES_CASOS_PARTICULARS" | "GERUNDI_PERD_T" | "CONFUSIONS"
                | "PRONOMS_FEBLES_DARRERE_VERB" | "VERBS_NO_INCOATIUS" | "PUNT_LLETRA"
            ) {
                return 30;
            }
            if matches!(id, "REEMPRENDRE" | "INCORRECT_WORDS_IN_CONTEXT"
            ) {
                return 28;
            }
            if id == "PRONOMS_FEBLES_SOLTS2" {
                return 26;
            }
            if matches!(id, "ES_UNKNOWN" | "HAVER_SENSE_HAC" | "HA_A" | "PASSAT_PERIFRASTIC"
            ) {
                return 25;
            }
            if matches!(id, "CONFUSIONS_ACCENT" | "CONFUSIO_PASSAT_INFINITIU" | "DIACRITICS" | "CAP_GENS" | "MOTS_SENSE_GUIONETS" | "ORDINALS"
                | "SUPER" | "PRONOM_FEBLE_HI"
            ) {
                return 20;
            }
            if id == "HAVER_PARTICIPI_HAVER_IMPERSONAL" {
                return 15;
            }
            if matches!(id, "CONCORDANCES_NUMERALS_DUES" | "FALTA_CONDICIONAL" | "ACCENTUATION_CHECK" | "CONCORDANCA_GRIS" | "A_PER" | "CONCORDANCES_NUMERALS"
                | "COMMA_IJ" | "AVIS" | "CAP_ELS_CAP_ALS" | "CASING" | "DOS_ARTICLES" | "MOTS_GUIONET"
                | "SELS_EN_VA" | "CONCORDANCES_NOUNS_PRIORITY" | "PREFIXOS_SENSE_GUIONET_EN_DICCIONARI" | "ZERO_O" | "URL"
            ) {
                return 10;
            }
            if matches!(id, "CONCORDANCES_DET_NOM" | "CONCORDANCES_DET_ADJ" | "CONCORDANCES_DET_POSSESSIU" | "PASSAR_SE" | "DET_GN" | "SPELLING"
                | "APOSTROF_ANYS" | "VENIR_NO_REFLEXIU" | "DEUS_SEUS" | "SON_BONIC" | "ACCENTUACIO" | "L_NO_APOSTROFA"
                | "L_D_N_NO_S_APOSTROFEN"
            ) {
                return 5;
            }
            if id == "CONTRACCIONS" {
                return 0;
            }
            if id == "CASING_START" {
                return -5;
            }
            if matches!(id, "CA_WORD_COHERENCY" | "CA_WORD_COHERENCY_VALENCIA" | "ARTICLE_TOPONIM_MIN" | "PEL_QUE" | "COMMA_LOCUTION" | "REGIONAL_VERBS"
                | "PRONOMS_FEBLES_SOLTS" | "CONCORDANCA_PRONOMS_CATCHALL"
            ) {
                return -10;
            }
            if id == "AGREEMENT_POSTPONED_ADJ" {
                return -15;
            }
            if matches!(id, "FALTA_COMA_FRASE_CONDICIONAL" | "ESPAIS_QUE_FALTEN_PUNTUACIO" | "VERBS_NOMSPROPIS"
            ) {
                return -20;
            }
            if id == "VERBS_PRONOMINALS" {
                return -25;
            }
            if id == "PORTO_LLEGINT" {
                return -30;
            }
            if id == "PORTA_UNA_HORA" {
                return -40;
            }
            if matches!(id, "REPETITIONS_STYLE" | "MUNDAR"
            ) {
                return -50;
            }
            if id == "NOMBRES_ROMANS" {
                return -90;
            }
            if matches!(id, "TASCAS_TASQUES" | "PREPOSICIONS_MINUSCULA" | "SUGGERIMENTS_LE"
            ) {
                return -97;
            }
            if id == "MORFOLOGIK_RULE_CA_ES" {
                return -100;
            }
            if matches!(id, "EXIGEIX_ACCENTUACIO_VALENCIANA" | "APOSTROFACIO_MOT_DESCONEGUT"
            ) {
                return -120;
            }
            if matches!(id, "PHRASE_REPETITION" | "SUBSTANTIUS_JUNTS"
            ) {
                return -150;
            }
            if id == "REPETITION_ADJ_N_ADJ" {
                return -155;
            }
            if matches!(id, "FALTA_ELEMENT_ENTRE_VERBS" | "PUNT_FINAL" | "PUNCTUATION_PARAGRAPH_END"
            ) {
                return -200;
            }
            if matches!(id, "CA_END_PARAGRAPH_PUNCTUATION" | "DICENDI_QUE"
            ) {
                return -250;
            }
            if matches!(id, "UPPERCASE_SENTENCE_START" | "MAJUSCULA_IMPROBABLE" | "ELA_GEMINADA_WIKI"
            ) {
                return -500;
            }
            0
        }
        "be" => {
            // Belarusian.getPriorityForId
            match id {
                "RUSSIAN_SIMPLE_REPLACE_RULE" => 10,
                "BELARUSIAN_SPECIFIC_CASE" => 9,
                "Word_root_repeat" => -1,
                "PUNCT_DPT_2" => -2,
                "TOO_LONG_PARAGRAPH" => -15,
                _ => 0,
            }
        }
        "pt" => {
            if id.starts_with("MORFOLOGIK_RULE") {
                return -50;
            }
            if id.starts_with("PT_SIMPLE_REPLACE_ORTHOGRAPHY") {
                return -49;
            }
            if id.starts_with("AI_PT_GGEC_REPLACEMENT_ORTHOGRAPHY_SPELL")
                || id.starts_with("PT_MULTITOKEN_SPELLING")
            {
                return -48;
            }
            if id.starts_with("AI_PT_GGEC_REPLACEMENT_OTHER") {
                return -4;
            }
            if id.starts_with("ACENTUAÇÃO_VOGAL_ÊNCLISE")
                || id.starts_with("AI_PT_HYDRA_LEO")
            {
                return -51;
            }
            if id.starts_with("COLOCACAO_PRONOMINAL_COM_ATRATOR") {
                return -52;
            }
            pt_static(id).unwrap_or(0)
        }
        "de" => de_static(id).unwrap_or(0),
        "nl" => {
            if id.starts_with("NL_SIMPLE_REPLACE") {
                1
            } else {
                0
            }
        }
        _ => 0,
    }
}

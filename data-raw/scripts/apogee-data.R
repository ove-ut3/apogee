source("data-raw/scripts/utils.R")

#### Inscrits ####

inscrits <- impexp::csv_import_path("data-raw", pattern = "Inscrits\\.csv$", skip = 1, zip = TRUE, encoding = "UTF-8") %>% 
  dplyr::transmute(import = purrr::map(import, patchr::rename, impexp::access_import("_rename", access_base_path)),
                   import = purrr::map(import, patchr::transcode, impexp::access_import("_contents", access_base_path))) %>%
  tidyr::unnest() %>% 
  doublon_maj_etudiant() %>% 
  patchr::recode_formula(impexp::access_import("_recodage", access_base_path) %>% 
                           patchr::filter_data_patch(source = "data_inscrits")) %>% 
  dplyr::mutate(inscription_annulee = inscription_en_cours == "N" | !is.na(date_annulation) | inscription_resiliee == "O" | !is.na(date_resiliation),
                inscription_annulee = ifelse(is.na(inscription_annulee), FALSE, inscription_annulee))

inscrits_annules <- dplyr::filter(inscrits, inscription_annulee) %>% 
  dplyr::select(-inscription_annulee) %>% 
  nest_inscrits(code_composante) %>%
  nest_inscrits(code_version_etape)

usethis::use_data(inscrits_annules, overwrite = TRUE)

inscrits <- inscrits %>% 
  dplyr::filter(!inscription_annulee) %>% 
  dplyr::select(-inscription_annulee) %>% 
  nest_inscrits(code_composante) %>%
  nest_inscrits(code_version_etape)

# Ajout ELP parcours
elp_parcours <- apogee::inscrits_elp %>% 
  dplyr::inner_join(apogee::etape_histo %>% 
                      tidyr::drop_na(code_elp) %>% 
                      dplyr::mutate(elp_parcours = code_elp) %>% 
                      tidyr::separate_rows(code_elp, sep = ";"),
                    by = c("code_etape", "code_elp")) %>% 
  dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, elp_parcours) %>% 
  unique() %>% 
  dplyr::anti_join(patchr::duplicate(., annee, code_etape, code_etudiant, inscription_premiere),
                   by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

# Vérifier la présence de double parcours
doublons <- patchr::duplicate(elp_parcours, annee, code_etape, code_etudiant, inscription_premiere)
if (nrow(doublons) != 0) {
  message("Présence de doublons dans les parcours ELP rattachés aux inscriptions => concaténation des doubles parcours...")
  
  elp_parcours <- elp_parcours %>% 
    dplyr::group_by(annee, code_etape, code_etudiant, inscription_premiere) %>% 
    dplyr::summarise(elp_parcours = paste(elp_parcours, collapse = " ; ")) %>% 
    dplyr::ungroup()
} 

inscrits <- inscrits %>% 
  dplyr::left_join(elp_parcours, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

# Ajout inscrits Base Access

inscrits <- impexp::access_import("inscrits_ajout", "data-raw/data/Tables_ref_individus.accdb") %>% 
  dplyr::select(-commentaire, -date_maj) %>% 
  dplyr::anti_join(inscrits, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
  nest_inscrits(code_composante) %>% 
  nest_inscrits(code_version_etape) %>% 
  dplyr::bind_rows(inscrits) %>% 
  dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere)

# CPGE

inscrits_cpge <- inscrits %>% 
  dplyr::left_join(apogee::etape %>% 
                     dplyr::select(code_etape, particularite),
                   by = "code_etape") %>% 
  dplyr::filter(code_profil_etudiant == "CP" | particularite == "CPGE") %>% 
  dplyr::select(-particularite)

usethis::use_data(inscrits_cpge, overwrite = TRUE)

inscrits <- inscrits %>% 
  dplyr::anti_join(inscrits_cpge, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

#Sauvegarde finale

usethis::use_data(inscrits, overwrite = TRUE)

#### Inscrits péda ####

inscrits_peda <- impexp::csv_import_path("data-raw", pattern = "Inscrits_peda\\.csv$", skip = 1, zip = TRUE, encoding = "UTF-8") %>% 
  dplyr::transmute(import = purrr::map(import, patchr::rename, impexp::access_import("_rename", access_base_path)),
                   import = purrr::map(import, patchr::transcode, impexp::access_import("_contents", access_base_path))) %>% 
  tidyr::unnest() %>% 
  doublon_maj_etudiant()

usethis::use_data(inscrits_peda, overwrite = TRUE)

#### Inscrits ELP ####

inscrits_elp <- impexp::csv_import_path("data-raw", pattern = "Inscrits_ELP.*?\\.csv$", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  dplyr::transmute(import = lapply(import, patchr::rename, impexp::access_import("_rename", access_base_path)),
                   import = lapply(import, patchr::transcode, impexp::access_import("_contents", access_base_path))) %>% 
  tidyr::unnest() %>% 
  doublon_maj_etudiant() # %>% 
  # dplyr::semi_join(apogee::inscrits, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

usethis::use_data(inscrits_elp, overwrite = TRUE)

#### Résultats ELP ####

resultats_elp <- impexp::csv_import_path("data-raw", pattern = "^Resultats_ELP.*?\\.csv$", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  dplyr::transmute(import = purrr::map(import, patchr::rename, impexp::access_import("_rename", access_base_path)),
                   import = purrr::map(import, patchr::transcode, impexp::access_import("_contents", access_base_path))) %>% 
  tidyr::unnest() %>% 
  doublon_maj_etudiant() %>% 
  dplyr::mutate(presence_examen = dplyr::case_when(
    code_resultat == "DEM" ~ "N",
    code_resultat == "DEF" & note > 0 ~ "O",
    code_resultat == "DEF" ~ "N",
    apogee::hier_resultat_parent(code_resultat) == "Admis" | code_resultat == "ADJ" ~ "O",
    is.na(note) | note == 0 ~ "N",
    TRUE ~ "O"
  ))

usethis::use_data(resultats_elp, overwrite = TRUE)

#### Résultats Etape ####

resultats_etape <- impexp::csv_import_path("data-raw", pattern = "Resultats_etape\\.csv$", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  dplyr::transmute(import = purrr::map(import, patchr::rename, impexp::access_import("_rename", access_base_path)),
                   import = purrr::map(import, patchr::transcode, impexp::access_import("_contents", access_base_path))) %>% 
  tidyr::unnest() %>% 
  doublon_maj_etudiant()

# PACES
resultats_paces <- impexp::csv_import_path("data-raw", pattern = "Resultats_etape_paces\\.csv$", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  dplyr::transmute(import = purrr::map(import, patchr::rename, impexp::access_import("_rename", access_base_path)),
                   import = purrr::map(import, patchr::transcode, impexp::access_import("_contents", access_base_path))) %>% 
  tidyr::unnest() %>% 
  doublon_maj_etudiant()

resultats_etape <- resultats_etape %>% 
  dplyr::anti_join(resultats_paces, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
  dplyr::bind_rows(resultats_paces)

#doublons session

doublons <- resultats_etape %>% 
  patchr::duplicate(annee, code_etape, code_etudiant, inscription_premiere, lib_session) %>% 
  dplyr::mutate(admis = ifelse(code_resultat %in% c("ADM", "ADJ"), 1, NA_integer_)) %>% 
  dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere, lib_session, admis) %>% 
  dplyr::group_by(annee, code_etape, code_etudiant, inscription_premiere, lib_session) %>% 
  dplyr::filter(dplyr::row_number() == 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-admis)

resultats_etape <- resultats_etape %>% 
  dplyr::anti_join(doublons, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere", "lib_session")) %>% 
  dplyr::bind_rows(doublons) %>% 
  dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere, lib_session)

#Supression de Session 2 si : résultat Session 1 ADM et résultat Session 2 non-ADM

suppression_session2 <- apogee::resultats_etape %>% 
  dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, lib_session, code_resultat) %>% 
  tidyr::spread(lib_session, code_resultat) %>% 
  patchr::normalise_colnames() %>% 
  dplyr::filter(apogee::hier_resultat_parent(session_1) == "Admis" & !apogee::hier_resultat_parent(session_2) %in% c(NA_character_, "Admis")) %>% 
  dplyr::mutate(lib_session = "Session 2")

resultats_etape <- resultats_etape %>% 
  dplyr::anti_join(suppression_session2, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere", "lib_session"))

# Supprimer les résultats étape non-existants chez les inscrits
resultats_etape <- resultats_etape %>% 
  dplyr::semi_join(apogee::inscrits, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

#Sauvegarde

usethis::use_data(resultats_etape, overwrite = TRUE)

#### Résultats diplôme ####

resultats_diplome <- impexp::csv_import_path("data-raw", pattern = "Resultats_diplome\\.csv$", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  dplyr::transmute(import = purrr::map(import, patchr::rename, impexp::access_import("_rename", access_base_path)),
                   import = purrr::map(import, patchr::transcode, impexp::access_import("_contents", access_base_path))) %>% 
  tidyr::unnest() %>% 
  doublon_maj_etudiant()

#Suppression diplômés non-existants chez les inscrits

resultats_diplome <- resultats_diplome %>%
  dplyr::semi_join(dplyr::select(apogee::inscrits, annee, code_etape, code_etudiant, inscription_premiere),
                   by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

usethis::use_data(resultats_diplome, overwrite = TRUE)

#### Diplômés ####

diplomes <- impexp::csv_import_path("data-raw", pattern = "Diplomes\\.csv$", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  dplyr::transmute(import = purrr::map(import, patchr::rename, impexp::access_import("_rename", access_base_path)),
                   import = purrr::map(import, patchr::transcode, impexp::access_import("_contents", access_base_path))) %>% 
  tidyr::unnest() %>% 
  doublon_maj_etudiant()

# Ajout diplômés Base Access

ajout_diplomes <- impexp::access_import("diplomes_ajout", "data-raw/data/Tables_ref_individus.accdb") %>% 
  dplyr::select(-commentaire, -date_maj)

ajout_diplomes %>%
  dplyr::semi_join(dplyr::filter(diplomes,
                                 apogee::hier_resultat_parent(code_resultat) == "Admis"),
                   by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

diplomes <- diplomes %>% 
  dplyr::anti_join(ajout_diplomes, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
  dplyr::bind_rows(ajout_diplomes) %>% 
  dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere)

# Suppression diplômés non-existants chez les inscrits

diplomes <- diplomes %>%
  dplyr::semi_join(dplyr::select(apogee::inscrits, annee, code_etape, code_etudiant, inscription_premiere),
                   by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

# Suppression des étudiants de DUT en 1ère année

diplomes <- diplomes %>% 
  dplyr::filter(apogee::hier_etape_type_diplome(code_etape) == "DUT",
                apogee::annee_etape(code_etape) == 1) %>% 
  dplyr::anti_join(diplomes, ., by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

usethis::use_data(diplomes, overwrite = TRUE)

#### Individus ####

individus <- impexp::csv_import_path("data-raw", pattern = "Individus\\.csv", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  tidyr::unnest() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>% 
  dplyr::semi_join(dplyr::bind_rows(inscrits, inscrits_annules, inscrits_cpge),
                   by = "code_etudiant")

individus_bac <- impexp::csv_import_path("data-raw", pattern = "Individus - Bac\\.csv", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  tidyr::unnest() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>% 
  dplyr::arrange(code_etudiant, desc(annee_bac), code_mention_bac, code_type_etab_bac) %>% 
  dplyr::group_by(code_etudiant) %>% 
  dplyr::filter(dplyr::row_number() == 1) %>% 
  dplyr::ungroup()

individus <- dplyr::left_join(individus, individus_bac, by = "code_etudiant")

individus_mail_ups <- impexp::csv_import_path("data-raw", pattern = "Individus - Mail UPS\\.csv", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  tidyr::unnest() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path))

individus <- dplyr::left_join(individus, individus_mail_ups, by = "code_etudiant")

individus_departement_naissance <- impexp::csv_import_path("data-raw", pattern = "Individus_departement_naissance\\.csv", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  tidyr::unnest() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path))

individus <- dplyr::left_join(individus, individus_departement_naissance, by = "code_etudiant")

usethis::use_data(individus, overwrite = TRUE)

#### Individus - origine ####

individus_diplome_anterieur <- impexp::csv_import_path("data-raw", pattern = "Individus_diplome_origine\\.csv", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  tidyr::unnest() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>% 
  dplyr::arrange(code_etudiant, annee_diplome_obtenu)

individus_diplome_externe <- impexp::csv_import_path("data-raw", pattern = "Individus_diplome_externe\\.csv", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  tidyr::unnest() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>% 
  dplyr::mutate(code_type_diplome_externe = stringr::str_pad(code_type_diplome_externe, 3, "left", "0")) %>% 
  dplyr::arrange(code_etudiant, annee_diplome_externe) %>% 
  tidyr::drop_na(code_type_diplome_externe)

individus_diplome_origine <- individus_diplome_anterieur %>% 
  dplyr::full_join(individus_diplome_externe, 
                   by = c("code_etudiant", c("annee_diplome_obtenu" = "annee_diplome_externe"))) %>% 
  dplyr::left_join(impexp::access_import("diplome_externe_anterieur", access_base_path) %>% 
                     dplyr::rename(code_type_diplome_anterieur_maj = code_type_diplome_anterieur),
                   by = "code_type_diplome_externe") %>% 
  dplyr::mutate(
    code_type_diplome_anterieur = dplyr::case_when(
      code_type_diplome_anterieur %in% c(NA_character_, "N", "U", "X") ~ code_type_diplome_anterieur_maj,
      code_type_diplome_anterieur == "N" & !is.na(code_type_diplome_externe) ~ "LI",
      TRUE ~ code_type_diplome_anterieur)
  ) %>% 
  tidyr::drop_na(code_type_diplome_anterieur) %>% 
  dplyr::mutate(ordre = dplyr::case_when(
    code_type_diplome_anterieur == code_type_diplome_anterieur_maj ~ 2,
    code_type_diplome_anterieur %in% c("I", "Q", "N", "U", "X") ~ 3,
    code_type_diplome_anterieur %in% c("Y", "Z") ~ 4,
    TRUE ~ 1
  )) %>% 
  dplyr::arrange(code_etudiant, annee_diplome_obtenu, ordre) %>% 
  dplyr::group_by(code_etudiant, annee_diplome_obtenu) %>% 
  dplyr::filter(dplyr::row_number() == 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(code_etudiant, annee_diplome_obtenu, code_type_diplome_origine = code_type_diplome_anterieur, code_etab_diplome_obtenu, code_departement_pays_dernier_diplome) %>% 
  dplyr::semi_join(dplyr::bind_rows(inscrits, inscrits_annules, inscrits_cpge),
                   by = "code_etudiant")

usethis::use_data(individus_diplome_origine, overwrite = TRUE)

individus_situation_annee_precedente <- impexp::csv_import_path("data-raw", pattern = "Individus_situation_annee_prece\\.csv", zip = TRUE, skip = 1, encoding = "UTF-8") %>% 
  tidyr::unnest() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>% 
  dplyr::semi_join(dplyr::bind_rows(inscrits, inscrits_annules, inscrits_cpge),
                   by = "code_etudiant")

usethis::use_data(individus_situation_annee_precedente, overwrite = TRUE)

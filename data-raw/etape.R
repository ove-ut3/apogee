#### Etape ####

n_inscrits <- dplyr::bind_rows(apogee::inscrits, apogee::inscrits_cpge) %>% 
  dplyr::arrange(annee, code_etape) %>% 
  dplyr::count(code_etape) %>% 
  dplyr::rename(n_inscrits = n)

annee_premiere_etape <- dplyr::bind_rows(apogee::inscrits, apogee::inscrits_cpge, apogee::inscrits_annules) %>% 
  dplyr::arrange(annee, code_etape) %>% 
  dplyr::select(code_etape, annee_premiere_etape = annee) %>% 
  dplyr::group_by(code_etape) %>% 
  dplyr::filter(dplyr::row_number() == 1) %>% 
  dplyr::ungroup()

annee_derniere_etape <- dplyr::bind_rows(apogee::inscrits, apogee::inscrits_cpge, apogee::inscrits_annules) %>% 
  dplyr::arrange(annee, code_etape) %>% 
  dplyr::select(code_etape, annee_derniere_etape = annee) %>% 
  dplyr::group_by(code_etape) %>% 
  dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
  dplyr::ungroup()

etape_diplome_type <- readxl::read_excel("data-raw/Etape.xlsx", "Etape_diplome_type", skip = 1) %>% 
  patchr::rename(apogee::rename) %>% 
  patchr::recode_formula(apogee::recodage %>% 
                           patchr::filter_data_patch(source = "data_diplome")) %>% 
  dplyr::select(-annee) %>% 
  dplyr::group_by(code_etape) %>% 
  dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
  dplyr::ungroup() %>% 
  patchr::anti_join_bind(impexp::access_import("etape_diplome_type_ajout", "data-raw/Tables_ref.accdb") %>% 
                           dplyr::select(-date_maj), ., by = "code_etape")

etape <- readxl::read_excel("data-raw/Etape.xlsx", skip = 1) %>% 
  patchr::rename(apogee::rename) %>% 
  patchr::remove_duplicate(temoin_annee1_diplome) %>% 
  dplyr::rename(lib_etape_apogee = lib_etape) %>% 
  dplyr::left_join(impexp::access_import("etape", "data-raw/Tables_ref.accdb") %>% 
                     dplyr::mutate(temoin_access = TRUE),
                   by = "code_etape") %>% 
  dplyr::left_join(etape_diplome_type, by = "code_etape") %>% 
  patchr::recode_formula(apogee::recodage %>% 
                           patchr::filter_data_patch(source = "data_etape")) %>% 
  dplyr::mutate(temoin_etape_apogee = ifelse(lib_etape != lib_etape_apogee, FALSE, TRUE)) %>% 
  dplyr::select(-annee_etape_apogee) %>% 
  dplyr::left_join(n_inscrits, by = "code_etape") %>% 
  dplyr::left_join(annee_premiere_etape, by = "code_etape") %>% 
  dplyr::left_join(annee_derniere_etape, by = "code_etape") %>% 
  dplyr::mutate(actif = dplyr::if_else(annee_derniere_etape >= apogee::annee_en_cours() | !is.na(actif), TRUE, FALSE, FALSE))

etape_ville <- etape %>% 
  dplyr::filter(is.na(ville),
                code_type_diplome %in% c("DUT", "Licence pr")) %>% 
  dplyr::left_join(apogee::etape_composante %>% 
                     dplyr::arrange(code_etape, derniere_annee) %>% 
                     dplyr::group_by(code_etape) %>% 
                     dplyr::filter(dplyr::row_number() == dplyr::n()),
                   by = "code_etape") %>% 
  dplyr::left_join(dplyr::rename(apogee::composante, ville_composante = ville), 
                   by = "code_composante") %>% 
  dplyr::mutate(ville = ifelse(!is.na(ville_composante), ville_composante, ville),
                lib_etape_lower = tolower(lib_etape))

jointure <- dplyr::select(etape_ville, code_etape, code_type_diplome, ville, annee_etape, lib_etape_lower)

etape_ville2 <- etape_ville %>% 
  dplyr::select(lib_etape_lower, ville, code_type_diplome, annee_etape) %>% 
  unique() %>% 
  dplyr::group_by(lib_etape_lower, code_type_diplome, annee_etape) %>% 
  dplyr::filter(dplyr::n() >= 2) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(jointure, by = c("lib_etape_lower", "code_type_diplome", "annee_etape", "ville"))

etape <- etape %>% 
  dplyr::left_join(dplyr::select(etape_ville2, code_etape, ville_maj = ville), by = "code_etape") %>% 
  dplyr::mutate(ville = ifelse(!is.na(ville_maj), ville_maj, ville)) %>% 
  dplyr::select(-ville_maj) %>% 
  # Nouvelle passe pour les libellés d'étape
  patchr::recode_formula(apogee::recodage %>% 
                           patchr::filter_data_patch(source = "data_etape"))

if (nrow(patchr::duplicate(etape, code_etape)) >= 1) {
  stop("Doublons étape", call. = FALSE)
}

usethis::use_data(etape, overwrite = TRUE)

#### Etape - histo ####

etape_histo <- impexp::access_import("etape_histo", "data-raw/Tables_ref.accdb") %>% 
  dplyr::filter(is.na(suppression_histo))

dplyr::filter(etape_histo, is.na(code_elp)) %>% 
  patchr::duplicate(code_etape, code_etape_succ)

dplyr::filter(etape_histo, !is.na(code_elp)) %>% 
  patchr::duplicate(code_etape, code_elp, code_etape_succ)

# Eclatements
eclatement <- etape_histo %>% 
  patchr::duplicate(code_etape) %>% 
  dplyr::mutate(doublon = "éclatement")

eclatement_elp <- etape_histo %>% 
  patchr::duplicate(code_etape, code_elp) %>% 
  dplyr::mutate(doublon_elp = "éclatement")

etape_histo <- etape_histo %>% 
  dplyr::anti_join(eclatement, by = c("code_etape", "code_etape_succ")) %>% 
  dplyr::bind_rows(eclatement) %>% 
  dplyr::anti_join(eclatement_elp, by = c("code_etape", "code_elp", "code_etape_succ")) %>% 
  dplyr::bind_rows(eclatement_elp) %>% 
  dplyr::mutate(doublon = ifelse(!is.na(doublon_elp), doublon_elp, doublon)) %>% 
  dplyr::arrange(code_etape, code_elp, code_etape_succ)

usethis::use_data(etape_histo, overwrite = TRUE)

#### Etape - composante ####

etape_composante <- readxl::read_excel("data-raw/Etape.xlsx", "Etape_composante", skip = 1) %>% 
  patchr::rename(apogee::rename) %>% 
  dplyr::anti_join(impexp::access_import("etape_composante", "data-raw/Tables_ref.accdb") %>% 
                     tidyr::drop_na(suppression),
                   by = c("code_etape", "code_composante")) %>% 
  dplyr::arrange(code_etape, code_composante, annee) %>% 
  dplyr::group_by(code_etape, code_composante) %>% 
  dplyr::summarise(premiere_annee = min(annee),
                   derniere_annee = max(annee)) %>% 
  dplyr::ungroup()

usethis::use_data(etape_composante, overwrite = TRUE)

#### Etape - mention ####

etape_mention <- readxl::read_excel("data-raw/Etape.xlsx", "Etape_mention", skip = 1) %>% 
  patchr::rename(apogee::rename) %>% 
  dplyr::bind_rows(impexp::access_import("etape_mention_diplome", "data-raw/Tables_ref.accdb")) %>% 
  dplyr::arrange(code_etape, code_mention_diplome)

ajout_apogee <- impexp::access_import("etape_mention_diplome", "data-raw/Tables_ref.accdb") %>% 
  dplyr::filter(is.na(suppression)) %>% 
  dplyr::semi_join(
    readxl::read_excel("data-raw/Etape.xlsx", "Etape_mention", skip = 1) %>% 
      patchr::rename(apogee::rename),
    by = c("code_etape", "code_mention_diplome")) %>% 
  dplyr::arrange(code_etape, code_mention_diplome)

if (nrow(ajout_apogee)) {
  stop("Liens étape-mention dans la base Access déjà présent dans Apogée", call. = FALSE)
}

etape_mention <- etape_mention %>% 
  dplyr::filter(is.na(suppression)) %>% 
  dplyr::select(-suppression) %>% 
  dplyr::left_join(dplyr::filter(etape_mention, !is.na(suppression)) %>% 
                     dplyr::select(-date_maj), 
                   by = c("code_etape", "code_mention_diplome")) %>% 
  dplyr::mutate(code_mention_diplome = ifelse(!is.na(suppression), NA_character_, code_mention_diplome)) %>% 
  dplyr::select(-suppression) %>% 
  dplyr::arrange(code_etape, code_mention_diplome) %>% 
  dplyr::group_by(code_etape) %>% 
  dplyr::filter(!is.na(code_mention_diplome) | dplyr::row_number() == 1) %>% 
  dplyr::ungroup() %>% 
  unique()

if (nrow(patchr::duplicate(etape_mention, code_etape, code_mention_diplome))) {
  stop("Doublons dans etape_mention", call. = FALSE)
}

usethis::use_data(etape_mention, overwrite = TRUE)

#### Etape - domaine  ####

etape_domaine <- readxl::read_excel("data-raw/Etape.xlsx", "Etape_domaine") %>% 
  patchr::rename(apogee::rename) %>% 
  dplyr::bind_rows(impexp::access_import("etape_domaine_diplome", "data-raw/Tables_ref.accdb")) %>% 
  dplyr::arrange(code_etape, code_domaine_diplome)

etape_domaine <- etape_domaine %>% 
  dplyr::filter(is.na(suppression)) %>% 
  dplyr::select(-suppression) %>% 
  dplyr::left_join(dplyr::filter(etape_domaine, !is.na(suppression)), by = c("code_etape", "code_domaine_diplome")) %>% 
  dplyr::mutate(code_domaine_diplome = ifelse(!is.na(suppression), NA_character_, code_domaine_diplome)) %>% 
  dplyr::select(-suppression) %>% 
  dplyr::arrange(code_etape, code_domaine_diplome) %>% 
  dplyr::group_by(code_etape) %>% 
  dplyr::filter(!is.na(code_domaine_diplome) | dplyr::row_number() == 1) %>% 
  dplyr::ungroup()

patchr::duplicate(etape_domaine, code_etape, code_domaine_diplome)

usethis::use_data(etape_domaine, overwrite = TRUE)

#### Etape - cycle ####

etape_cycle <- readxl::read_excel("data-raw/Etape.xlsx", "Etape_cycle") %>% 
  patchr::rename(apogee::rename)

usethis::use_data(etape_cycle, overwrite = TRUE)

#### Etape - spécialité ####

etape_specialite_diplome <- readxl::read_excel("data-raw/Etape.xlsx", "Etape_specialite") %>% 
  patchr::rename(apogee::rename) %>% 
  dplyr::anti_join(dplyr::filter(., !is.na(code_specialite_diplome)) %>% 
                     dplyr::mutate(code_specialite_diplome = NA_character_),
                   by = c("code_etape", "code_specialite_diplome"))

usethis::use_data(etape_specialite_diplome, overwrite = TRUE)

#### Etape - finalité ####

etape_finalite <- readxl::read_excel("data-raw/Etape.xlsx", "Etape_finalite", skip = 1) %>% 
  patchr::rename(apogee::rename) %>% 
  patchr::recode_formula(apogee::recodage %>% 
                           patchr::filter_data_patch(source = "data_etape_finalite")) %>% 
  dplyr::arrange(code_etape, code_finalite_diplome) %>% 
  dplyr::group_by(code_etape) %>% 
  dplyr::filter(!is.na(code_finalite_diplome) | dplyr::row_number() == 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(code_finalite_diplome = ifelse(is.na(code_finalite_diplome),
                                               dplyr::recode(substr(code_etape, 2, 2),
                                                             "F" = "005",
                                                             "I" = "004",
                                                             "P" = "001",
                                                             "R" = "002",
                                                             .default = NA_character_),
                                               code_finalite_diplome))

patchr::duplicate(etape_finalite, code_etape)

usethis::use_data(etape_finalite, overwrite = TRUE)

#### Etape - secteur ####

etape_secteur <- impexp::access_import("etape_secteur", "data-raw/Tables_ref.accdb")

usethis::use_data(etape_secteur, overwrite = TRUE)

#### Etape - discipline SISE ####

etape_sise_discipline <- readxl::read_excel("data-raw/Etape.xlsx", "Etape_discipline_sise") %>% 
  patchr::rename(apogee::rename) %>% 
  dplyr::mutate(code_discipline_sise = stringr::str_remove(code_discipline_sise, "^00") %>% 
                  stringr::str_c("SD", .)) %>% 
  dplyr::bind_rows(impexp::access_import("etape_discipline_sise", "data-raw/Tables_ref.accdb") %>% 
                     dplyr::filter(is.na(suppression)) %>% 
                     dplyr::select(-suppression)) %>% 
  dplyr::left_join(impexp::access_import("etape_discipline_sise", "data-raw/Tables_ref.accdb"),
                   by = c("code_etape", "code_discipline_sise")) %>% 
  dplyr::filter(is.na(suppression)) %>% 
  dplyr::select(-suppression)

usethis::use_data(etape_sise_discipline, overwrite = TRUE)

#### Cycle ####

cycle <- readxl::read_excel("data-raw/Etape.xlsx", "Cycle") %>% 
  patchr::rename(apogee::rename)

usethis::use_data(cycle, overwrite = TRUE)

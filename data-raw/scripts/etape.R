#### Etape ####

n_inscrits <- dplyr::bind_rows(apogee::inscrits, apogee::inscrits_cpge) %>%
  dplyr::arrange(annee, code_etape) %>%
  dplyr::count(code_etape, name = "n_inscrits")

annees_activite <- readxl::read_excel("data-raw/data/Etape.xlsx", "Etape_diplome_type", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>% 
  dplyr::count(code_etape, annee) %>% 
  dplyr::select(-n) %>% 
  tidyr::nest(annees_activite = annee) %>% 
  dplyr::mutate_at("annees_activite", ~ purrr::map(., 1))

etape_diplome_type <- readxl::read_excel("data-raw/data/Etape.xlsx", "Etape_diplome_type", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  patchr::recode_formula(
    impexp::access_import("_recodage", access_base_path) %>%
      dplyr::filter(source == "data_diplome")) %>%
  dplyr::select(-annee) %>%
  dplyr::group_by(code_etape) %>%
  dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
  dplyr::ungroup() %>%
  patchr::anti_join_bind(
    impexp::access_import("etape_diplome_type_ajout", "data-raw/data/Tables_ref.accdb") %>%
      dplyr::filter(is.na(suppression)) %>%
      dplyr::select(-date_maj, -suppression), ., by = "code_etape") %>%
      dplyr::anti_join(impexp::access_import("etape_diplome_type_ajout", "data-raw/data/Tables_ref.accdb") %>%
      dplyr::filter(!is.na(suppression)),
    by = c("code_etape", "code_type_diplome")
  )

etape <- readxl::read_excel("data-raw/data/Etape.xlsx", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  tidyr::nest(annee1_diplome = annee1_diplome) %>% 
  dplyr::mutate_at("annee1_diplome", purrr::map, 1) %>% 
  dplyr::mutate_at("annee1_diplome", purrr::pluck, 1) %>% 
  dplyr::semi_join(
    dplyr::bind_rows(apogee::inscrits, apogee::inscrits_annules, apogee::inscrits_cpge),
    by = "code_etape"
  ) %>% 
  dplyr::rename(lib_etape_apogee = lib_etape) %>%
  dplyr::left_join(
    impexp::access_import("etape", "data-raw/data/Tables_ref.accdb"),
    by = "code_etape"
  ) %>%
  dplyr::left_join(etape_diplome_type, by = "code_etape") %>%
  patchr::recode_formula(
    impexp::access_import("_recodage", access_base_path) %>%
      dplyr::filter(source == "data_etape")
  ) %>%
  dplyr::mutate(temoin_etape_apogee = dplyr::if_else(lib_etape != lib_etape_apogee, FALSE, TRUE)) %>%
  dplyr::select(-annee_etape_apogee) %>%
  dplyr::left_join(n_inscrits, by = "code_etape") %>%
  dplyr::left_join(annees_activite, by = "code_etape") %>%
  dplyr::mutate(
    annee_en_cours = apogee::annee_en_cours(),
    actif = purrr::map2_lgl(annee_en_cours, annees_activite, ~ .x %in% .y)
  )

etape_ville <- etape %>%
  dplyr::filter(
    is.na(ville),
    code_type_diplome %in% c("DUT", "Licence pr")
  ) %>%
  dplyr::left_join(apogee::etape_composante %>%
    dplyr::arrange(code_etape, premiere_annee, derniere_annee) %>%
    dplyr::group_by(code_etape) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()),
  by = "code_etape"
  ) %>%
  dplyr::left_join(dplyr::rename(apogee::composante, ville_composante = ville),
    by = "code_composante"
  ) %>%
  dplyr::mutate(
    ville = dplyr::if_else(!is.na(ville_composante), ville_composante, ville),
    lib_etape_lower = tolower(lib_etape)
  )

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
  dplyr::mutate(ville = dplyr::if_else(!is.na(ville_maj), ville_maj, ville)) %>%
  dplyr::select(-ville_maj) %>%
  # Nouvelle passe pour les libellés d'étape
  patchr::recode_formula(impexp::access_import("_recodage", access_base_path) %>%
    dplyr::filter(source == "data_etape"))

etape <- etape %>% 
  dplyr::select(code_etape, lib_etape, acronyme_etape, annee_etape, annee_diplome, code_type_diplome, option, acronyme_option, particularite, acronyme_particularite, ville, cohabilite, annees_activite, actif, n_inscrits, temoin_etape_apogee, dplyr::starts_with("lib_etape"), dplyr::starts_with("acronyme_etape"), -lib_etape_court)

usethis::use_data(etape, overwrite = TRUE)

#### Etape - historique ####

etape_histo <- impexp::access_import("etape_histo", "data-raw/data/Tables_ref.accdb") %>%
  dplyr::filter(is.na(suppression_histo))

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
  dplyr::mutate(doublon = dplyr::if_else(!is.na(doublon_elp), doublon_elp, doublon)) %>%
  dplyr::arrange(code_etape, code_elp, code_etape_succ)

usethis::use_data(etape_histo, overwrite = TRUE)

#### Etape - composante ####

etape_composante <- readxl::read_excel("data-raw/data/Etape.xlsx", "Etape_composante", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  dplyr::anti_join(impexp::access_import("etape_composante", "data-raw/data/Tables_ref.accdb") %>%
    tidyr::drop_na(suppression),
  by = c("code_etape", "code_composante")
  ) %>%
  dplyr::arrange(code_etape, code_composante, annee) %>%
  dplyr::group_by(code_etape, code_composante) %>%
  dplyr::summarise(
    premiere_annee = min(annee),
    derniere_annee = max(annee)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    apogee::etape %>%
      dplyr::mutate(annee_derniere_etape = purrr::map_int(annees_activite, tail, 1)) %>% 
      dplyr::select(code_etape, annee_derniere_etape),
    by = "code_etape"
  ) %>%
  # Ajout de la dernière année d'activité (si actif différent de vide)
  dplyr::mutate_at(c("premiere_annee", "derniere_annee"), as.integer) %>%
  dplyr::mutate(derniere_annee = dplyr::if_else(derniere_annee != annee_derniere_etape, annee_derniere_etape, derniere_annee, derniere_annee)) %>%
  dplyr::select(-annee_derniere_etape)

usethis::use_data(etape_composante, overwrite = TRUE)

#### Etape - mention ####

etape_mention <- readxl::read_excel("data-raw/data/Etape.xlsx", "Etape_mention", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  dplyr::bind_rows(impexp::access_import("etape_mention_diplome", "data-raw/data/Tables_ref.accdb")) %>%
  dplyr::arrange(code_etape, code_mention_diplome)

ajout_apogee <- impexp::access_import("etape_mention_diplome", "data-raw/data/Tables_ref.accdb") %>%
  dplyr::filter(is.na(suppression)) %>%
  dplyr::semi_join(
    readxl::read_excel("data-raw/data/Etape.xlsx", "Etape_mention", skip = 1) %>%
      patchr::rename(impexp::access_import("_rename", access_base_path)),
    by = c("code_etape", "code_mention_diplome")
  ) %>%
  dplyr::arrange(code_etape, code_mention_diplome)

if (nrow(ajout_apogee)) {
  stop("Liens étape-mention dans la base Access déjà présent dans Apogée", call. = FALSE)
}

etape_mention <- etape_mention %>%
  dplyr::filter(is.na(suppression)) %>%
  dplyr::select(-suppression) %>%
  dplyr::left_join(
    dplyr::filter(etape_mention, !is.na(suppression)) %>%
      dplyr::select(-date_maj),
    by = c("code_etape", "code_mention_diplome")
  ) %>%
  dplyr::mutate(code_mention_diplome = dplyr::if_else(!is.na(suppression), NA_character_, code_mention_diplome)) %>%
  dplyr::select(-suppression) %>%
  dplyr::arrange(code_etape, code_mention_diplome) %>%
  dplyr::group_by(code_etape) %>%
  dplyr::filter(!is.na(code_mention_diplome) | dplyr::row_number() == 1) %>%
  dplyr::ungroup() %>%
  unique() %>%
  tidyr::drop_na(code_mention_diplome)

if (nrow(patchr::duplicate(etape_mention, code_etape, code_mention_diplome))) {
  stop("Doublons dans etape_mention", call. = FALSE)
}

usethis::use_data(etape_mention, overwrite = TRUE)

#### Etape - domaine  ####

etape_domaine <- readxl::read_excel("data-raw/data/Etape.xlsx", "Etape_domaine") %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  dplyr::bind_rows(impexp::access_import("etape_domaine_diplome", "data-raw/data/Tables_ref.accdb")) %>%
  dplyr::arrange(code_etape, code_domaine_diplome)

etape_domaine <- etape_domaine %>%
  dplyr::filter(is.na(suppression)) %>%
  dplyr::select(-suppression) %>%
  dplyr::left_join(dplyr::filter(etape_domaine, !is.na(suppression)), by = c("code_etape", "code_domaine_diplome")) %>%
  dplyr::mutate(code_domaine_diplome = dplyr::if_else(!is.na(suppression), NA_character_, code_domaine_diplome)) %>%
  dplyr::select(-suppression) %>%
  dplyr::arrange(code_etape, code_domaine_diplome) %>%
  dplyr::group_by(code_etape) %>%
  dplyr::filter(!is.na(code_domaine_diplome) | dplyr::row_number() == 1) %>%
  dplyr::ungroup()

patchr::duplicate(etape_domaine, code_etape, code_domaine_diplome)

usethis::use_data(etape_domaine, overwrite = TRUE)

#### Etape - finalité ####

etape_finalite <- readxl::read_excel("data-raw/data/Etape.xlsx", "Etape_finalite", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  patchr::recode_formula(
    impexp::access_import("_recodage", access_base_path) %>%
      dplyr::filter(source == "data_etape_finalite")
  ) %>%
  dplyr::arrange(code_etape, code_finalite_diplome) %>%
  dplyr::group_by(code_etape) %>%
  dplyr::filter(!is.na(code_finalite_diplome) | dplyr::row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    code_finalite_diplome = dplyr::if_else(
      is.na(code_finalite_diplome),
      dplyr::recode(
        substr(code_etape, 2, 2),
        "F" = "005",
        "I" = "004",
        "P" = "001",
        "R" = "002",
        .default = NA_character_
      ),
      code_finalite_diplome
    )
  ) %>%
  tidyr::drop_na(code_finalite_diplome)

patchr::duplicate(etape_finalite, code_etape)

usethis::use_data(etape_finalite, overwrite = TRUE)

#### Etape - secteur ####

etape_secteur <- impexp::access_import("etape_secteur", "data-raw/data/Tables_ref.accdb")

etape_secteur_histo <- etape_secteur %>%
  dplyr::mutate_at("code_etape", apogee::histo_etape_succ, multiple = TRUE) %>%
  tidyr::unnest_legacy(code_etape) %>%
  unique() %>%
  dplyr::anti_join(etape_secteur, by = "code_etape")

etape_secteur <- dplyr::bind_rows(etape_secteur, etape_secteur_histo)

stopifnot(nrow(patchr::duplicate(etape_secteur, code_etape)) == 0)

usethis::use_data(etape_secteur, overwrite = TRUE)

#### Etape - SISE ####

etape_sise <- readxl::read_excel("data-raw/data/Etape.xlsx", "Etape_sise", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>%
  dplyr::filter(annee >= 2007) %>%
  dplyr::mutate_at("code_diplome_sise", as.character) %>%
  dplyr::left_join(
    impexp::access_import("diplome_sise", "data-raw/data/Tables_ref.accdb") %>%
      dplyr::filter(is.na(annee)) %>%
      dplyr::select(-annee),
    by = c("code_diplome_sise" = "code_diplome")
  ) %>%
  dplyr::mutate(code_diplome_sise = dplyr::if_else(!is.na(code_diplome_maj), code_diplome_maj, code_diplome_sise)) %>%
  dplyr::select(code_etape, annee, code_diplome_sise) %>%
  dplyr::left_join(
    impexp::access_import("diplome_sise", "data-raw/data/Tables_ref.accdb") %>%
      tidyr::drop_na(annee),
    by = c("annee", "code_diplome_sise" = "code_diplome")
  ) %>%
  dplyr::mutate(code_diplome_sise = dplyr::if_else(!is.na(code_diplome_maj), code_diplome_maj, code_diplome_sise)) %>%
  dplyr::select(code_etape, annee, code_diplome_sise) %>%
  dplyr::left_join(
    impexp::access_import("etape_diplome_sise", "data-raw/data/Tables_ref.accdb") %>%
      dplyr::filter(is.na(annee)) %>%
      dplyr::select(-annee),
    by = "code_etape"
  ) %>%
  dplyr::mutate(code_diplome_sise = dplyr::if_else(!is.na(code_diplome_maj), code_diplome_maj, code_diplome_sise)) %>%
  dplyr::select(code_etape, annee, code_diplome_sise) %>%
  dplyr::left_join(
    impexp::access_import("etape_diplome_sise", "data-raw/data/Tables_ref.accdb") %>%
      tidyr::drop_na(annee),
    by = c("annee", "code_etape")
  ) %>%
  dplyr::mutate(code_diplome_sise = dplyr::if_else(!is.na(code_diplome_maj), code_diplome_maj, code_diplome_sise)) %>%
  dplyr::select(code_etape, annee, code_diplome_sise) %>%
  unique()

usethis::use_data(etape_sise, overwrite = TRUE)

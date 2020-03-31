#### Profil étudiant ####

profil_etudiant <- readxl::read_excel("data-raw/data/Inscription.xlsx", "Profil_etudiant", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  tidyr::drop_na(code_profil_etudiant)

usethis::use_data(profil_etudiant, overwrite = TRUE)

#### Régime inscription ####

regime_inscription <- impexp::access_import("regime_inscription", "data-raw/data/Tables_ref.accdb")

usethis::use_data(regime_inscription, overwrite = TRUE)

#### Statut étudiant ####

statut_etudiant <- readxl::read_excel("data-raw/data/Inscription.xlsx", "Statut_etudiant", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  tidyr::drop_na(code_statut_etudiant)

usethis::use_data(statut_etudiant, overwrite = TRUE)

#### Sexe ####

sexe <- impexp::access_import("sexe", "data-raw/data/Tables_ref.accdb")

usethis::use_data(sexe, overwrite = TRUE)

#### PCS ####

pcs <- impexp::access_import("pcs", "data-raw/data/Tables_ref.accdb")

usethis::use_data(pcs, overwrite = TRUE)

#### Bac ####

bac <- impexp::access_import("bac", "data-raw/data/Tables_ref.accdb")

usethis::use_data(bac, overwrite = TRUE)

#### Bac - mention ####

bac_mention <- impexp::access_import("bac_mention", "data-raw/data/Tables_ref.accdb")

usethis::use_data(bac_mention, overwrite = TRUE)

#### Bourse ####

bourse <- readxl::read_excel("data-raw/data/Inscription.xlsx", "Bourse", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path))

usethis::use_data(bourse, overwrite = TRUE)

#### Situation sociale ####

situation_sociale <- readxl::read_excel("data-raw/data/Inscription.xlsx", "Situation_sociale", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path))

usethis::use_data(situation_sociale, overwrite = TRUE)

#### Type d'établissement ####

etablissement_type <- readxl::read_excel("data-raw/data/Individu.xlsx", "Type établissement") %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  dplyr::add_row(code_type_etab = NA_character_, lib_type_etab = "Non-ventilé")

usethis::use_data(etablissement_type, overwrite = TRUE)

#### Diplôme version ####

diplome_version <- readxl::read_excel("data-raw/data/Diplome_version.xlsx", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>% 
  tidyr::nest_legacy(code_domaine_diplome, .key = "code_domaine_diplome") %>% 
  dplyr::mutate(code_domaine_diplome = purrr::map(code_domaine_diplome, ~ .[[1]]))

usethis::use_data(diplome_version, overwrite = TRUE)

#### Domaine ####

diplome_domaine <- readxl::read_excel("data-raw/data/Diplome_version.xlsx", "Formation_domaine", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  dplyr::full_join(impexp::access_import("diplome_domaine", "data-raw/data/Tables_ref.accdb") %>% 
                     dplyr::rename(maj_lib_domaine_diplome = lib_domaine_diplome),
                   by = "code_domaine_diplome") %>% 
  dplyr::mutate(lib_domaine_diplome = ifelse(!is.na(maj_lib_domaine_diplome), maj_lib_domaine_diplome, lib_domaine_diplome)) %>% 
  dplyr::select(-maj_lib_domaine_diplome)

usethis::use_data(diplome_domaine, overwrite = TRUE)

#### Finalité ####

diplome_finalite <- readxl::read_excel("data-raw/data/Diplome_version.xlsx", "Finalite", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  dplyr::bind_rows(impexp::access_import("diplome_finalite_ajout", "data-raw/data/Tables_ref.accdb"))

usethis::use_data(diplome_finalite, overwrite = TRUE)

#### Mention ####

mention_annee <- dplyr::bind_rows(apogee::inscrits, apogee::inscrits_cpge, apogee::inscrits_annules) %>% 
  dplyr::left_join(impexp::r_import("data/etape_mention.rda"), by = "code_etape") %>% 
  dplyr::arrange(annee, code_mention_diplome)

mention_premiere_annee <- mention_annee %>% 
  dplyr::select(code_mention_diplome, mention_premiere_annee = annee) %>% 
  dplyr::group_by(code_mention_diplome) %>% 
  dplyr::filter(dplyr::row_number() == 1) %>% 
  dplyr::ungroup()

mention_derniere_annee <- mention_annee %>% 
  dplyr::select(code_mention_diplome, mention_derniere_annee = annee) %>% 
  dplyr::group_by(code_mention_diplome) %>% 
  dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
  dplyr::ungroup()

diplome_mention <- readxl::read_excel("data-raw/data/Diplome_version.xlsx", "Mention", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  dplyr::full_join(impexp::access_import("diplome_mention", "data-raw/data/Tables_ref.accdb") %>% 
                     dplyr::rename(maj_lib_mention_diplome = lib_mention_diplome),
                   by = "code_mention_diplome") %>% 
  dplyr::mutate(lib_mention_diplome = ifelse(!is.na(maj_lib_mention_diplome), maj_lib_mention_diplome, lib_mention_diplome)) %>% 
  dplyr::select(-maj_lib_mention_diplome) %>% 
  dplyr::left_join(mention_premiere_annee, by = "code_mention_diplome") %>% 
  dplyr::left_join(mention_derniere_annee, by = "code_mention_diplome") %>% 
  dplyr::mutate(actif = dplyr::if_else(mention_derniere_annee >= apogee::annee_en_cours(), TRUE, FALSE, FALSE))

patchr::duplicate(diplome_mention, code_mention_diplome)

usethis::use_data(diplome_mention, overwrite = TRUE)

#### Mention - historique ####

diplome_mention_histo <- impexp::access_import("diplome_mention_histo", "data-raw/data/Tables_ref.accdb")

usethis::use_data(diplome_mention_histo, overwrite = TRUE)

#### Mention - compatibilité licence et master ####

diplome_mention_lm <- impexp::access_import("diplome_mention_lm", "data-raw/data/Tables_ref.accdb")

usethis::use_data(diplome_mention_lm, overwrite = TRUE)

#### Spécialité ####

diplome_specialite <- readxl::read_excel("data-raw/data/Diplome_version.xlsx", "Specialite") %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  tidyr::drop_na(code_specialite_diplome)

usethis::use_data(diplome_specialite, overwrite = TRUE)

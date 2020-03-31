#### Domaine ####

domaine_diplome <- readxl::read_excel("data-raw/data/Diplome_version.xlsx", "Formation_domaine", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  dplyr::full_join(impexp::access_import("domaine_diplome", "data-raw/data/Tables_ref.accdb") %>%
    dplyr::rename(maj_lib_domaine_diplome = lib_domaine_diplome),
  by = "code_domaine_diplome"
  ) %>%
  dplyr::mutate(lib_domaine_diplome = dplyr::if_else(!is.na(maj_lib_domaine_diplome), maj_lib_domaine_diplome, lib_domaine_diplome)) %>%
  dplyr::select(-maj_lib_domaine_diplome)

usethis::use_data(domaine_diplome, overwrite = TRUE)

#### Finalité ####

finalite_diplome <- readxl::read_excel("data-raw/data/Diplome_version.xlsx", "Finalite", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  dplyr::bind_rows(impexp::access_import("finalite_diplome_ajout", "data-raw/data/Tables_ref.accdb"))

usethis::use_data(finalite_diplome, overwrite = TRUE)

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

mention_diplome <- readxl::read_excel("data-raw/data/Diplome_version.xlsx", "Mention", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  dplyr::full_join(
    impexp::access_import("mention_diplome", "data-raw/data/Tables_ref.accdb") %>%
      dplyr::rename(maj_lib_mention_diplome = lib_mention_diplome),
    by = "code_mention_diplome"
  ) %>%
  dplyr::mutate(
    lib_mention_diplome = dplyr::if_else(
      !is.na(maj_lib_mention_diplome),
      maj_lib_mention_diplome,
      lib_mention_diplome
    )
  ) %>%
  dplyr::select(-maj_lib_mention_diplome) %>%
  dplyr::left_join(mention_premiere_annee, by = "code_mention_diplome") %>%
  dplyr::left_join(mention_derniere_annee, by = "code_mention_diplome") %>%
  dplyr::mutate(actif = dplyr::if_else(mention_derniere_annee >= apogee::annee_en_cours(), TRUE, FALSE, FALSE))

patchr::duplicate(mention_diplome, code_mention_diplome)

usethis::use_data(mention_diplome, overwrite = TRUE)

#### Mention - historique ####

mention_diplome_histo <- impexp::access_import("mention_diplome_histo", "data-raw/data/Tables_ref.accdb")

usethis::use_data(mention_diplome_histo, overwrite = TRUE)

#### Mention - compatibilité licence et master ####

mention_diplome_lm <- impexp::access_import("mention_diplome_lm", "data-raw/data/Tables_ref.accdb")

usethis::use_data(mention_diplome_lm, overwrite = TRUE)

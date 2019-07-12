#### SISE - Etape ####

conv_etape_sise <- readxl::read_excel("data-raw/Etape.xlsx", "Etape_sise", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>% 
  dplyr::filter(annee >= 2007) %>% 
  dplyr::mutate(code_diplome_sise = as.character(code_diplome_sise)) %>% 
  dplyr::left_join(impexp::access_import("diplome_sise", "data-raw/Tables_ref.accdb") %>% 
                     tidyr::drop_na(annee),
                   by = c("annee", "code_diplome_sise" = "code_diplome")) %>% 
  dplyr::mutate(code_diplome_sise = ifelse(!is.na(code_diplome_maj), code_diplome_maj, code_diplome_sise)) %>% 
  dplyr::select(code_etape, annee, code_diplome_sise) %>% 
  dplyr::left_join(impexp::access_import("diplome_sise", "data-raw/Tables_ref.accdb") %>% 
                     dplyr::filter(is.na(annee)) %>% 
                     dplyr::select(-annee),
                   by = c("code_diplome_sise" = "code_diplome")) %>% 
  dplyr::mutate(code_diplome_sise = ifelse(!is.na(code_diplome_maj), code_diplome_maj, code_diplome_sise)) %>% 
  dplyr::select(code_etape, annee, code_diplome_sise) %>% 
  dplyr::left_join(impexp::access_import("etape_diplome_sise", "data-raw/Tables_ref.accdb") %>% 
                     dplyr::filter(is.na(annee)) %>% 
                     dplyr::select(-annee),
                   by = "code_etape") %>% 
  dplyr::mutate(code_diplome_sise = ifelse(!is.na(code_diplome_maj), code_diplome_maj, code_diplome_sise)) %>% 
  dplyr::select(code_etape, annee, code_diplome_sise) %>% 
  unique()

usethis::use_data(conv_etape_sise, overwrite = TRUE)

#### SISE - diplôme ####

sise_diplome <- readxl::read_excel("data-raw/Diplome.xlsx", "Diplome_sise", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>% 
  dplyr::filter(annee >= 2007) %>% 
  dplyr::select(-annee) %>% 
  unique()

sise_diplome <- sise_diplome %>% 
  dplyr::anti_join(patchr::duplicate(sise_diplome, code_diplome), by = "code_diplome")

usethis::use_data(sise_diplome, overwrite = TRUE)

#### SISE - discipline ####

sise_discipline <- impexp::access_import("sise_discipline", "data-raw/Tables_ref.accdb")

usethis::use_data(sise_discipline, overwrite = TRUE)

#### SISE - libellé diplôme ####

sise_diplome_lib <- readxl::read_excel("data-raw/Diplome.xlsx", "Diplome_sise_lib", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path))

usethis::use_data(sise_diplome_lib, overwrite = TRUE)

#### SISE - Type diplôme ####

sise_diplome_type <- impexp::access_import("sise_diplome_type", "data-raw/Tables_ref.accdb")

usethis::use_data(sise_diplome_type, overwrite = TRUE)

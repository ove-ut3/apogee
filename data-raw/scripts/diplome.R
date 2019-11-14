#### Diplôme ####

diplome <- readxl::read_excel("data-raw/data/Diplome.xlsx", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path))

usethis::use_data(diplome, overwrite = TRUE)

#### Diplôme - type ####

diplome_type <- readxl::read_excel("data-raw/data/Etape.xlsx", "Etape_diplome_type") %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  dplyr::select(code_type_diplome) %>% 
  unique() %>% 
  dplyr::full_join(impexp::access_import("diplome_type", "data-raw/data/Tables_ref.accdb"), by = "code_type_diplome") %>% 
  dplyr::arrange(code_type_diplome) %>% 
  dplyr::mutate(acronyme_type_diplome = dplyr::if_else(is.na(acronyme_type_diplome), code_type_diplome, acronyme_type_diplome))

usethis::use_data(diplome_type, overwrite = TRUE)

#### Diplôme origine - type ####

diplome_origine_type <- readxl::read_excel("data-raw/data/Diplome.xlsx", "Diplome_anterieur_origine", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  dplyr::rename(code_type_diplome_origine = code_type_diplome_anterieur) %>% 
  dplyr::full_join(impexp::access_import("diplome_origine_type", "data-raw/data/Tables_ref.accdb"),
                   by = "code_type_diplome_origine") %>% 
  dplyr::mutate(lib_type_diplome_origine = dplyr::if_else(is.na(lib_type_diplome_origine), lib_type_diplome_anterieur, lib_type_diplome_origine)) %>% 
  dplyr::select(-lib_type_diplome_anterieur) %>% 
  dplyr::add_row(code_type_diplome_origine = NA_character_, lib_type_diplome_origine = "Non-ventilé")

usethis::use_data(diplome_origine_type, overwrite = TRUE)

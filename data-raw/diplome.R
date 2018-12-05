#### Diplôme ####

diplome <- readxl::read_excel("data-raw/Diplome.xlsx", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", "data-raw/Tables_ref.accdb"))

usethis::use_data(diplome, overwrite = TRUE)

#### Diplôme - type ####

diplome_type <- readxl::read_excel("data-raw/Etape.xlsx", "Etape_diplome_type") %>% 
  patchr::rename(impexp::access_import("_rename", "data-raw/Tables_ref.accdb")) %>% 
  dplyr::select(code_type_diplome) %>% 
  unique() %>% 
  dplyr::full_join(impexp::access_import("diplome_type", "data-raw/Tables_ref.accdb"), by = "code_type_diplome") %>% 
  dplyr::arrange(code_type_diplome) %>% 
  dplyr::mutate(acronyme_type_diplome = ifelse(is.na(acronyme_type_diplome), code_type_diplome, acronyme_type_diplome))

usethis::use_data(diplome_type, overwrite = TRUE)

#### Diplôme antérieur - type ####

diplome_anterieur_type <- readxl::read_excel("data-raw/Diplome.xlsx", "Diplome_anterieur_origine", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", "data-raw/Tables_ref.accdb")) %>% 
  patchr::recode_formula(impexp::access_import("_recodage", "data-raw/Tables_ref.accdb") %>% 
                           patchr::filter_data_patch("data_diplome_anterieur_type")) %>% 
  dplyr::add_row(code_type_diplome_anterieur = NA_character_, lib_type_diplome_anterieur = "Non-ventilé")

usethis::use_data(diplome_anterieur_type, overwrite = TRUE)


#### Diplôme externe - type ####

diplome_externe_type <- readxl::read_excel("data-raw/Diplome.xlsx", "Diplome_externe", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", "data-raw/Tables_ref.accdb")) %>% 
  dplyr::add_row(code_type_diplome_externe = NA_character_, lib_type_diplome_externe = "Non-ventilé")

usethis::use_data(diplome_externe_type, overwrite = TRUE)

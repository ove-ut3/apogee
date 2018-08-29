#### Diplôme ####

diplome <- readxl::read_excel("data-raw/Diplome.xlsx", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", "data-raw/Tables_ref.accdb"))

devtools::use_data(diplome, overwrite = TRUE)

#### Diplôme - type ####

diplome_type <- impexp::access_import("diplome_type", "data-raw/Tables_ref.accdb")

devtools::use_data(diplome_type, overwrite = TRUE)

#### Diplôme antérieur - type ####

diplome_anterieur_type <- readxl::read_excel("data-raw/Diplome.xlsx", "Diplome_anterieur_origine", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", "data-raw/Tables_ref.accdb")) %>% 
  dplyr::add_row(code_type_diplome_anterieur = NA_character_, lib_type_diplome_anterieur = "Non-ventilé")

devtools::use_data(diplome_anterieur_type, overwrite = TRUE)

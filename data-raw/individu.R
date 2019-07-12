#### Situation année précédente ####

situation_annee_precedente <- readxl::read_excel("data-raw/Individu.xlsx", "Situation_annee_precedente", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  tidyr::drop_na(code_situation_annee_precedente)

usethis::use_data(situation_annee_precedente, overwrite = TRUE)


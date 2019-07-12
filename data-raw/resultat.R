#### Résultat ####

resultat <- readxl::read_excel("data-raw/Resultat.xlsx", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  dplyr::full_join(impexp::access_import("resultat", "data-raw/Tables_ref.accdb") %>% 
                     dplyr::rename(lib_resultat_maj = lib_resultat),
                   by = "code_resultat") %>% 
  dplyr::mutate(lib_resultat = dplyr::if_else(!is.na(lib_resultat_maj), lib_resultat_maj, lib_resultat)) %>% 
  dplyr::select(-lib_resultat_maj)

usethis::use_data(resultat, overwrite = TRUE)

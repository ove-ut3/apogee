#### Résultat ####

resultat <- readxl::read_excel("data-raw/Resultat.xlsx", skip = 1) %>% 
  patchr::rename(apogee::rename) %>% 
  dplyr::left_join(impexp::access_import("resultat", "data-raw/Tables_ref.accdb"), by = "code_resultat")

usethis::use_data(resultat, overwrite = TRUE)

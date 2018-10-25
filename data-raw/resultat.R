#### Résultat ####

resultat <- readxl::read_excel("data-raw/Resultat.xlsx", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", "data-raw/Tables_ref.accdb"))

usethis::use_data(resultat, overwrite = TRUE)

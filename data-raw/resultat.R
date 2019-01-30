#### Résultat ####

resultat <- readxl::read_excel("data-raw/Resultat.xlsx", skip = 1) %>% 
  patchr::rename(apogee::rename)

usethis::use_data(resultat, overwrite = TRUE)

#### Académie ####

academie <- impexp::access_import("academie", "data-raw/data/Tables_ref.accdb")

usethis::use_data(academie, overwrite = TRUE)

#### Académie - département ####

departement_academie <- impexp::access_import("departement_academie", "data-raw/data/Tables_ref.accdb")

usethis::use_data(departement_academie, overwrite = TRUE)

#### BCE ####

bce_lycees <- readr::read_csv2("https://www.data.gouv.fr/s/resources/adresse-et-geolocalisation-des-etablissements-denseignement-du-premier-et-second-degres/20160526-143453/DEPP-etab-1D2D.csv", col_types = readr::cols(.default = "c"), locale = readr::locale(decimal_mark = ",", encoding = "Latin1")) %>%
  dplyr::mutate(code_postal_uai = stringr::str_pad(code_postal_uai, 5, "left", "0")) %>%
  dplyr::filter(substr(nature_uai, 1, 2) %in% c("30", "31", "32", "33", "37"))

usethis::use_data(bce_lycees, overwrite = TRUE)

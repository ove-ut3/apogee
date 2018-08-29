#### Académie ####

academie <- impexp::access_import("academie", "data-raw/Tables_ref.accdb")

devtools::use_data(academie, overwrite = TRUE)

#### Académie - département ####

departement_academie <- impexp::access_import("departement_academie", "data-raw/Tables_ref.accdb")

devtools::use_data(departement_academie, overwrite = TRUE)

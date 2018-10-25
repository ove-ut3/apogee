#### ELP ####

elp <- readxl::read_excel("data-raw/ELP.xlsx", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", "data-raw/Tables_ref.accdb")) %>% 
  dplyr::bind_rows(impexp::access_import("elp_ajout", "data-raw/Tables_ref.accdb")) %>% 
  dplyr::arrange(code_elp)

patchr::duplicate(elp, code_elp)

usethis::use_data(elp, overwrite = TRUE)

#### ELP nature ####

elp_nature <- readxl::read_excel("data-raw/ELP.xlsx", "ELP - Nature", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", "data-raw/Tables_ref.accdb"))

usethis::use_data(elp_nature, overwrite = TRUE)

#### ELP Période ####

elp_periode <- readxl::read_excel("data-raw/ELP.xlsx", "ELP - Periode", skip = 1) %>% 
  patchr::rename(impexp::access_import("_rename", "data-raw/Tables_ref.accdb"))

usethis::use_data(elp_periode, overwrite = TRUE)

#### ELP histo ####

elp_histo <- impexp::access_import("elp_histo", "data-raw/Tables_ref.accdb")

usethis::use_data(elp_histo, overwrite = TRUE)

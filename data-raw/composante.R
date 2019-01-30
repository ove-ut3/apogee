#### Composante ####

composante <- readxl::read_excel("data-raw/Composante.xlsx", skip = 1) %>% 
  patchr::rename(apogee::rename) %>% 
  dplyr::full_join(impexp::access_import("composante", "data-raw/Tables_ref.accdb") %>% 
                     dplyr::rename(lib_composante_maj = lib_composante),
                   by = "code_composante") %>% 
  dplyr::mutate(lib_composante = ifelse(!is.na(lib_composante_maj), lib_composante_maj, lib_composante)) %>% 
  dplyr::select(-lib_composante_maj) %>% 
  tidyr::drop_na(code_composante)

usethis::use_data(composante, overwrite = TRUE)

#### Composante - type ####

composante_type <- readxl::read_excel("data-raw/Composante.xlsx", "Composante_type", skip = 1) %>% 
  patchr::rename(apogee::rename) %>% 
  tidyr::drop_na(code_type_composante)

usethis::use_data(composante_type, overwrite = TRUE)

#### Composante - histo ####

composante_histo <- impexp::access_import("composante_histo", "data-raw/Tables_ref.accdb")

usethis::use_data(composante_histo, overwrite = TRUE)

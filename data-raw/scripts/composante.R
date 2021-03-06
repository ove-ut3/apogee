#### Composante ####

composante <- readxl::read_excel("data-raw/data/Composante.xlsx", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  dplyr::full_join(impexp::access_import("composante", "data-raw/data/Tables_ref.accdb") %>%
    dplyr::rename(lib_composante_maj = lib_composante),
  by = "code_composante"
  ) %>%
  dplyr::mutate(lib_composante = dplyr::if_else(!is.na(lib_composante_maj), lib_composante_maj, lib_composante)) %>%
  dplyr::select(-lib_composante_maj) %>%
  tidyr::drop_na(code_composante)

usethis::use_data(composante, overwrite = TRUE)

#### Composante - type ####

composante_type <- readxl::read_excel("data-raw/data/Composante.xlsx", "Composante_type", skip = 1) %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  tidyr::drop_na(code_type_composante)

usethis::use_data(composante_type, overwrite = TRUE)

#### Composante - histo ####

composante_histo <- impexp::access_import("composante_histo", "data-raw/data/Tables_ref.accdb")

usethis::use_data(composante_histo, overwrite = TRUE)

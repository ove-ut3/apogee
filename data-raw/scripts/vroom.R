system.time({
  resultats_elp <- list.files("data-raw", pattern = "Resultats_ELP.*?\\.zip", recursive = TRUE, full.names = TRUE) %>%
    purrr::map(vroom::vroom, delim = ";", skip = 1, col_types = vroom::cols()) %>%
    purrr::map(patchr::rename, impexp::access_import("_rename", access_base_path)) %>%
    purrr::map(patchr::transcode, impexp::access_import("_contents", access_base_path)) %>%
    dplyr::bind_rows() %>%
    tidyr::drop_na(annee) # Import vroom avec une ligne vide supplémentaire
})

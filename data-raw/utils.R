doublon_maj_etudiant <- function(table) {
  
  table <- table %>% 
    dplyr::left_join(impexp::access_import("doublons_code_etudiant", "data-raw/Tables_ref_individus.accdb"),
                     by = "code_etudiant") %>% 
    dplyr::mutate(code_etudiant = ifelse(!is.na(code_etudiant_maj), code_etudiant_maj, code_etudiant)) %>% 
    dplyr::select(-code_etudiant_maj, -commentaire, -date_maj) %>% 
    unique()
  
  return(table)
}

nest_inscrits <- function(table, champ_nest, cle = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) {
  
  quo_champ_nest <- dplyr::enquo(champ_nest)
  nom_champ_nest <- dplyr::quo_name(quo_champ_nest)
  
  nest2 <- table %>%
    dplyr::select(cle, !!quo_champ_nest) %>%
    unique() %>%
    dplyr::arrange(!!! rlang::syms(cle), !!quo_champ_nest) %>% 
    tidyr::nest(!!quo_champ_nest, .key = !!nom_champ_nest) %>%
    dplyr::mutate(!!nom_champ_nest := purrr::map(!!quo_champ_nest, ~ .[[1]]))
  
  nest2 <- table %>%
    dplyr::select(setdiff(names(table), nom_champ_nest)) %>%
    unique() %>%
    dplyr::full_join(nest2, by = cle)
  
  return(nest2)
}

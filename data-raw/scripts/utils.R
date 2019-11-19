doublon_maj_etudiant <- function(table) {
  
  table <- table %>% 
    dplyr::left_join(impexp::access_import("doublons_code_etudiant", "data-raw/data/Tables_ref_individus.accdb"),
                     by = "code_etudiant") %>% 
    dplyr::mutate(code_etudiant = dplyr::if_else(!is.na(code_etudiant_maj), code_etudiant_maj, code_etudiant)) %>% 
    dplyr::select(-code_etudiant_maj, -commentaire, -date_maj) %>% 
    unique()
  
  return(table)
}

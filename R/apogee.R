#' Renvoie l'annee d'etape a partir du code etape
#'
#' Renvoie l'année d'étape à partir du code étape (Bac+1 -> 1, Bac+3 -> 3, etc).
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur contenant les années d'étape.
#'
#' Jeu de données source : \code{apogee::etape}.\cr
#' Il est créé à partir d'Apogée et de la table "etape" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
annee_etape <- function(code_etape) {
  
  annee_etape <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape, by = "code_etape") %>%
    dplyr::pull(annee_etape)
  
  return(annee_etape)
}

#' Renvoie la premiere annee d'une etape a partir du code etape
#'
#' Renvoie la première annee d'une étape à partir du code étape.
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur contenant les années.
#'
#' Jeu de données source : \code{apogee::etape}.\cr
#'
#' @export
etape_premiere_annee <- function(code_etape) {
  
  etape_premiere_annee <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape, by = "code_etape") %>%
    dplyr::pull(annee_premiere_etape)
  
  return(etape_premiere_annee)
}

#' Renvoie la derniere annee d'une etape a partir du code etape
#'
#' Renvoie la dernière annee d'une étape à partir du code étape.
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur contenant les années.
#'
#' Jeu de données source : \code{apogee::etape}.\cr
#'
#' @export
etape_derniere_annee <- function(code_etape) {
  
  etape_derniere_annee <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape, by = "code_etape") %>%
    dplyr::pull(annee_derniere_etape)
  
  return(etape_derniere_annee)
}

#' Temoin TRUE/FALSE si le code etape est la premiere annee d'un diplome
#' 
#' Témoin TRUE/FALSE si le code étape est la première année d'un diplôme.
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur de booléens TRUE/FALSE.
#' 
#' @export
temoin_annee1_diplome <- function(code_etape) {
  
  temoin_annee1_diplome <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape, by = "code_etape") %>%
    dplyr::pull(temoin_annee1_diplome) %>% 
    dplyr::recode("O" = TRUE, "N" = FALSE)
  
  return(temoin_annee1_diplome)
}


#' Transformation de l'annee d'inscription en annee universitaire
#' 
#' Transformation de l'année d'inscription en année universitaire.
#'
#' @param annee Année d'inscription.
#' @param fichier Sans le caractère illicite "/".
#'
#' @return Année universitaire.
#' 
#' @examples 
#' 
#' apogee::annee_u(2016)
#' apogee::annee_u(2016, fichier = TRUE)
#' 
#' @export
annee_u <- function(annee, fichier = FALSE) {
  
  sep <- dplyr::case_when(fichier ~ "-", 
                          !fichier ~ "/")
  
  annee_u <- paste(annee, substr(annee + 1, 3, 4), sep = sep)
  
  return(annee_u)
}

#' Temoin TRUE/FALSE si l'ELP est une UE
#' 
#' Témoin TRUE/FALSE si l'ELP est une UE
#'
#' @param code_elp Un vecteur de code ELP.
#'
#' @return Un vecteur de booléens TRUE/FALSE.
#' 
#' @export
temoin_elp_ue <- function(code_elp) {
  
  temoin_elp_ue <- dplyr::tibble(code_elp) %>%
    dplyr::left_join(apogee::elp, by = "code_elp") %>%
    dplyr::pull(temoin_elp_ue) %>% 
    dplyr::recode("O" = TRUE, "N" = FALSE)
  
  return(temoin_elp_ue)
}

#' Liste des formations d'une annee universitaire
#' 
#' Liste des formations d'une année universitaire
#'
#' @param annee Année
#'
#' @return Un tibble des fromations
#' 
#' @export
liste_formations <- function(annee) {
  
  liste_formations <- apogee::inscrits %>% 
    dplyr::filter(annee == !!annee) %>% 
    dplyr::select(code_etape) %>% 
    unique() %>% 
    dplyr::mutate(lib_etape = apogee::lib_etape(code_etape),
                  acronyme_etape = apogee::acronyme_etape(code_etape),
                  type_diplome = apogee::hier_etape_type_diplome(code_etape) %>% apogee::acronyme_type_diplome(),
                  type_diplome = ifelse(is.na(type_diplome), apogee::hier_etape_type_diplome(code_etape), type_diplome),
                  annee_etape = apogee::annee_etape(code_etape))
  
  return(liste_formations)
}

#' Retourne l'année universitaire en cours
#' 
#' Retourne l'année universitaire en cours selon le mois défini pour les résultats
#'
#' @param mois_resultats Mois des résultats, octobre par défaut
#'
#' @return Une valeur integer contenant l'année en cours
#' 
#' @export
annee_en_cours <- function(mois_resultats = 10) {
  
  annee_en_cours <- lubridate::year(lubridate::today())
  
  if (lubridate::month(lubridate::today()) < mois_resultats) {
    annee_en_cours <- annee_en_cours - 1
  }
  
  return(annee_en_cours)
}

#' Liste des formations de l'annee en cours
#' 
#' Liste des formations de l'année en cours.
#'
#' @export
formations_en_cours <- function() {
  
  formations_en_cours <- apogee::inscrits %>% 
    dplyr::filter(annee == apogee::annee_en_cours()) %>% 
    dplyr::select(code_etape, code_composante) %>% 
    unique() %>% 
    tidyr::unnest(code_composante) %>% 
    dplyr::mutate(lib_composante = apogee::hier_composante_parent(code_composante) %>%
                    apogee::lib_composante()) %>% 
    dplyr::select(-code_composante) %>% 
    unique() %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::summarise(lib_composante = paste(lib_composante, collapse = " / ")) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(lib_etape = apogee::lib_etape(code_etape),
                  acronyme_etape = apogee::acronyme_etape(code_etape),
                  acronyme_type_diplome = apogee::hier_etape_type_diplome(code_etape) %>% apogee::acronyme_type_diplome(),
                  acronyme_type_diplome = ifelse(is.na(acronyme_type_diplome), apogee::hier_etape_type_diplome(code_etape), acronyme_type_diplome),
                  annee_etape = apogee::annee_etape(code_etape)) %>% 
    dplyr::arrange(lib_composante, acronyme_type_diplome, annee_etape, code_etape) %>% 
    dplyr::select(lib_composante, acronyme_type_diplome, annee_etape, code_etape, lib_etape, acronyme_etape) %>% 
    dplyr::left_join(apogee::etape %>% dplyr::select(code_etape, lib_etape_apogee),
                     by = "code_etape")
  
  return(formations_en_cours)
}

#' Mise a jour mensuelle des donnees Apogee (individus et meta-donnees)
#' 
#' Mise à jour mensuelle des données Apogée (individus et méta-données).
#'
#' @export
#' @keywords internal
mise_a_jour_data <- function() {
  
  # Données par individu
  apogee::data_individus()
  apogee::data_individus_diplome_origine()
  apogee::data_individus_formation_origine()
  apogee::data_inscrits()
  apogee::data_inscrits_peda()
  apogee::data_inscrits_elp()
  apogee::data_resultats_elp()
  apogee::data_resultats_etape()
  apogee::data_resultats_diplome()
  apogee::data_diplomes()
  
  # Méta-données
  apogee::data_etape()
  apogee::data_sise()
  apogee::data_diplome()
  apogee::data_composante()
  apogee::data_diplome_version()
  apogee::data_inscription()
  apogee::data_elp()
  apogee::data_resultat()
  
}
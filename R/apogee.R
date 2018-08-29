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
#' @param historique Témoin de prise en compte de l'historique des code_etape.
#'
#' @return Un vecteur contenant les années.
#'
#' Jeu de données source : \code{apogee::etape}.\cr
#'
#' @export
etape_premiere_annee <- function(code_etape, historique = FALSE) {
  
  etape_premiere_annee <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape, by = "code_etape") %>%
    dplyr::pull(annee_premiere_etape)
  
  if (historique == TRUE) {
    
    histo_etape_premiere_annee <- apogee::histo_etape_pred(code_etape) %>%
      purrr::map(apogee::etape_premiere_annee, historique = FALSE) %>%
      purrr::map_int(min)
    
    etape_premiere_annee <- ifelse(!is.na(histo_etape_premiere_annee), histo_etape_premiere_annee, etape_premiere_annee)
  }
  
  names(etape_premiere_annee) <- code_etape

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

#' Temoin TRUE/FALSE si le code_etape est actif
#' 
#' Témoin TRUE/FALSE si le code_etape est actif.
#'
#' @param code_etape Un vecteur de code_etape.
#'
#' @return Un vecteur de booléens TRUE/FALSE.
#' 
#' @export
temoin_etape_actif <- function(code_etape) {
  
  temoin_etape_actif <- dplyr::tibble(code_etape) %>% 
    dplyr::left_join(apogee::etape, by = "code_etape") %>% 
    dplyr::pull(actif)
  
  return(temoin_etape_actif)
}

#' Liste des formations d'une annee universitaire
#' 
#' Liste des formations d'une année universitaire
#'
#' @param annee Année (par défaut, celle en cours)
#'
#' @return Un tibble des formations
#' 
#' @export
formations_liste <- function(annee = NULL) {
  
  if (is.null(annee)) {
    annee <- apogee::annee_en_cours()
  }
  
  liste_formations <- apogee::etape %>% 
    dplyr::filter(actif) %>% 
    dplyr::select(-actif) %>% 
    dplyr::left_join(apogee::etape_composante, by = "code_etape") %>% 
    dplyr::filter(actif) %>% 
    dplyr::select(code_etape, code_composante) %>% 
    dplyr::mutate(lib_composante = apogee::hier_composante_parent(code_composante) %>%
                    apogee::lib_composante()) %>% 
    dplyr::select(-code_composante) %>% 
    unique() %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::summarise(lib_composante = paste(lib_composante, collapse = " / ")) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(lib_etape = apogee::lib_etape(code_etape, type_diplome = FALSE, annee = FALSE, option = FALSE, particularite = FALSE, ville = FALSE),
                  acronyme_etape = apogee::acronyme_etape(code_etape),
                  acronyme_type_diplome = apogee::hier_etape_type_diplome(code_etape) %>% apogee::acronyme_type_diplome(),
                  acronyme_type_diplome = ifelse(is.na(acronyme_type_diplome), apogee::hier_etape_type_diplome(code_etape), acronyme_type_diplome),
                  annee_etape = apogee::annee_etape(code_etape),
                  lib_mention = apogee::hier_etape_mention(code_etape) %>% 
                    purrr::map(apogee::lib_mention_diplome) %>% 
                    purrr::map_chr(caractr::str_paste, collapse = " ; ")) %>% 
    dplyr::arrange(lib_composante, acronyme_type_diplome, annee_etape, code_etape) %>% 
    dplyr::select(lib_composante, acronyme_type_diplome, annee_etape, code_etape, lib_etape, acronyme_etape, lib_mention) %>% 
    dplyr::left_join(apogee::etape %>% dplyr::select(code_etape, option, particularite, ville, lib_etape_apogee),
                     by = "code_etape") %>% 
    dplyr::mutate(lib_etape_apogee = ifelse(lib_etape_apogee, "Apogée", "OVE"))
  
  return(liste_formations)
}

#' Renvoie l'annee universitaire en cours
#' 
#' Renvoie l'année universitaire en cours selon le mois de début d'année défini
#'
#' @param mois_debut Mois de début de l'année, septembre par défaut
#'
#' @return Une valeur integer contenant l'année en cours
#' 
#' @export
annee_en_cours <- function(mois_debut = 9) {
  
  annee_en_cours <- lubridate::year(lubridate::today())
  
  if (lubridate::month(lubridate::today()) < mois_debut) {
    annee_en_cours <- annee_en_cours - 1
  }
  
  return(annee_en_cours)
}

#' Historique des formations
#' 
#' Historique des formations depuis l'année de début définie
#' 
#' @param annee_debut Année de début définie
#'
#' @export
formations_historique <- function(annee_debut) {
  
  formations_historique <- apogee::formations_liste() %>% 
    dplyr::anti_join(apogee::etape_histo, by = c("code_etape" = "code_etape_succ")) %>% 
    dplyr::mutate(annee = purrr::map2(apogee::etape_premiere_annee(code_etape), apogee::etape_derniere_annee(code_etape), ~ .x:.y)) %>% 
    dplyr::mutate(id = dplyr::row_number()) %>% 
    tidyr::unnest(annee) %>% 
    dplyr::filter(annee >= !!annee_debut) %>% 
    dplyr::mutate(lib_etape = apogee::lib_etape(code_etape)) %>% 
    dplyr::select(annee, acronyme_type_diplome, id, code_etape, lib_etape) %>% 
    tidyr::gather("champ", "valeur", code_etape, lib_etape) %>% 
    tidyr::unite(champ, annee, champ, sep = "##") %>% 
    tidyr::spread(champ, valeur) %>% 
    dplyr::select(-id) %>% 
    split(f = dplyr::pull(., acronyme_type_diplome))
  
  return(formations_historique)
}

#' Renvoie les mentions master compatibles
#'
#' Renvoie les mentions master compatibles.
#'
#' @param code_mention_diplome_l Un vecteur de codes mentions licence.
#'
#' @return Une liste contenant les mentions master compatibles.
#'
#' @export
compatibilite_mention_diplome_l <- function(code_mention_diplome_l) {
  
  compatibilite_mention_diplome_l <- dplyr::tibble(code_mention_diplome_l) %>%
    dplyr::mutate(.id = dplyr::row_number()) %>% 
    dplyr::left_join(apogee::diplome_mention_lm, by = "code_mention_diplome_l") %>%
    split(x = .$code_mention_diplome_m, f = .$.id)
  
  names(compatibilite_mention_diplome_l) <- code_mention_diplome_l
  
  return(compatibilite_mention_diplome_l)
}

#' Renvoie les mentions licence compatibles
#'
#' Renvoie les mentions licence compatibles.
#'
#' @param code_mention_diplome_m Un vecteur de code mention master.
#'
#' @return Une liste contenant les mentions licence compatibles.
#'
#' @export
compatibilite_mention_diplome_m <- function(code_mention_diplome_m) {
  
  compatibilite_mention_diplome_m <- dplyr::tibble(code_mention_diplome_m) %>%
    dplyr::mutate(.id = dplyr::row_number()) %>% 
    dplyr::left_join(apogee::diplome_mention_lm, by = "code_mention_diplome_m") %>%
    split(x = .$code_mention_diplome_l, f = .$.id)
  
  names(compatibilite_mention_diplome_m) <- code_mention_diplome_m
  
  return(compatibilite_mention_diplome_m)
}

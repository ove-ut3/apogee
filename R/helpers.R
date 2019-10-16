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

#' Renvoie l'annee de diplome a partir du code etape
#'
#' Renvoie l'année de diplôme à partir du code étape au sein d'un diplôme.
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur contenant les années de diplôme.
#'
#' Jeu de données source : \code{apogee::etape}.\cr
#' Il est créé à partir d'Apogée et de la table "etape" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
annee_diplome <- function(code_etape) {
  
  annee_diplome <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape, by = "code_etape") %>%
    dplyr::pull(annee_diplome)
  
  return(annee_diplome)
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
    
    etape_premiere_annee <- dplyr::if_else(!is.na(histo_etape_premiere_annee), histo_etape_premiere_annee, etape_premiere_annee)
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
#' @param fichier Si \code{TRUE}, utilisation du trait d'union et non du tiret insécable.
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
                          !fichier ~ "\U2011")
  
  annee_u <- paste(annee, substr(annee + 1, 3, 4), sep = sep)
  
  return(annee_u)
}

#' Temoin TRUE/FALSE si le code_etape est actif
#' 
#' Témoin TRUE/FALSE si le code_etape est actif.
#'
#' @param code_etape Un vecteur de code_etape.
#' @param annee Année(s) d'activité à vérifier, par défaut l'année en cours.
#' @param annules Si \code{TRUE}, inclut les formations dont toutes les inscriptions sont annulées.
#'
#' @return Un vecteur de booléens TRUE/FALSE.
#' 
#' @export
temoin_etape_actif <- function(code_etape, annee = NULL, annules = FALSE) {
  
  temoin_etape_actif <- dplyr::tibble(code_etape) %>% 
    dplyr::left_join(apogee::etape, by = "code_etape")
  
  if (!is.null(annee)) {
    temoin_etape_actif <- dplyr::mutate(temoin_etape_actif, actif = purrr::map2_lgl(annee_premiere_etape, annee_derniere_etape, ~ length(intersect(.x:.y, !!annee)) >= 1))
  }
  
  if (annules == FALSE) {
    temoin_etape_actif <- dplyr::mutate(temoin_etape_actif, actif = dplyr::if_else(is.na(n_inscrits), FALSE, actif))
  }
  
  temoin_etape_actif <- dplyr::pull(temoin_etape_actif, actif)
  
  return(temoin_etape_actif)
}

#' Liste des formations d'une annee universitaire
#' 
#' Liste des formations d'une année universitaire
#'
#' @param annee Année(s) (par défaut, celle en cours)
#' @param unique Si \code{TRUE}, la table est dédoublonnée par code_etape.
#'
#' @return Un tibble des formations
#' 
#' @export
formations_liste <- function(annee = NULL, unique = TRUE) {
  
  if (is.null(annee)) {
    annee <- apogee::annee_en_cours()
  }
  
  liste_formations <- apogee::etape %>% 
    tidyr::drop_na(annee_premiere_etape, annee_derniere_etape) %>% 
    dplyr::filter(purrr::map2_lgl(annee_premiere_etape, annee_derniere_etape, ~ length(intersect(.x:.y, !!annee)) >= 1),
                  !is.na(n_inscrits)) %>% 
    dplyr::left_join(apogee::etape_composante %>% 
                       dplyr::filter(purrr::map2_lgl(premiere_annee, derniere_annee, ~ length(intersect(.x:.y, !!annee)) >= 1)),
                     by = "code_etape") %>% 
    dplyr::select(code_etape, code_composante) %>% 
    dplyr::mutate(lib_composante = apogee::hier_composante_parent(code_composante) %>%
                    apogee::lib_composante()) %>% 
    dplyr::select(-code_composante) %>% 
    unique()
  
  if (unique == TRUE) {
    
    liste_formations <- liste_formations %>% 
      dplyr::group_by(code_etape) %>% 
      dplyr::summarise(lib_composante = paste(lib_composante, collapse = " / ")) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(lib_etape = apogee::lib_etape(code_etape, prefixe = NA_character_, suffixe = NA_character_),
                    acronyme_etape = apogee::acronyme_etape(code_etape),
                    acronyme_type_diplome = apogee::hier_etape_type_diplome(code_etape) %>% 
                      apogee::acronyme_type_diplome(),
                    acronyme_type_diplome = dplyr::if_else(is.na(acronyme_type_diplome), 
                                                           apogee::hier_etape_type_diplome(code_etape), 
                                                           acronyme_type_diplome),
                    annee_etape = apogee::annee_etape(code_etape),
                    lib_mention = apogee::hier_etape_mention(code_etape) %>% 
                      purrr::map(apogee::lib_mention_diplome) %>% 
                      purrr::map_chr(caractr::str_paste, collapse = " ; "),
                    lib_domaine = apogee::hier_etape_domaine(code_etape) %>% 
                      purrr::map(apogee::lib_domaine_diplome) %>% 
                      purrr::map_chr(caractr::str_paste, collapse = " ; "))
    
  } else {
    
    liste_formations <- liste_formations %>% 
      dplyr::mutate(lib_etape = apogee::lib_etape(code_etape, prefixe = NA_character_, suffixe = NA_character_),
                    acronyme_etape = apogee::acronyme_etape(code_etape),
                    acronyme_type_diplome = apogee::hier_etape_type_diplome(code_etape) %>% 
                      apogee::acronyme_type_diplome(),
                    acronyme_type_diplome = dplyr::if_else(is.na(acronyme_type_diplome), 
                                                           apogee::hier_etape_type_diplome(code_etape), 
                                                           acronyme_type_diplome),
                    annee_etape = apogee::annee_etape(code_etape),
                    lib_mention = apogee::hier_etape_mention(code_etape) %>% 
                      purrr::map(apogee::lib_mention_diplome)) %>% 
      tidyr::unnest_legacy() %>% 
      dplyr::mutate(lib_domaine = apogee::hier_etape_domaine(code_etape) %>% 
                      purrr::map(apogee::lib_domaine_diplome)) %>% 
      tidyr::unnest_legacy()
    
  }
  
  liste_formations <- liste_formations %>% 
    dplyr::left_join(apogee::etape_secteur, by = "code_etape") %>% 
    dplyr::arrange(lib_composante, acronyme_type_diplome, annee_etape, code_etape) %>% 
    dplyr::select(lib_composante, acronyme_type_diplome, annee_etape, code_etape, lib_etape, acronyme_etape, lib_mention, lib_domaine, secteur) %>% 
    dplyr::left_join(apogee::etape %>% dplyr::select(code_etape, option, particularite, ville, temoin_etape_apogee),
                     by = "code_etape") %>% 
    dplyr::mutate(temoin_etape_apogee = dplyr::if_else(temoin_etape_apogee, "Apogée", "OVE"))
  
  return(liste_formations)
}

#' Renvoie l'annee universitaire en cours
#' 
#' Renvoie l'année universitaire en cours selon la dernière année observée dans Apogée.
#'
#' @return Une valeur integer contenant l'année en cours
#' 
#' @export
annee_en_cours <- function() {
  
  annee_en_cours <- apogee::etape %>% 
    dplyr::pull(annee_derniere_etape) %>% 
    max(na.rm = TRUE)
  
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
    tidyr::unnest_legacy(annee) %>% 
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

#' Rezippe les archives CSV extraites depuis BO.
#'
#' @param fichier_zip Chemin vers le fichier zip.
#'
#' @export
rezip_csv <- function(fichier_zip) {
  
  match <- stringr::str_match(fichier_zip, "(.+)(/.+?)$")
  path <- match[, 2]
  file <- match[, 3]
  unzip(zipfile = glue::glue("{path}{file}"), exdir = path)
  
  csv_file <- list.files(path, pattern = "csv$", full.names = TRUE)
  
  zip(glue::glue("{path}{file}"), csv_file, flags = "-jqr")
  
  suppression <- file.remove(csv_file)
}

#' Temoin TRUE/FALSE si le code_mention_diplome est actif
#' 
#' Témoin TRUE/FALSE si le code_mention_diplome est actif.
#'
#' @param code_mention_diplome Un vecteur de code_mention_diplome.
#' @param annee Année(s) d'activité à vérifier, par défaut l'année en cours.
#'
#' @return Un vecteur de booléens TRUE/FALSE.
#' 
#' @export
temoin_mention_actif <- function(code_mention_diplome, annee = NULL) {
  
  temoin_mention_actif <- dplyr::tibble(code_mention_diplome) %>% 
    dplyr::left_join(apogee::diplome_mention, by = "code_mention_diplome")
  
  if (!is.null(annee)) {
    temoin_mention_actif <- dplyr::mutate(temoin_mention_actif, actif = purrr::map2_lgl(mention_premiere_annee, mention_derniere_annee, ~ length(intersect(.x:.y, !!annee)) >= 1))
  }
  
  temoin_mention_actif <- dplyr::pull(temoin_mention_actif, actif)
  
  return(temoin_mention_actif)
}

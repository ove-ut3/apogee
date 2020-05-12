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
    dplyr::left_join(
      apogee::etape, 
      by = "code_etape"
    ) %>%
    dplyr::pull(.data$annee_etape)

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
    dplyr::left_join(
      apogee::etape, 
      by = "code_etape"
    ) %>%
    dplyr::pull(.data$annee_diplome)

  return(annee_diplome)
}

#' Renvoie les annees d'activite d'une formation a partir du code etape
#'
#' Renvoie les années d'activité d'une formation à partir du code étape.
#'
#' @param code_etape Un vecteur de code étape.
#' @param historique Témoin de prise en compte de l'historique des code_etape.
#'
#' @return Une liste contenant les années d'activité.
#'
#' Jeu de données source : \code{apogee::etape}.\cr
#'
#' @export
etape_annees_activite <- function(code_etape, historique = FALSE) {
  
  etape_annees_activite <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(
      apogee::etape, 
      by = "code_etape"
    )

  if (historique == FALSE) {
    
    etape_annees_activite <- etape_annees_activite$annees_activite
    
  } else {
    
    histo_annee_premiere_etape <- dplyr::tibble(code_etape) %>% 
      dplyr::mutate(.id = dplyr::row_number())
    
    etape_annees_activite <- histo_annee_premiere_etape %>% 
      dplyr::bind_rows(
        histo_annee_premiere_etape %>% 
          dplyr::mutate_at("code_etape", apogee::histo_etape_pred, predecesseur_final = TRUE) %>% 
          tidyr::unnest(.data$code_etape) 
      ) %>% 
      dplyr::left_join(
        apogee::etape, 
        by = "code_etape"
      ) %>% 
      tidyr::unnest(.data$annees_activite) %>% 
      dplyr::count(.data$.id, .data$annees_activite) %>% 
      split(x = .$annees_activite, f = .$.id) %>% 
      unname()

  }
  
  return(etape_annees_activite)
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
#' @export
annee_u <- function(annee, fichier = FALSE) {
  
  sep <- dplyr::case_when(
    fichier ~ "-",
    !fichier ~ "\U2011"
  )

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
    dplyr::left_join(
      apogee::etape, 
      by = "code_etape"
    )

  if (!is.null(annee)) {
    temoin_etape_actif <- dplyr::mutate(temoin_etape_actif, actif = purrr::map_lgl(.data$annees_activite, ~ length(intersect(., !!annee)) >= 1))
  }

  if (annules == FALSE) {
    temoin_etape_actif <- dplyr::mutate(temoin_etape_actif, actif = dplyr::if_else(is.na(.data$n_inscrits), FALSE, .data$actif))
  }

  temoin_etape_actif <- dplyr::pull(temoin_etape_actif, .data$actif)

  return(temoin_etape_actif)
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
    tidyr::unnest(.data$annees_activite) %>% 
    dplyr::pull(.data$annees_activite) %>%
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
  
  formations_historique <- apogee::etape %>% 
    dplyr::filter(.data$actif) %>% 
    dplyr::anti_join(
      apogee::etape_histo, 
      by = c("code_etape" = "code_etape_succ")
    ) %>%
    dplyr::mutate(annee = etape_annees_activite(.data$code_etape)) %>%
    dplyr::mutate(.id = dplyr::row_number()) %>%
    tidyr::unnest(.data$annee) %>%
    dplyr::filter(.data$annee >= !!annee_debut) %>%
    dplyr::mutate(
      lib_etape = apogee::lib_etape(.data$code_etape),
      acronyme_type_diplome = apogee::acronyme_type_diplome(.data$code_type_diplome)
    ) %>%
    dplyr::select(.data$annee, .data$acronyme_type_diplome, .data$.id, .data$code_etape, .data$lib_etape) %>%
    tidyr::gather("champ", "valeur", .data$code_etape, .data$lib_etape) %>%
    tidyr::unite("champ", .data$annee, .data$champ, sep = "##") %>%
    tidyr::spread(.data$champ, .data$valeur) %>%
    dplyr::select(-.data$.id) %>%
    split(f = dplyr::pull(., .data$acronyme_type_diplome))

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
    dplyr::left_join(
      apogee::mention_diplome_lm, 
      by = "code_mention_diplome_l"
    ) %>%
    split(x = .$code_mention_diplome_m, f = .$.id) %>% 
    unname()

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
    dplyr::left_join(
      apogee::mention_diplome_lm, 
      by = "code_mention_diplome_m"
    ) %>%
    split(x = .$code_mention_diplome_l, f = .$.id) %>% 
    unname()

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
  utils::unzip(zipfile = glue::glue("{path}{file}"), exdir = path)

  csv_file <- list.files(path, pattern = "csv$", full.names = TRUE)

  utils::zip(glue::glue("{path}{file}"), csv_file, flags = "-jqr")

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
    dplyr::left_join(
      apogee::mention_diplome, 
      by = "code_mention_diplome"
    )

  if (!is.null(annee)) {
    temoin_mention_actif <- dplyr::mutate(temoin_mention_actif, actif = purrr::map2_lgl(.data$mention_premiere_annee, .data$mention_derniere_annee, ~ length(intersect(.x:.y, !!annee)) >= 1))
  }

  temoin_mention_actif <- dplyr::pull(temoin_mention_actif, .data$actif)

  return(temoin_mention_actif)
}

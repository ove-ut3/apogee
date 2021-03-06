#' Renvoie le code etape successeur
#'
#' Renvoie le code étape successeur.
#'
#' @param code_etape Un vecteur de code étape.
#' @param code_elp Un vecteur de code ELP, si c'est un parcours spécifique qui s'est ensuite transformé en étape.
#' @param multiple \code{FALSE} par défaut, renvoie les successeurs sous forme de vecteurs. Si les successeurs sont multiples pour un même code_etape saisi, alors un warning est lancé pour diriger l'utilisateur vers la valeur \code{TRUE}. Celle-ci renvoie tous les successeurs mais sous forme de liste.
#' @param successeur_final \code{TRUE}, renvoit le successeur le plus récent dans l'historique; \code{FALSE}, renvoie le premier successeur.
#' @param garder_na \code{TRUE}, les codes sans successeur passent à \code{NA}; \code{FALSE}, les codes sans successeur sont gardés tels quels.
#'
#' @return Un vecteur de code étape successeur.
#'
#' Jeu de données source : \code{apogee::etape_histo}.\cr
#' Il est créé à partir d'Apogée et de la table "etape_histo" de la base Access Tables_ref (projet Apogee).
#'
#' @export
histo_etape_succ <- function(code_etape, code_elp = NULL, multiple = FALSE, successeur_final = TRUE, garder_na = FALSE) {
  
  if (multiple) {
    
    histo_etape_succ <- histo_etape_succ_all(code_etape = code_etape, code_elp = code_elp, successeur_final = successeur_final, garder_na = garder_na)
    
  } else {
    
    code_etape_mutliples <- dplyr::intersect(
      code_etape,
      dplyr::filter(apogee::etape_histo, .data$doublon == "\u00e9clatement") %>% 
        dplyr::pull(.data$code_etape)
    )
    
    if (is.null(code_elp)) {
      
      histo_etape_succ <- dplyr::tibble(code_etape) %>%
        dplyr::left_join(
          dplyr::filter(apogee::etape_histo, is.na(.data$doublon)),
          by = "code_etape"
        ) %>%
        dplyr::pull(.data$code_etape_succ)
      
    } else if (!is.null(code_elp)) {
      
      if (length(code_etape) != length(code_elp)) {
        
        stop("Les vecteurs de code_etape et code_elp doivent \u00eatre de taille identique.", call. = FALSE)
        
      }
      
      histo_etape_succ <- dplyr::tibble(
        code_etape,
        code_elp
      ) %>%
        dplyr::left_join(
          apogee::etape_histo %>% 
            dplyr::filter(is.na(.data$doublon_elp)),
          by = c("code_etape", "code_elp")
        ) %>%
        dplyr::pull(.data$code_etape_succ)
      
    }
    
    if (garder_na == FALSE) {
      
      histo_etape_succ <- dplyr::if_else(is.na(histo_etape_succ), code_etape, histo_etape_succ)
      
    }
    
    if (successeur_final == TRUE) {
      
      if (any(!is.na(apogee::histo_etape_succ(histo_etape_succ, successeur_final = FALSE, garder_na = TRUE)))) {
        
        histo_etape_succ <- Recall(histo_etape_succ, successeur_final = successeur_final, garder_na = garder_na)
      }
      
    }
    
  }
  
  return(histo_etape_succ)
}

#' Renvoie le code etape successeur (avec prise en charge de l'eclatement)
#'
#' Renvoie le code étape successeur (avec prise en charge de l'éclatement).
#'
#' @param code_etape Un vecteur de code étape.
#' @param code_elp Un vecteur de code ELP, si c'est un parcours spécifique qui s'est ensuite transformé en étape.
#' @param successeur_final \code{TRUE}, renvoit le successeur le plus récent dans l'historique; \code{FALSE}, renvoie le premier successeur.
#' @param garder_na \code{TRUE}, les codes sans successeur passent à \code{NA}; \code{FALSE}, les codes sans successeur sont gardés tels quels.
#'
#' @return Une liste de code étape successeur.
#'
#' Jeu de données source : \code{apogee::etape_histo}.\cr
#' Il est créé à partir d'Apogée et de la table "etape_histo" de la base Access Tables_ref (projet Apogee).
#' @keywords internal
histo_etape_succ_all <- function(code_etape, code_elp = NULL, successeur_final = TRUE, garder_na = FALSE) {

  if (is.null(code_elp)) {
    
    histo_etape_succ_all <- dplyr::tibble(code_etape) %>%
      dplyr::mutate(.id = dplyr::row_number()) %>%
      dplyr::left_join(
        apogee::etape_histo %>% 
          dplyr::filter(is.na(.data$doublon)),
        by = "code_etape"
      )
    
  } else if (!is.null(code_elp)) {
    
    if (length(code_etape) != length(code_elp)) {
      
      stop("Les vecteurs de code_etape et code_elp doivent \u00eatre de taille identique.", call. = FALSE)
      
    }
    
    histo_etape_succ_all <- dplyr::tibble(
      code_etape,
      code_elp
    ) %>%
      dplyr::mutate(.id = dplyr::row_number()) %>%
      dplyr::left_join(
        apogee::etape_histo %>% 
          dplyr::filter(is.na(.data$doublon_elp)),
        by = c("code_etape", "code_elp")
      )
    
  }

  if (garder_na == FALSE) {
    
    histo_etape_succ_all <- histo_etape_succ_all %>%
      dplyr::mutate(code_etape_succ = dplyr::if_else(is.na(.data$code_etape_succ), .data$code_etape, .data$code_etape_succ))
    
  }

  if (successeur_final == TRUE) {
    
    histo_etape_succ_all <- histo_etape_succ_all %>%
      dplyr::select(.data$.id, code_etape = .data$code_etape_succ) %>%
      dplyr::left_join(
        dplyr::filter(apogee::etape_histo, is.na(.data$code_elp)),
        by = "code_etape"
      )

    while (any(!is.na(histo_etape_succ_all$code_etape_succ))) {
      
      if (garder_na == FALSE) {
        histo_etape_succ_all <- histo_etape_succ_all %>%
          dplyr::mutate(code_etape_succ = dplyr::if_else(is.na(.data$code_etape_succ), .data$code_etape, .data$code_etape_succ))
      }

      histo_etape_succ_all <- histo_etape_succ_all %>%
        dplyr::select(.data$.id, code_etape = .data$code_etape_succ) %>%
        dplyr::left_join(
          apogee::etape_histo %>% 
            dplyr::filter(is.na(.data$code_elp)),
          by = "code_etape"
        )
      
    }
    
  }

  if (garder_na == FALSE) {
    histo_etape_succ_all <- histo_etape_succ_all %>%
      dplyr::mutate(code_etape_succ = dplyr::if_else(is.na(.data$code_etape_succ), .data$code_etape, .data$code_etape_succ))
  }

  histo_etape_succ_all <- histo_etape_succ_all %>%
    split(x = .$code_etape_succ, f = .$.id) %>% 
    unname()

  return(histo_etape_succ_all)
}

#' Renvoie les codes etape predecesseurs
#'
#' Renvoie les codes étape prédécesseurs.
#'
#' @param code_etape Un vecteur de code étape.
#' @param annee Année universitaire (2014 pour 2014/15) jusqu'à laquelle stopper l'historique.
#' @param predecesseur_final \code{TRUE}, renvoit le prédécesseur le plus ancien dans l'historique; \code{FALSE}, renvoie le premier prédécesseur.
#' @param garder_na \code{TRUE}, les codes sans prédécesseur passent à \code{NA}; \code{FALSE}, les codes sans successeur sont gardés tels quels.
#'
#' @return Une liste de vecteurs de code étape prédécesseur.
#'
#' Jeu de données source : \code{apogee::etape_histo}.\cr
#' Il est créé à partir d'Apogée et de la table "etape_historique" de la base Access Tables_ref (projet Apogee).
#'
#' @export
histo_etape_pred <- function(code_etape, annee = NULL, predecesseur_final = FALSE, garder_na = FALSE) {
  
  etape_histo <- apogee::etape_histo %>% 
    dplyr::select(code_etape = .data$code_etape_succ, code_etape_pred = .data$code_etape)

  if (!is.null(annee)) {
    
    predecesseur_final <- TRUE

    etape_histo <- etape_histo %>%
      dplyr::semi_join(
        apogee::etape %>% 
          dplyr::mutate(annee_derniere_etape = purrr::map_int(.data$annees_activite, utils::tail, 1)) %>% 
          dplyr::filter(.data$annee_derniere_etape >= !!annee),
        by = c("code_etape_pred" = "code_etape")
      )
    
  }

  histo_etape_pred <- dplyr::tibble(code_etape) %>%
    dplyr::mutate(.id = dplyr::row_number()) %>%
    dplyr::left_join(
      etape_histo, 
      by = "code_etape"
    )

  if (garder_na == FALSE) {
    
    histo_etape_pred <- histo_etape_pred %>%
      dplyr::mutate(code_etape_pred = dplyr::if_else(is.na(.data$code_etape_pred), .data$code_etape, .data$code_etape_pred))
    
  }

  if (predecesseur_final == TRUE) {
    
    histo_etape_pred <- histo_etape_pred %>%
      dplyr::select(.data$.id, code_etape = .data$code_etape_pred) %>%
      dplyr::left_join(
        etape_histo, 
        by = "code_etape"
      )

    while (any(!is.na(histo_etape_pred$code_etape_pred))) {
      
      if (garder_na == FALSE) {
        histo_etape_pred <- histo_etape_pred %>%
          dplyr::mutate(code_etape_pred = dplyr::if_else(is.na(.data$code_etape_pred), .data$code_etape, .data$code_etape_pred))
      }

      histo_etape_pred <- histo_etape_pred %>%
        dplyr::select(.data$.id, code_etape = .data$code_etape_pred) %>%
        dplyr::left_join(
          etape_histo, 
          by = "code_etape"
        )
      
    }
    
  }

  if (garder_na == FALSE) {
    
    histo_etape_pred <- histo_etape_pred %>%
      dplyr::mutate(code_etape_pred = dplyr::if_else(is.na(.data$code_etape_pred), .data$code_etape, .data$code_etape_pred))
    
  }

  histo_etape_pred <- histo_etape_pred %>%
    split(x = .$code_etape_pred, f = .$.id) %>% 
    unname()

  return(histo_etape_pred)
}

#' Renvoie le code ELP successeur
#'
#' Renvoie le code ELP successeur.
#'
#' @param code_elp Un vecteur de code ELP.
#' @param successeur_final \code{TRUE}, renvoit le successeur le plus récent dans l'historique; \code{FALSE}, renvoie le premier successeur.
#' @param garder_na \code{TRUE}, les codes sans successeur passent à \code{NA}; \code{FALSE}, les codes sans successeur sont gardés tels quels.
#'
#' @return Un vecteur de code ELP successeur.
#'
#' Jeu de données source : \code{apogee::elp_histo}.\cr
#' Il est créé à partir d'Apogée et de la table "elp_histo" de la base Access Tables_ref (projet Apogee).
#'
#' @export
histo_elp_succ <- function(code_elp, successeur_final = TRUE, garder_na = FALSE) {
  
  histo_elp_succ <- dplyr::tibble(code_elp) %>%
    dplyr::left_join(
      apogee::elp_histo, 
      by = "code_elp"
    ) %>%
    dplyr::pull(.data$code_elp_succ)

  if (garder_na == FALSE) {
    
    histo_elp_succ <- dplyr::if_else(is.na(histo_elp_succ), code_elp, histo_elp_succ)
    
  }

  if (successeur_final == TRUE) {
    
    if (any(!is.na(apogee::histo_elp_succ(histo_elp_succ, successeur_final = FALSE, garder_na = TRUE)))) {
      
      histo_elp_succ <- Recall(histo_elp_succ, successeur_final = successeur_final, garder_na = garder_na)
      
    }
    
  }

  return(histo_elp_succ)
}

#' Renvoie le code de mention de diplome successeur
#'
#' Renvoie le code de mention de diplôme successeur.
#'
#' @param code_mention_diplome Un vecteur de code de mention de diplôme.
#' @param successeur_final \code{TRUE}, renvoit le successeur le plus récent dans l'historique; \code{FALSE}, renvoie le premier successeur.
#' @param garder_na \code{TRUE}, les codes sans successeur passent à \code{NA}; \code{FALSE}, les codes sans successeur sont gardés tels quels.
#'
#' @return Un vecteur de code de mention de diplôme successeur.
#'
#' Jeu de données source : \code{apogee::mention_diplome_histo}.\cr
#' Il est créé à partir d'Apogée et de la table "mention_diplome_histo" de la base Access Tables_ref (projet Apogee).
#'
#' @export
histo_mention_diplome_succ <- function(code_mention_diplome, successeur_final = TRUE, garder_na = FALSE) {
  
  histo_mention_diplome_succ <- dplyr::tibble(code_mention_diplome) %>%
    dplyr::left_join(
      apogee::mention_diplome_histo, 
      by = "code_mention_diplome"
    ) %>%
    dplyr::pull(.data$code_mention_diplome_succ)

  if (garder_na == FALSE) {
    
    histo_mention_diplome_succ <- dplyr::if_else(is.na(histo_mention_diplome_succ), code_mention_diplome, histo_mention_diplome_succ)
    
  }

  if (successeur_final == TRUE) {
    
    if (any(!is.na(apogee::histo_mention_diplome_succ(histo_mention_diplome_succ, successeur_final = FALSE, garder_na = TRUE)))) {
      
      histo_mention_diplome_succ <- Recall(histo_mention_diplome_succ, successeur_final = successeur_final, garder_na = garder_na)
      
    }
    
  }

  return(histo_mention_diplome_succ)
}

#' Renvoie le code de composante successeur
#'
#' Renvoie le code de composante successeur.
#'
#' @param code_composante Un vecteur de code de composante.
#' @param successeur_final \code{TRUE}, renvoit le successeur le plus récent dans l'historique; \code{FALSE}, renvoie le premier successeur.
#' @param garder_na \code{TRUE}, les codes sans successeur passent à \code{NA}; \code{FALSE}, les codes sans successeur sont gardés tels quels.
#'
#' @return Un vecteur de code de composante successeur.
#'
#' Jeu de données source : \code{apogee::composante_histo}.\cr
#' Il est créé à partir d'Apogée et de la table "composante_histo" de la base Access Tables_ref (projet Apogee).
#'
#' @export
histo_composante_succ <- function(code_composante, successeur_final = TRUE, garder_na = FALSE) {
  
  histo_composante_succ <- dplyr::tibble(code_composante) %>%
    dplyr::left_join(
      apogee::composante_histo, 
      by = "code_composante"
    ) %>%
    dplyr::pull(.data$code_composante_succ)

  if (garder_na == FALSE) {
    
    histo_composante_succ <- dplyr::if_else(is.na(histo_composante_succ), code_composante, histo_composante_succ)
    
  }

  if (successeur_final == TRUE) {
    
    if (any(!is.na(apogee::histo_composante_succ(histo_composante_succ, successeur_final = FALSE, garder_na = TRUE)))) {
      
      histo_composante_succ <- Recall(histo_composante_succ, successeur_final = successeur_final, garder_na = garder_na)
      
    }
    
  }

  return(histo_composante_succ)
}

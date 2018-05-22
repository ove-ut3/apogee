#' Renvoie le code etape successeur
#'
#' Renvoie le code étape successeur.
#'
#' @param code_etape Un vecteur de code étape.
#' @param code_elp Un vecteur de code ELP, si c'est un parcours spécifique qui s'est ensuite transformé en étape.
#' @param successeur_final \code{TRUE}, renvoit le successeur le plus récent dans l'historique; \code{FALSE}, renvoie le premier successeur.
#' @param garder_na \code{TRUE}, les codes sans successeur passent à \code{NA}; \code{FALSE}, les codes sans successeur sont gardés tels quels.
#'
#' @return Un vecteur de code étape successeur.
#'
#' Jeu de données source : \code{apogee::etape_histo}.\cr
#' Il est créé à partir d'Apogée et de la table "etape_histo" de la base Access Tables_ref (projet Apogee).
#'
#' @export
histo_etape_succ <- function(code_etape, code_elp = NULL, successeur_final = TRUE, garder_na = FALSE) {
  
  if (is.null(code_elp)) {
    histo_etape_succ <- dplyr::tibble(code_etape) %>%
      dplyr::left_join(dplyr::filter(apogee::etape_histo, is.na(doublon)),
                       by = "code_etape") %>%
      dplyr::pull(code_etape_succ)
    
  } else if (!is.null(code_elp)) {
    
    if (length(code_etape) != length(code_elp)) {
      stop("Les vecteurs de code_etape et code_elp doivent être de taille identique.", call. = FALSE)
    }
    
    histo_etape_succ <- dplyr::tibble(code_etape,
                                       code_elp) %>%
      dplyr::left_join(dplyr::filter(apogee::etape_histo, is.na(doublon_elp)),
                       by = c("code_etape", "code_elp")) %>%
      dplyr::pull(code_etape_succ)
  }
  
  if (garder_na == FALSE) {
    histo_etape_succ <- ifelse(is.na(histo_etape_succ), code_etape, histo_etape_succ)
  }
  
  if (successeur_final == TRUE) {
    if (any(!is.na(apogee::histo_etape_succ(histo_etape_succ, successeur_final = FALSE, garder_na = TRUE)))) {
      histo_etape_succ <- Recall(histo_etape_succ, successeur_final = successeur_final, garder_na = garder_na)
    }
  }
  
  return(histo_etape_succ)
}

#' Renvoie le code etape successeur (avec prise en charge de l'éclatement)
#'
#' Renvoie le code étape successeur (avec prise en charge de l'éclatement).
#'
#' @param code_etape Un vecteur de code étape.
#' @param successeur_final \code{TRUE}, renvoit le successeur le plus récent dans l'historique; \code{FALSE}, renvoie le premier successeur.
#' @param garder_na \code{TRUE}, les codes sans successeur passent à \code{NA}; \code{FALSE}, les codes sans successeur sont gardés tels quels.
#'
#' @return Une liste de code étape successeur.
#'
#' Jeu de données source : \code{apogee::etape_histo}.\cr
#' Il est créé à partir d'Apogée et de la table "etape_histo" de la base Access Tables_ref (projet Apogee).
#'
#' @export
histo_etape_succ_2 <- function(code_etape, successeur_final = TRUE, garder_na = FALSE) {
  
  histo_etape_succ_2 <- dplyr::tibble(code_etape) %>% 
    dplyr::mutate(.id = dplyr::row_number()) %>% 
    dplyr::left_join(apogee::etape_histo, by = "code_etape")
  
  if (garder_na == FALSE) {
    histo_etape_succ_2 <- histo_etape_succ_2 %>% 
      dplyr::mutate(code_etape_succ = ifelse(is.na(code_etape_succ), code_etape, code_etape_succ))
  }
  
  if (successeur_final == TRUE) {
    
    histo_etape_succ_2 <- histo_etape_succ_2 %>% 
      dplyr::select(.id, code_etape = code_etape_succ) %>% 
      dplyr::left_join(apogee::etape_histo, by = "code_etape")
    
    while (any(!is.na(histo_etape_succ_2$code_etape_succ))) {
      
      if (garder_na == FALSE) {
        histo_etape_succ_2 <- histo_etape_succ_2 %>% 
          dplyr::mutate(code_etape_succ = ifelse(is.na(code_etape_succ), code_etape, code_etape_succ))
      }
      
      histo_etape_succ_2 <- histo_etape_succ_2 %>% 
        dplyr::select(.id, code_etape = code_etape_succ) %>% 
        dplyr::left_join(apogee::etape_histo, by = "code_etape")

    }
    
  }
  
  if (garder_na == FALSE) {
    histo_etape_succ_2 <- histo_etape_succ_2 %>% 
      dplyr::mutate(code_etape_succ = ifelse(is.na(code_etape_succ), code_etape, code_etape_succ))
  }
  
  histo_etape_succ_2 <- histo_etape_succ_2 %>% 
    split(x = .$code_etape_succ, f = .$.id)
  
  names(histo_etape_succ_2) <- code_etape
  
  return(histo_etape_succ_2)
}

#' Renvoie les codes etape predecesseurs
#'
#' Renvoie les codes étape prédécesseurs.
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Une liste de vecteurs de code étape prédécesseur.
#'
#' Jeu de données source : \code{apogee::etape_histo}.\cr
#' Il est créé à partir d'Apogée et de la table "etape_historique" de la base Access Tables_ref (projet Apogee).
#'
#' @export
histo_etape_pred <- function(code_etape) {
  
  histo_etape_pred <- dplyr::tibble(code_etape_succ = code_etape) %>%
    dplyr::left_join(apogee::etape_histo, by = "code_etape_succ") %>%
    split(x = .$code_etape, f = .$code_etape_succ)
  
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
    dplyr::left_join(apogee::elp_histo, by = "code_elp") %>%
    dplyr::pull(code_elp_succ)
  
  if (garder_na == FALSE) {
    histo_elp_succ <- ifelse(is.na(histo_elp_succ), code_elp, histo_elp_succ)
  }
  
  if (successeur_final == TRUE) {
    if (any(!is.na(apogee::histo_elp_succ(histo_elp_succ, successeur_final = FALSE, garder_na = TRUE)))) {
      histo_elp_succ <- Recall(histo_elp_succ, successeur_final = successeur_final, garder_na = garder_na)
    }
  }
  
  return(histo_elp_succ)
}

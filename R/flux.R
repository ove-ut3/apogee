#' Renvoie le code etape successeur
#'
#' Renvoie le code étape successeur.
#'
#' @param code_etape Un vecteur de code étape.
#' @param successeur_final \code{TRUE}, renvoit le successeur le plus récent dans l'historique; \code{FALSE}, renvoie le premier successeur.
#' @param garder_na \code{TRUE}, les codes sans successeur passent à \code{NA}; \code{FALSE}, les codes sans successeur sont gardés tels quels.
#'
#' @return Un vecteur de code étape successeur.
#'
#' Jeu de données source : \code{apogee::etape_flux}.\cr
#' Il est créé à partir d'Apogée et de la table "etape_flux" de la base Access Tables_ref (projet Apogee).
#'
#' @export
flux_etape_succ <- function(code_etape, successeur_final = FALSE, garder_na = FALSE) {
  
  flux_etape_succ <- tibble::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape_flux, by = "code_etape") %>%
    dplyr::pull(code_etape_flux)
  
  if (garder_na == FALSE) {
    flux_etape_succ <- ifelse(is.na(flux_etape_succ), code_etape, flux_etape_succ)
  }
  
  if (successeur_final == TRUE) {
    if (any(!is.na(apogee::flux_etape_succ(flux_etape_succ, successeur_final = FALSE, garder_na = TRUE)))) {
      flux_etape_succ <- Recall(flux_etape_succ, successeur_final = successeur_final, garder_na = garder_na)
    }
  }
  
  return(flux_etape_succ)
}

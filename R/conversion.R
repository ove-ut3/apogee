#' Renvoie le code de diplome SISE a partir du code etape
#'
#' Renvoie le code de diplôme SISE à partir du code étape.
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur contenant les codes de diplôme SISE.
#'
#' Jeu de données source : \code{apogee::etape_sise}.\cr
#' Il est créé à partir d'Apogée.
#'
#' @export
conv_etape_sise_diplome <- function(code_etape, annee) {
  conv_etape_sise_diplome <- dplyr::tibble(code_etape, annee) %>%
    dplyr::mutate(.id = dplyr::row_number()) %>%
    dplyr::left_join(apogee::etape_sise, by = c("code_etape", "annee")) %>%
    split(x = .$code_diplome_sise, f = .$.id) %>% 
    unname()

  return(conv_etape_sise_diplome)
}

#' Renvoie le code de finalite diplome a partir du code etape
#'
#' Renvoie le code de finalité diplôme à partir du code étape.
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur contenant les codes de finalité diplôme.
#'
#' Jeu de données source : \code{apogee::etape_finalite}.\cr
#'
#' @export
conv_etape_finalite_diplome <- function(code_etape) {
  conv_etape_finalite_diplome <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape_finalite, by = "code_etape") %>%
    dplyr::pull(code_finalite_diplome)

  return(conv_etape_finalite_diplome)
}

#' Renvoie le code de periode d'enseignement a partir du code ELP
#'
#' Renvoie le code de période d'enseignement à partir du code ELP.
#'
#' @param code_elp Un vecteur de code ELP.
#'
#' @return Un vecteur contenant les codes de période d'enseignement.
#'
#' Jeu de données source : \code{apogee::elp}.\cr
#'
#' @export
conv_elp_periode <- function(code_elp) {
  conv_elp_periode <- dplyr::tibble(code_elp) %>%
    dplyr::left_join(apogee::elp, by = "code_elp") %>%
    dplyr::pull(code_periode_elp)

  return(conv_elp_periode)
}

#' Renvoie le type de diplome a partir de l'annee d'etape
#'
#' Renvoie le type de diplôme à partir de l'année d'étape.
#'
#' @param code_elp Un vecteur d'année d'étape.
#'
#' @return Un vecteur contenant les acronymes de type de diplôme.
#'
#' Jeu de données source : \code{apogee::diplome_type}.\cr
#'
#' @export
conv_annee_etape_type_diplome <- function(annee_etape) {
  conv_annee_etape_type_diplome <- dplyr::tibble(annee_etape) %>%
    dplyr::left_join(apogee::diplome_type, by = "annee_etape") %>%
    dplyr::pull(acronyme_type_diplome)

  return(conv_annee_etape_type_diplome)
}

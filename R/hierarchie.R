#' Renvoie le type de diplome a partir du code etape
#'
#' Renvoie le type de diplôme à partir du code étape.
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur contenant les types de diplôme.
#'
#' @export
hier_etape_type_diplome <- function(code_etape) {
  
  hier_etape_type_diplome <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape, by = "code_etape") %>%
    dplyr::pull(code_type_diplome)

  return(hier_etape_type_diplome)
}

#' Renvoie la filiere a partir du code etape
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur contenant les filières.
#'
#' @export
hier_etape_filiere <- function(code_etape) {
  
  if (is.null(code_etape)) {
    return(NA_character_)
  }
  
  hier_etape_filiere <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape, by = "code_etape") %>%
    dplyr::pull(filiere)
  
  return(hier_etape_filiere)
}

#' Renvoie la nature ELP a partir du code
#'
#' @param code_elp Un vecteur de code ELP.
#'
#' @return Un vecteur contenant les natures ELP.
#'
#' @export
hier_elp_nature <- function(code_elp) {
  
  hier_elp_nature <- dplyr::tibble(code_elp) %>%
    dplyr::left_join(apogee::elp, by = "code_elp") %>%
    dplyr::pull(code_nature_elp)
  
  return(hier_elp_nature)
}

#' Renvoie le code discipline SISE parent
#'
#' Renvoie le code discipline SISE parent.
#'
#' @param code_discipline_sise Un vecteur de code discipline SISE.
#' @param parent_final \code{TRUE}, renvoit le parent le plus haut dans la hiérarchie; \code{FALSE}, renvoie le premier parent.
#' @param garder_na \code{TRUE}, les codes sans parent passent à \code{NA}; \code{FALSE}, les codes sans parent sont gardés tels quels.
#'
#' @return Un vecteur de code discipline SISE parent.
#'
#' Jeu de données source : \code{apogee::sise_discipline}.\cr
#' Il est créé à partir de la table "sise_discipline" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
hier_discipline_sise_parent <- function(code_discipline_sise, parent_final = FALSE, garder_na = FALSE) {
  
  hier_discipline_sise_parent <- dplyr::tibble(code_discipline_sise) %>%
    dplyr::left_join(apogee::sise_discipline, by = "code_discipline_sise") %>%
    dplyr::pull(code_discipline_sise_parent)
  
  if (garder_na == FALSE) {
    hier_discipline_sise_parent <- dplyr::if_else(is.na(hier_discipline_sise_parent), code_discipline_sise, hier_discipline_sise_parent)
  }
  
  if (parent_final == TRUE) {
    if (any(!is.na(apogee::hier_discipline_sise_parent(hier_discipline_sise_parent, parent_final = FALSE, garder_na = TRUE)))) {
      hier_discipline_sise_parent <- Recall(hier_discipline_sise_parent, parent_final = parent_final, garder_na = garder_na)
    }
  }
  
  return(hier_discipline_sise_parent)
}

#' Renvoie le code Bac parent
#'
#' Renvoie le code Bac parent.
#'
#' @param code_bac Un vecteur de code Bac.
#' @param parent_final \code{TRUE}, renvoit le parent le plus haut dans la hiérarchie; \code{FALSE}, renvoie le premier parent.
#' @param garder_na \code{TRUE}, les codes sans parent passent à \code{NA}; \code{FALSE}, les codes sans parent sont gardés tels quels.
#'
#' @return Un vecteur de code Bac parent.
#'
#' Jeu de données source : \code{apogee::bac}.\cr
#' Il est créé à partir de la table "bac" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
hier_bac_parent <- function(code_bac, parent_final = FALSE, garder_na = FALSE) {
  
  hier_bac_parent <- dplyr::tibble(code_bac) %>%
    dplyr::left_join(apogee::bac, by = "code_bac") %>%
    dplyr::pull(code_bac_parent)
  
  if (garder_na == FALSE) {
    hier_bac_parent <- dplyr::if_else(is.na(hier_bac_parent), code_bac, hier_bac_parent)
  }
  
  if (parent_final == TRUE) {
    if (any(!is.na(apogee::hier_bac_parent(hier_bac_parent, parent_final = FALSE, garder_na = TRUE)))) {
      hier_bac_parent <- Recall(hier_bac_parent, parent_final = parent_final, garder_na = garder_na)
    }
  }
  
  return(hier_bac_parent)
}

#' Renvoie le code PCS parent
#'
#' Renvoie le code PCS parent.
#'
#' @param code_pcs Un vecteur de code PCS.
#' @param parent_final \code{TRUE}, renvoit le parent le plus haut dans la hiérarchie; \code{FALSE}, renvoie le premier parent.
#' @param garder_na \code{TRUE}, les codes sans parent passent à \code{NA}; \code{FALSE}, les codes sans parent sont gardés tels quels.
#'
#' @return Un vecteur de code PCS parent.
#'
#' Jeu de données source : \code{apogee::pcs}.\cr
#' Il est créé à partir de la table "pcs" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
hier_pcs_parent <- function(code_pcs, parent_final = FALSE, garder_na = FALSE) {
  
  hier_pcs_parent <- dplyr::tibble(code_pcs) %>%
    dplyr::left_join(apogee::pcs, by = "code_pcs") %>%
    dplyr::pull(code_pcs_parent)
  
  if (garder_na == FALSE) {
    hier_pcs_parent <- dplyr::if_else(is.na(hier_pcs_parent), code_pcs, hier_pcs_parent)
  }
  
  if (parent_final == TRUE) {
    if (any(!is.na(apogee::hier_pcs_parent(hier_pcs_parent, parent_final = FALSE, garder_na = TRUE)))) {
      hier_pcs_parent <- Recall(hier_pcs_parent, parent_final = parent_final, garder_na = garder_na)
    }
  }
  
  return(hier_pcs_parent)
}

#' Renvoie la mention de diplome a partir du code etape
#'
#' Renvoie la mention de diplôme à partir du code étape.
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur contenant les mentions de diplôme.
#'
#' @export
hier_etape_mention <- function(code_etape) {
  
  hier_etape_mention <- dplyr::tibble(code_etape) %>%
    dplyr::mutate(.id = dplyr::row_number()) %>% 
    dplyr::left_join(apogee::etape_mention, by = "code_etape") %>%
    split(x = .$code_mention_diplome, f = .$.id)
  
  names(hier_etape_mention) <- code_etape

  return(hier_etape_mention)
}

#' Renvoie le domaine de diplome a partir du code etape
#'
#' Renvoie le domaine de diplôme à partir du code étape.
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur contenant les domaines de diplôme.
#'
#' @export
hier_etape_domaine <- function(code_etape) {
  
  hier_etape_domaine <- dplyr::tibble(code_etape) %>%
    dplyr::mutate(.id = dplyr::row_number()) %>% 
    dplyr::left_join(apogee::etape_domaine, by = "code_etape") %>%
    split(x = .$code_domaine_diplome, f = .$.id)
  
  names(hier_etape_domaine) <- code_etape
  
  return(hier_etape_domaine)
}

#' Renvoie le code composante parent
#'
#' Renvoie le code composante parent.
#'
#' @param code_composante Un vecteur de code composante.
#' @param parent_final \code{TRUE}, renvoit le parent le plus haut dans la hiérarchie; \code{FALSE}, renvoie le premier parent.
#' @param garder_na \code{TRUE}, les codes sans parent passent à \code{NA}; \code{FALSE}, les codes sans parent sont gardés tels quels.
#'
#' @return Un vecteur de code composante parent.
#'
#' Jeu de données source : \code{apogee::composante}.\cr
#' Il est créé à partir de la table "composante" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
hier_composante_parent <- function(code_composante, parent_final = FALSE, garder_na = FALSE) {
  
  hier_composante_parent <- dplyr::tibble(code_composante) %>%
    dplyr::left_join(apogee::composante, by = "code_composante") %>%
    dplyr::pull(code_composante_parent)
  
  if (garder_na == FALSE) {
    hier_composante_parent <- dplyr::if_else(is.na(hier_composante_parent), code_composante, hier_composante_parent)
  }
  
  if (parent_final == TRUE) {
    if (any(!is.na(apogee::hier_composante_parent(hier_composante_parent, parent_final = FALSE, garder_na = TRUE)))) {
      hier_composante_parent <- Recall(hier_composante_parent, parent_final = parent_final, garder_na = garder_na)
    }
  }
  
  return(hier_composante_parent)
}

#' Renvoie les codes academie a partir des codes de departement
#'
#' Renvoie les codes académie à partir des codes de département.
#'
#' @param code_departement Un vecteur de code de départements.
#'
#' @return Un vecteur de code académie.
#'
#' Jeu de données source : \code{apogee::departement_academie}.\cr
#' Il est créé à partir de la table "departement_academie" de la base Access "Tables_ref.accdb".
#'
#' @examples
#' apogee::hier_departement_academie(c("031", "056"))
#'
#' @export
hier_departement_academie <- function(code_departement) {
  
  hier_departement_academie <- dplyr::tibble(code_departement) %>%
    dplyr::left_join(apogee::departement_academie, by = "code_departement") %>%
    dplyr::pull(code_academie)
  
  return(hier_departement_academie)
}

#' Renvoie les codes de cycle a partir du code etape
#'
#' Renvoie les codes de cycle à partir du code étape.
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur de code cycle.
#'
#' Jeu de données source : \code{apogee::etape_cycle}.\cr
#'
#' @export
hier_etape_cycle <- function(code_etape) {
  
  hier_etape_cycle <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape_cycle, by = "code_etape") %>%
    dplyr::pull(code_cycle)
  
  return(hier_etape_cycle)
}

#' Renvoie le secteur (secondaire/tertiaire) a partir du code etape
#'
#' Renvoie le secteur (secondaire/tertiaire) à partir du code étape.
#'
#' @param code_etape Un vecteur de code étape.
#'
#' @return Un vecteur contenant les secteurs (secondaire/tertiaire).
#'
#' Jeu de données source : \code{apogee::etape_secteur}.\cr
#' Il est créé à partir de la table "etape_secteur" de la base Access Tables_ref (projet Apogee).
#'
#' @export
hier_etape_secteur <- function(code_etape) {
  
  hier_etape_secteur <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape_secteur, by = "code_etape") %>%
    dplyr::pull(secteur)
  
  return(hier_etape_secteur)
}

#' Renvoie la mention de diplome parent
#'
#' Renvoie la mention de diplôme parent.
#'
#' @param code_mention_diplome Un vecteur de code de mention de diplôme.
#'
#' @return Un vecteur contenant les mentions de diplôme parent.
#'
#' @export
hier_mention_parent <- function(code_mention_diplome, parent_final = FALSE, garder_na = FALSE) {
  
  hier_mention_parent <- dplyr::tibble(code_mention_diplome) %>%
    dplyr::left_join(apogee::diplome_mention, by = "code_mention_diplome") %>%
    dplyr::pull(code_mention_diplome_parent)
  
  if (garder_na == FALSE) {
    hier_mention_parent <- dplyr::if_else(is.na(hier_mention_parent), code_mention_diplome, hier_mention_parent)
  }
  
  if (parent_final == TRUE) {
    if (any(!is.na(apogee::hier_mention_parent(hier_mention_parent, parent_final = FALSE, garder_na = TRUE)))) {
      hier_mention_parent <- Recall(hier_mention_parent, parent_final = parent_final, garder_na = garder_na)
    }
  }
  
  return(hier_mention_parent)
}

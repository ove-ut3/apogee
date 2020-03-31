#' Renvoie le libelle a partir du code etape
#'
#' Renvoie le libellé à partir du code étape.
#'
#' @param code_etape Un vecteur de code étape.
#' @param prefixe Libellé de type de formation ou de type de diplôme en préfixe du libellé de formation.
#' @param suffixe Libellé de ville, option, particularité ou année dans le diplôme en suffixe du libellé de formation.
#'
#' @return Un vecteur contenant les libellés étape.
#'
#' Jeu de données source : \code{apogee::etape}.\cr
#' Il est créé à partir d'Apogée et enrichi par la table "etape" de la base Access "Tables_ref.accdb".
#'
#' @export
lib_etape <- function(code_etape, prefixe = "formation", suffixe = c("ville", "option", "particularite", "annee")) {
  champ_lib_etape <- purrr::map_lgl(c("ville", "option", "particularite"), ~ . %in% suffixe) %>%
    paste(collapse = " ") %>%
    dplyr::recode(
      "TRUE TRUE TRUE" = "lib_etape_ville_option_particularite",
      "TRUE TRUE FALSE" = "lib_etape_ville_option",
      "TRUE FALSE TRUE" = "lib_etape_ville_particularite",
      "TRUE FALSE FALSE" = "lib_etape_ville",
      "FALSE TRUE TRUE" = "lib_etape_option_particularite",
      "FALSE TRUE FALSE" = "lib_etape_option",
      "FALSE FALSE TRUE" = "lib_etape_particularite",
      "FALSE FALSE FALSE" = "lib_etape"
    )

  lib_etape <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape, by = "code_etape") %>%
    dplyr::rename(champ_lib_etape = !!champ_lib_etape)

  if (prefixe %in% "formation") {
    lib_etape <- lib_etape %>%
      dplyr::mutate(
        type_diplome = apogee::acronyme_type_diplome(code_type_diplome),
        champ_lib_etape = dplyr::if_else(
          temoin_etape_apogee == FALSE & !type_diplome %in% c("DAEU", "DE infirmier-e", "Dentaire", "Diplôme d'Etat", "DNO", "HDR", "Médecine", "Pharmacie", "TH FICTIVE", "Vétérinaire"),
          paste(
            stringr::str_replace_na(type_diplome, ""),
            champ_lib_etape
          ),
          champ_lib_etape
        )
      )
  } else if (prefixe %in% "diplome") {
    lib_etape <- lib_etape %>%
      dplyr::mutate_at("code_type_diplome", apogee::hier_type_diplome_parent) %>%
      dplyr::mutate(
        lib_type_diplome = apogee::acronyme_type_diplome(code_type_diplome),
        champ_lib_etape = dplyr::if_else(
          temoin_etape_apogee == FALSE,
          paste(
            stringr::str_replace_na(lib_type_diplome, ""),
            champ_lib_etape
          ),
          champ_lib_etape
        )
      )
  }

  if ("annee" %in% suffixe) {
    lib_etape <- lib_etape %>%
      dplyr::mutate(
        champ_lib_etape = dplyr::if_else(
          !is.na(annee_diplome),
          paste0(
              scales::ordinal(rules = scales::ordinal_french(gender = "feminin")),
            champ_lib_etape,
            " - ",
            apogee::annee_diplome(code_etape) %>%
            " année"
          ),
          champ_lib_etape
        )
      )
  }

  lib_etape <- lib_etape %>%
    dplyr::pull(champ_lib_etape)

  if (class(lib_etape) == "logical") {
    lib_etape <- as.character(lib_etape)
  }

  return(lib_etape)
}

#' Renvoie l'acronyme a partir du code etape
#'
#' Renvoie l'acronyme à partir du code étape.
#'
#' @param code_etape Un vecteur de code étape.
#' @param prefixe Libellé de type de formation ou de type de diplôme en préfixe du libellé de formation.
#' @param suffixe Libellé de ville, option, particularité ou année dans le diplôme en suffixe de l'acronyme de formation.
#'
#' @return Un vecteur contenant les acronymes d'étape.
#'
#' Jeu de données source : \code{apogee::etape}.\cr
#' Il est créé à partir de la table "etape" de la base Access "Tables_ref.accdb".
#'
#' @export
acronyme_etape <- function(code_etape, prefixe = "formation", suffixe = c("ville", "option", "particularite", "annee")) {
  champ_acronyme_etape <- purrr::map_lgl(c("ville", "option", "particularite"), ~ . %in% suffixe) %>%
    paste(collapse = " ") %>%
    dplyr::recode(
      "TRUE TRUE TRUE" = "acronyme_etape_ville_option_particularite",
      "TRUE TRUE FALSE" = "acronyme_etape_ville_option",
      "TRUE FALSE TRUE" = "acronyme_etape_ville_particularite",
      "TRUE FALSE FALSE" = "acronyme_etape_ville",
      "FALSE TRUE TRUE" = "acronyme_etape_option_particularite",
      "FALSE TRUE FALSE" = "acronyme_etape_option",
      "FALSE FALSE TRUE" = "acronyme_etape_particularite",
      "FALSE FALSE FALSE" = "acronyme_etape"
    )

  acronyme_etape <- dplyr::tibble(code_etape) %>%
    dplyr::left_join(apogee::etape, by = "code_etape") %>%
    dplyr::rename(champ_acronyme_etape = !!champ_acronyme_etape)

  if (prefixe %in% "formation") {
    acronyme_etape <- acronyme_etape %>%
      dplyr::mutate(
        type_diplome = apogee::acronyme_type_diplome(code_type_diplome),
        champ_acronyme_etape = dplyr::if_else(
          temoin_etape_apogee == FALSE & !type_diplome %in% c("DAEU", "DE infirmier-e", "Dentaire", "Diplôme d'Etat", "DNO", "HDR", "Médecine", "Pharmacie", "TH FICTIVE", "Vétérinaire"),
          paste(
            stringr::str_replace_na(type_diplome, ""),
            champ_acronyme_etape
          ),
          champ_acronyme_etape
        )
      )
  } else if (prefixe %in% "diplome") {
    acronyme_etape <- acronyme_etape %>%
      dplyr::mutate_at("code_type_diplome", apogee::hier_type_diplome_parent) %>%
      dplyr::mutate(
        lib_type_diplome = apogee::acronyme_type_diplome(code_type_diplome),
        champ_acronyme_etape = dplyr::if_else(
          temoin_etape_apogee == FALSE,
          paste(
            stringr::str_replace_na(lib_type_diplome, ""),
            champ_acronyme_etape
          ),
          champ_acronyme_etape
        )
      )
  }

  if ("annee" %in% suffixe) {
    acronyme_etape <- acronyme_etape %>%
      dplyr::mutate(
        champ_acronyme_etape = dplyr::if_else(
          !is.na(annee_diplome),
          paste0(
            champ_acronyme_etape,
            " - ",
            apogee::annee_diplome(code_etape) %>%
              scales::ordinal(rules = ordinal_french(gender = "feminin")), " année"
          ),
          champ_acronyme_etape
        )
      )
  }

  acronyme_etape <- acronyme_etape %>%
    dplyr::pull(champ_acronyme_etape)

  return(acronyme_etape)
}

#' Renvoie le libelle a partir du code cursus d'une etape
#'
#' Renvoie le libellé à partir du code cursus d'une étape.
#'
#' @param code_cursus_etape Un vecteur de code cursus d'une étape.
#'
#' @return Un vecteur contenant les libellé de cursus.
#'
#' Jeu de données source : \code{apogee::cursus_etape}.\cr
#'
#' @export
lib_cursus_etape <- function(code_cursus_etape) {
  lib_cursus_etape <- dplyr::tibble(code_cursus_etape) %>%
    dplyr::left_join(apogee::etape_cursus, by = "code_cursus_etape") %>%
    dplyr::pull(lib_cursus_etape)

  return(lib_cursus_etape)
}

#' Renvoie le libelle a partir du code composante
#'
#' Renvoie le libellé à partir du code composante.
#'
#' @param code_composante Un vecteur de code composante.
#'
#' @return Un vecteur contenant les libellés composante.
#'
#' Jeu de données source : \code{apogee::composante}.\cr
#' Il est créé à partir de la table "composante" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
lib_composante <- function(code_composante) {
  lib_composante <- dplyr::tibble(code_composante) %>%
    dplyr::left_join(apogee::composante, by = "code_composante") %>%
    dplyr::pull(lib_composante)

  return(lib_composante)
}

#' Renvoie le libelle a partir du code de regime d'inscription
#'
#' Renvoie le libellé à partir du code de régime d'inscription.
#'
#' @param code_regime_inscription Un vecteur de code de régime d'inscription.
#'
#' @return Un vecteur contenant les libellés de régime d'inscription.
#'
#' Jeu de données source : \code{apogee::regime_inscription}.\cr
#' Il est créé à partir de la table "regime_inscription" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
lib_regime_inscription <- function(code_regime_inscription) {
  lib_regime_inscription <- dplyr::tibble(code_regime_inscription) %>%
    dplyr::left_join(apogee::regime_inscription, by = "code_regime_inscription") %>%
    dplyr::pull(lib_regime_inscription)

  return(lib_regime_inscription)
}

#' Renvoie le libelle a partir du code de type diplome
#'
#' Renvoie le libellé à partir du code de type diplôme.
#'
#' @param code_type_diplome Un vecteur de code de type diplôme.
#'
#' @return Un vecteur contenant les libellés de type diplôme.
#'
#' Jeu de données source : \code{apogee::diplome_type}.\cr
#' Il est créé à partir de la table "diplome_type" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
lib_type_diplome <- function(code_type_diplome) {
  lib_type_diplome <- dplyr::tibble(code_type_diplome) %>%
    dplyr::left_join(apogee::diplome_type, by = "code_type_diplome") %>%
    dplyr::mutate(lib_type_diplome = dplyr::if_else(is.na(lib_type_diplome), code_type_diplome, lib_type_diplome)) %>%
    dplyr::pull(lib_type_diplome)

  return(lib_type_diplome)
}

#' Renvoie le libelle a partir du code de finalite de diplome
#'
#' Renvoie le libellé à partir du code de finalité de diplôme.
#'
#' @param code_type_diplome Un vecteur de code de finalité de diplôme.
#'
#' @return Un vecteur contenant les libellés de type diplôme.
#'
#' Jeu de données source : \code{apogee::finalite_diplome}.\cr
#'
#' @export
lib_finalite_diplome <- function(code_finalite_diplome) {
  lib_finalite_diplome <- dplyr::tibble(code_finalite_diplome) %>%
    dplyr::left_join(apogee::finalite_diplome, by = "code_finalite_diplome") %>%
    dplyr::pull(lib_finalite_diplome)

  return(lib_finalite_diplome)
}

#' Renvoie le libelle a partir du code bourse
#'
#' Renvoie le libellé à partir du code bourse.
#'
#' @param code_type_diplome Un vecteur de code de bourse.
#'
#' @return Un vecteur contenant les libellés de bourse.
#'
#' Jeu de données source : \code{apogee::bourse}.\cr
#'
#' @export
lib_bourse <- function(code_bourse) {
  lib_bourse <- dplyr::tibble(code_bourse) %>%
    dplyr::left_join(apogee::bourse, by = "code_bourse") %>%
    dplyr::pull(lib_bourse)

  return(lib_bourse)
}

#' Renvoie l'acronyme a partir du code de type diplome
#'
#' Renvoie l'acronyme à partir du code de type diplôme.
#'
#' @param code_type_diplome Un vecteur de code de type diplôme.
#'
#' @return Un vecteur contenant les acronymes de type diplôme.
#'
#' Jeu de données source : \code{apogee::diplome_type}.\cr
#' Il est créé à partir de la table "diplome_type" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
acronyme_type_diplome <- function(code_type_diplome) {
  acronyme_type_diplome <- dplyr::tibble(code_type_diplome) %>%
    dplyr::left_join(apogee::diplome_type, by = "code_type_diplome") %>%
    dplyr::pull(acronyme_type_diplome)

  return(acronyme_type_diplome)
}

#' Renvoie le libelle a partir du code type etablissement
#'
#' Renvoie le libellé à partir du code type établissement.
#'
#' @param code_type_etab Un vecteur de code de type établissement.
#'
#' @return Un vecteur contenant les libellés de type établissement.
#'
#' Jeu de données source : \code{apogee::etablissement_type}.\cr
#'
#' @export
lib_type_etab <- function(code_type_etab) {
  lib_type_etab <- dplyr::tibble(code_type_etab) %>%
    dplyr::left_join(apogee::etablissement_type, by = "code_type_etab") %>%
    dplyr::pull(lib_type_etab)

  return(lib_type_etab)
}

#' Renvoie le libelle a partir du code ELP
#'
#' Renvoie le libellé à partir du code ELP.
#'
#' @param code_elp Un vecteur de code de ELP.
#'
#' @return Un vecteur contenant les libellés de ELP.
#'
#' Jeu de données source : \code{apogee::elp}.\cr
#'
#' @export
lib_elp <- function(code_elp) {
  lib_elp <- dplyr::tibble(code_elp) %>%
    dplyr::left_join(apogee::elp, by = "code_elp") %>%
    dplyr::pull(lib_elp)

  return(lib_elp)
}

#' Renvoie le libelle a partir du code nature ELP
#'
#' Renvoie le libellé à partir du code nature ELP.
#'
#' @param code_nature_elp Un vecteur de code de nature ELP.
#'
#' @return Un vecteur contenant les libellés de nature ELP.
#'
#' Jeu de données source : \code{apogee::elp_nature}.\cr
#'
#' @export
lib_nature_elp <- function(code_nature_elp) {
  lib_nature_elp <- dplyr::tibble(code_nature_elp) %>%
    dplyr::left_join(apogee::elp_nature, by = "code_nature_elp") %>%
    dplyr::pull(lib_nature_elp)

  return(lib_nature_elp)
}

#' Renvoie le libelle a partir du code Bac
#'
#' Renvoie le libellé à partir du code Bac.
#'
#' @param code_bac Un vecteur de code Bac.
#'
#' @return Un vecteur contenant les libellés Bac.
#'
#' Jeu de données source : \code{apogee::bac}.\cr
#' Il est créé à partir de la table "bac" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
lib_bac <- function(code_bac) {
  lib_bac <- dplyr::tibble(code_bac) %>%
    dplyr::left_join(apogee::bac, by = "code_bac") %>%
    dplyr::pull(lib_bac)

  return(lib_bac)
}

#' Renvoie l'acronyme a partir du code Bac
#'
#' Renvoie l'acronyme à partir du code Bac.
#'
#' @param code_bac Un vecteur de code Bac.
#'
#' @return Un vecteur contenant les acronymes de Bac.
#'
#' Jeu de données source : \code{apogee::bac}.\cr
#' Il est créé à partir de la table "bac" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
acronyme_bac <- function(code_bac) {
  acronyme_bac <- dplyr::tibble(code_bac) %>%
    dplyr::left_join(apogee::bac, by = "code_bac") %>%
    dplyr::pull(acronyme_bac)

  return(acronyme_bac)
}

#' Renvoie le libelle a partir du code de mention au Bac
#'
#' Renvoie le libellé à partir du code de mention au Bac.
#'
#' @param code_pcs Un vecteur de code de mention au Bac.
#'
#' @return Un vecteur contenant les libellés de mention au Bac.
#'
#' Jeu de données source : \code{apogee::bac_mention}.\cr
#' Il est créé à partir de la table "bac_mention" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
lib_mention_bac <- function(code_mention_bac) {
  lib_mention_bac <- dplyr::tibble(code_mention_bac) %>%
    dplyr::left_join(apogee::bac_mention, by = "code_mention_bac") %>%
    dplyr::pull(lib_mention_bac)

  return(lib_mention_bac)
}

#' Renvoie le libelle a partir du code PCS
#'
#' Renvoie le libellé à partir du code PCS.
#'
#' @param code_pcs Un vecteur de code PCS.
#'
#' @return Un vecteur contenant les libellés PCS.
#'
#' Jeu de données source : \code{apogee::pcs}.\cr
#' Il est créé à partir de la table "pcs" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
lib_pcs <- function(code_pcs) {
  lib_pcs <- dplyr::tibble(code_pcs) %>%
    dplyr::left_join(apogee::pcs, by = "code_pcs") %>%
    dplyr::pull(lib_pcs)

  return(lib_pcs)
}

#' Renvoie le libelle a partir du code sexe
#'
#' Renvoie le libellé à partir du code sexe.
#'
#' @param code_sexe Un vecteur de code sexe.
#'
#' @return Un vecteur contenant les libellés de sexe.
#'
#' Jeu de données source : \code{apogee::sexe}.\cr
#' Il est créé à partir de la table "sexe" de la base Access "Tables_ref.accdb" (projet Apogée).
#'
#' @export
lib_sexe <- function(code_sexe) {
  lib_sexe <- dplyr::tibble(code_sexe) %>%
    dplyr::left_join(apogee::sexe, by = "code_sexe") %>%
    dplyr::pull(lib_sexe)

  return(lib_sexe)
}

#' Renvoie le libelle a partir du code situation sociale
#'
#' Renvoie le libellé à partir du code situation sociale.
#'
#' @param code_situation_sociale Un vecteur de code situation sociale.
#'
#' @return Un vecteur contenant les libellés de situation sociale.
#'
#' Jeu de données source : \code{apogee::situation_sociale}.\cr
#'
#' @export
lib_situation_sociale <- function(code_situation_sociale) {
  lib_situation_sociale <- dplyr::tibble(code_situation_sociale) %>%
    dplyr::left_join(apogee::situation_sociale, by = "code_situation_sociale") %>%
    dplyr::pull(lib_situation_sociale)

  return(lib_situation_sociale)
}

#' Renvoie le libelle a partir du code statut etudiant
#'
#' Renvoie le libellé à partir du code statut étudiant.
#'
#' @param code_situation_sociale Un vecteur de code statut étudiant.
#'
#' @return Un vecteur contenant les libellés de statut étudiant.
#'
#' Jeu de données source : \code{apogee::statut_etudiant}.\cr
#'
#' @export
lib_statut_etudiant <- function(code_statut_etudiant) {
  lib_statut_etudiant <- dplyr::tibble(code_statut_etudiant) %>%
    dplyr::left_join(apogee::statut_etudiant, by = "code_statut_etudiant") %>%
    dplyr::pull(lib_statut_etudiant)

  return(lib_statut_etudiant)
}

#' Renvoie le libelle a partir du code mention de diplome
#'
#' Renvoie le libellé à partir du code mention de diplôme.
#'
#' @param code_mention_diplome Un vecteur de code mention de diplôme.
#'
#' @return Un vecteur contenant les libellés de mention de diplôme.
#'
#' Jeu de données source : \code{apogee::mention_diplome}.\cr
#'
#' @export
lib_mention_diplome <- function(code_mention_diplome) {
  lib_mention_diplome <- dplyr::tibble(code_mention_diplome) %>%
    dplyr::left_join(apogee::mention_diplome, by = "code_mention_diplome") %>%
    dplyr::pull(lib_mention_diplome)

  return(lib_mention_diplome)
}

#' Renvoie le libelle a partir du code domaine de diplome
#'
#' Renvoie le libellé à partir du code domaine de diplôme.
#'
#' @param code_domaine_diplome Un vecteur de code domaine de diplôme.
#'
#' @return Un vecteur contenant les libellés de domaine de diplôme.
#'
#' Jeu de données source : \code{apogee::domaine_diplome}.\cr
#'
#' @export
lib_domaine_diplome <- function(code_domaine_diplome) {
  lib_domaine_diplome <- dplyr::tibble(code_domaine_diplome) %>%
    dplyr::left_join(apogee::domaine_diplome, by = "code_domaine_diplome") %>%
    dplyr::pull(lib_domaine_diplome)

  return(lib_domaine_diplome)
}

#' Renvoie le libelle a partir du code diplome
#'
#' Renvoie le libellé à partir du diplôme.
#'
#' @param code_diplome Un vecteur de code diplôme.
#'
#' @return Un vecteur contenant les libellés de diplôme.
#'
#' Jeu de données source : \code{apogee::diplome}.\cr
#'
#' @export
lib_diplome <- function(code_diplome) {
  lib_diplome <- dplyr::tibble(code_diplome) %>%
    dplyr::left_join(apogee::diplome, by = "code_diplome") %>%
    dplyr::pull(lib_diplome)

  return(lib_diplome)
}

#' Renvoie le libelle a partir du code resultat
#'
#' Renvoie le libellé à partir du résultat.
#'
#' @param code_resultat Un vecteur de code résultat.
#'
#' @return Un vecteur contenant les libellés de résultat.
#'
#' Jeu de données source : \code{apogee::resultat}.\cr
#'
#' @export
lib_resultat <- function(code_resultat) {
  lib_resultat <- dplyr::tibble(code_resultat) %>%
    dplyr::left_join(apogee::resultat, by = "code_resultat") %>%
    dplyr::pull(lib_resultat)

  return(lib_resultat)
}

#' Renvoie le libelle a partir du code de periode d'enseignement
#'
#' Renvoie le libellé à partir code de période d'enseignement.
#'
#' @param code_periode_elp Un vecteur de code de période d'enseignement.
#'
#' @return Un vecteur contenant les libellés de période d'enseignement.
#'
#' Jeu de données source : \code{apogee::elp_periode}.\cr
#'
#' @export
lib_periode_elp <- function(code_periode_elp) {
  lib_periode_elp <- dplyr::tibble(code_periode_elp) %>%
    dplyr::left_join(apogee::elp_periode, by = "code_periode_elp") %>%
    dplyr::pull(lib_periode_elp)

  return(lib_periode_elp)
}

#' Renvoie le libelle a partir du code de type diplome d'origine
#'
#' Renvoie le libellé à partir du code de type diplôme d'origine
#'
#' @param code_type_diplome_origine Un vecteur de code de type diplôme d'origine
#'
#' @return Un vecteur contenant les libellés de type diplôme d'origine
#'
#' Jeu de données source : \code{apogee::diplome_origine_type}.\cr
#'
#' @export
lib_type_diplome_origine <- function(code_type_diplome_origine) {
  lib_type_diplome_origine <- dplyr::tibble(code_type_diplome_origine) %>%
    dplyr::left_join(apogee::diplome_origine_type, by = "code_type_diplome_origine") %>%
    dplyr::pull(lib_type_diplome_origine)

  return(lib_type_diplome_origine)
}

#' Renvoie le libelle a partir du code academie
#'
#' Renvoie le libellé à partir du code académie.
#'
#' @param code_academie Un vecteur de code académie.
#'
#' @return Un vecteur contenant les libellés d'académie.
#'
#' Jeu de données source : \code{apogee::academie}.\cr
#' Il est créé à partir de la table "academie" de la base Access "Tables_ref.accdb".
#'
#' @examples
#' apogee::lib_academie(c("01", "09"))
#' @export
lib_academie <- function(code_academie) {
  lib_academie <- dplyr::tibble(code_academie) %>%
    dplyr::left_join(apogee::academie, by = "code_academie") %>%
    dplyr::pull(lib_academie)

  return(lib_academie)
}

#' Renvoie l'acronyme a partir du code de domaine de diplome
#'
#' Renvoie l'acronyme à partir du code de domaine de diplôme.
#'
#' @param code_bac Un vecteur de code de domaine de diplôme.
#'
#' @return Un vecteur contenant les acronymes de domaine de diplôme.
#'
#' Jeu de données source : \code{apogee::domaine_diplome}.\cr
#' Il est créé à partir de la table "apogee::domaine_diplome" de la base Access "Tables_ref.accdb".
#'
#' @export
acronyme_domaine_diplome <- function(code_domaine_diplome) {
  acronyme_domaine_diplome <- dplyr::tibble(code_domaine_diplome) %>%
    dplyr::left_join(apogee::domaine_diplome, by = "code_domaine_diplome") %>%
    dplyr::pull(acronyme_domaine_diplome)

  return(acronyme_domaine_diplome)
}

#' Renvoie l'acronyme a partir du code de mention de diplome
#'
#' Renvoie l'acronyme à partir du code de mention de diplôme.
#'
#' @param code_etape Un vecteur de code de mention de diplôme.
#'
#' @return Un vecteur contenant les acronymes de mention de diplôme.
#'
#' Jeu de données source : \code{apogee::mention_diplome}.\cr
#' Il est créé à partir de la table "mention_diplome" de la base Access "Tables_ref.accdb".
#'
#' @export
acronyme_mention_diplome <- function(code_mention_diplome) {
  acronyme_mention_diplome <- dplyr::tibble(code_mention_diplome) %>%
    dplyr::left_join(apogee::mention_diplome, by = "code_mention_diplome") %>%
    dplyr::pull(acronyme_mention_diplome)

  return(acronyme_mention_diplome)
}

#' Renvoie le libelle a partir du code de la situation l'annee precedente
#'
#' Renvoie le libellé à partir du code de la situation l'année précédente.
#'
#' @param code_situation_annee_precedente Un vecteur de code de la situation l'année précédente.
#'
#' @return Un vecteur contenant les libellé de la situation l'année précédente.
#'
#' Jeu de données source : \code{apogee::situation_annee_precedente}.\cr
#'
#' @export
lib_situation_annee_precedente <- function(code_situation_annee_precedente) {
  lib_situation_annee_precedente <- dplyr::tibble(code_situation_annee_precedente) %>%
    dplyr::left_join(apogee::situation_annee_precedente, by = "code_situation_annee_precedente") %>%
    dplyr::pull(lib_situation_annee_precedente)

  return(lib_situation_annee_precedente)
}

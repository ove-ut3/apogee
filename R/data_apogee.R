#' doublon_maj_etudiant
#'
#' @param table \dots
#'
#' @export
#' @keywords internal
doublon_maj_etudiant <- function(table) {
  
  table <- table %>% 
    dplyr::left_join(impexp::access_importer("doublons_code_etudiant", paste0(racine_packages, "apogee/raw/Tables_ref_individus.accdb")),
                     by = "code_etudiant") %>% 
    dplyr::mutate(code_etudiant = ifelse(!is.na(code_etudiant_maj), code_etudiant_maj, code_etudiant)) %>% 
    dplyr::select(-code_etudiant_maj, -commentaire, -date_maj) %>% 
    unique()
  
  return(table)
}

#' data_individus
#'
#' @export
#' @keywords internal
data_individus <- function() {
  
  individus <- impexp::csv_importer_masse(regex_fichier = "Individus\\.csv", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE, ligne_debut = 2) %>% 
    tidyr::unnest() %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  
  individus_bac <- impexp::csv_importer_masse("Individus - Bac\\.csv", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE, ligne_debut = 2) %>% 
    tidyr::unnest() %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::arrange(code_etudiant, desc(annee_bac), code_mention_bac, code_type_etab_bac) %>% 
    dplyr::group_by(code_etudiant) %>% 
    dplyr::filter(row_number() == 1) %>% 
    dplyr::ungroup()
  
  individus <- dplyr::left_join(individus, individus_bac, by = "code_etudiant")
  
  individus_mail_ups <- impexp::csv_importer_masse("Individus - Mail UPS\\.csv", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE, ligne_debut = 2) %>% 
    tidyr::unnest() %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  
  individus <- dplyr::left_join(individus, individus_mail_ups, by = "code_etudiant")
  
  individus_departement_naissance <- impexp::csv_importer_masse("Individus_departement_naissance\\.csv", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE, ligne_debut = 2) %>% 
    tidyr::unnest() %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  
  individus <- dplyr::left_join(individus, individus_departement_naissance, by = "code_etudiant")
  
  save("individus", file = paste0(racine_packages, "apogee/data/individus.RData"))
}

#' data_individus_origine
#'
#' @export
#' @keywords internal
data_individus_diplome_origine <- function() {
  
  individus_diplome_origine <- impexp::csv_importer_masse("Individus_diplome_origine\\.csv", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE, ligne_debut = 2) %>% 
    tidyr::unnest() %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::arrange(code_etudiant, annee_diplome_obtenu)
    
  save("individus_diplome_origine", file = paste0(racine_packages, "apogee/data/individus_diplome_origine.RData"))
}

#' data_individus_formation_origine
#'
#' @export
#' @keywords internal
data_individus_formation_origine <- function() {
  
  individus_formation_origine <- impexp::csv_importer_masse("Individus_formation_origine\\.csv", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE, ligne_debut = 2) %>% 
    tidyr::unnest() %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  
  save("individus_formation_origine", file = paste0(racine_packages, "apogee/data/individus_formation_origine.RData"))
}

#' nest_inscrits
#'
#' @param table \dots
#' @param champ_nest \dots
#' @param cle \dots
#'
#' @export
#' @keywords internal
nest_inscrits <- function(table, champ_nest, cle = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) {
  
  quo_champ_nest <- dplyr::enquo(champ_nest)
  nom_champ_nest <- dplyr::quo_name(quo_champ_nest)
  
  nest2 <- table %>%
    dplyr::select(cle, !!quo_champ_nest) %>%
    unique() %>% 
    tidyr::nest(!!quo_champ_nest, .key = !!nom_champ_nest) %>% 
    dplyr::mutate(!!nom_champ_nest := purrr::map(!!quo_champ_nest, ~ .[[1]]))

  nest2 <- table %>%
    dplyr::select(!!!rlang::parse_quosure(paste0("-", nom_champ_nest))) %>%
    unique() %>%
    dplyr::full_join(nest2, by = cle)
  
  return(nest2)
}

#' data_inscrits
#'
#' @param derniere_annee \dots
#'
#' @export
#' @keywords internal
data_inscrits <- function(derniere_annee = TRUE) {
  
  if (derniere_annee == TRUE) {
    n_csv <- -1
  } else {
    n_csv <- Inf
  }
  
  inscrits <- impexp::csv_importer_masse("Inscrits\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), ligne_debut = 2, archive_zip = TRUE, n_csv = n_csv) %>% 
    dplyr::transmute(import = purrr::map(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = purrr::map(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant() %>% 
    source.maj::recoder_champs(impexp::access_importer("_recodage", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")), source = "data_inscrits", champs_table = FALSE)
  
  inscrits <- inscrits %>%
    apogee::nest_inscrits(code_composante) %>%
    apogee::nest_inscrits(code_version_etape) %>%
    dplyr::mutate(inscription_annulee = inscription_en_cours == "N" | !is.na(date_annulation) | inscription_resiliee == "O" | !is.na(date_resiliation),
                  inscription_annulee = ifelse(is.na(inscription_annulee), FALSE, inscription_annulee))
  
  inscrits_annules <- dplyr::filter(inscrits, inscription_annulee) %>% 
    dplyr::select(-inscription_annulee)
  
  if (derniere_annee == TRUE) {
    inscrits_annules <- divr::anti_join_bind(apogee::inscrits_annules, inscrits_annules, by = "annee")
  }
  
  save("inscrits_annules", file = paste0(racine_packages, "apogee/data/inscrits_annules.RData"))
  
  inscrits <- inscrits %>% 
    dplyr::filter(!inscription_annulee) %>% 
    dplyr::select(-inscription_annulee)
  
  #### Ajout ELP parcours ####
  
  elp_parcours <- apogee::inscrits_elp %>% 
    dplyr::filter(code_elp %in% apogee::etape_histo$code_elp) %>% 
    dplyr::anti_join(divr::doublons(., annee, code_etape, code_etudiant, inscription_premiere),
                     by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
    dplyr::rename(elp_parcours = code_elp)
  
  # Vérifier la présence de double parcours
  doublons <- divr::doublons(elp_parcours, annee, code_etape, code_etudiant, inscription_premiere)
  if (nrow(doublons) != 0) {
    message("Présence de doublons dans les parcours ELP rattachés aux inscriptions => concaténation des doubles parcours...")
    
    elp_parcours <- elp_parcours %>% 
      dplyr::group_by(annee, code_etape, code_etudiant, inscription_premiere) %>% 
      dplyr::summarise(elp_parcours = paste(elp_parcours, collapse = " ; ")) %>% 
      dplyr::ungroup()
  } 
  
  inscrits <- inscrits %>% 
    dplyr::left_join(elp_parcours, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))
  
  #### Ajout inscrits Base Access ####
  
  inscrits <- impexp::access_importer("inscrits_ajout", paste0(racine_packages, "apogee/raw/Tables_ref_individus.accdb")) %>% 
    dplyr::select(-commentaire, -date_maj) %>% 
    dplyr::anti_join(inscrits, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
    dplyr::bind_rows(inscrits) %>% 
    dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere)
  
  #### CPGE ####
  
  inscrits_cpge <- inscrits %>% 
    dplyr::filter(code_profil_etudiant %in% "CP" | apogee::hier_etape_filiere(code_etape) %in% "CPGE")
  
  if (derniere_annee == TRUE) {
    inscrits_cpge <- divr::anti_join_bind(apogee::inscrits_cpge, inscrits_cpge, by = "annee")
  }
  
  save("inscrits_cpge", file = paste0(racine_packages, "apogee/data/inscrits_cpge.RData"))
  
  inscrits <- inscrits %>% 
    dplyr::filter(!code_profil_etudiant %in% "CP",
                  !apogee::hier_etape_filiere(code_etape) %in% "CPGE")
  
  #### Sauvegarde finale ####
  
  if (derniere_annee == TRUE) {
    inscrits <- divr::anti_join_bind(apogee::inscrits, inscrits, by = "annee")
  }

  save("inscrits", file = paste0(racine_packages, "apogee/data/inscrits.RData"))
}

#' data_inscrits_peda
#'
#' @param derniere_annee \dots
#' 
#' @export
#' @keywords internal
data_inscrits_peda <- function(derniere_annee = TRUE) {
  
  if (derniere_annee == TRUE) {
    n_csv <- -1
  } else {
    n_csv <- Inf
  }
  
  inscrits_peda <- impexp::csv_importer_masse("Inscrits_peda\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), ligne_debut = 2, archive_zip = TRUE, n_csv = n_csv) %>% 
    dplyr::transmute(import = purrr::map(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = purrr::map(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant()
  
  if (derniere_annee == TRUE) {
    inscrits_peda <- divr::anti_join_bind(apogee::inscrits_peda, inscrits_peda, by = "annee")
  }
  
  save("inscrits_peda", file = paste0(racine_packages, "apogee/data/inscrits_peda.RData"))
}

#' data_inscrits_elp
#'
#' @param derniere_annee \dots
#' 
#' @export
#' @keywords internal
data_inscrits_elp <- function(derniere_annee = TRUE) {
  
  if (derniere_annee == TRUE) {
    n_csv <- -2
  } else {
    n_csv <- Inf
  }
  
  inscrits_elp <- impexp::csv_importer_masse("Inscrits_ELP.+?\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE, n_csv = n_csv, ligne_debut = 2) %>% 
    dplyr::transmute(import = lapply(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = lapply(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant() # %>% 
    # dplyr::semi_join(apogee::inscrits, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))
  
  if (derniere_annee == TRUE) {
    inscrits_elp <- divr::anti_join_bind(apogee::inscrits_elp, inscrits_elp, by = "annee")
  }
  
  save("inscrits_elp", file = paste0(racine_packages, "apogee/data/inscrits_elp.RData"))
}

#' data_resultats_elp
#'
#' @param derniere_annee \dots
#' 
#' @export
#' @keywords internal
data_resultats_elp <- function(derniere_annee = TRUE) {

  if (derniere_annee == TRUE) {
    n_csv <- -1
  } else {
    n_csv <- Inf
  }
  
  resultats_elp <- impexp::csv_importer_masse("Resultats_ELP\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE, n_csv = n_csv, ligne_debut = 2) %>% 
    dplyr::transmute(import = purrr::map(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = purrr::map(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant()
  
  if (derniere_annee == TRUE) {
    resultats_elp <- divr::anti_join_bind(apogee::resultats_elp, resultats_elp, by = "annee")
  }
  
  save("resultats_elp", file = paste0(racine_packages, "apogee/data/resultats_elp.RData"))
}

#' data_resultats_etape
#'
#' @param derniere_annee \dots
#' 
#' @export
#' @keywords internal
data_resultats_etape <- function(derniere_annee = TRUE) {
  
  if (derniere_annee == TRUE) {
    n_csv <- -1
  } else {
    n_csv <- Inf
  }
  
  resultats_etape <- impexp::csv_importer_masse("Resultats_etape\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE, n_csv = n_csv, ligne_debut = 2) %>% 
    dplyr::transmute(import = purrr::map(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = purrr::map(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant()
  
  #### PACES ####
  resultats_paces <- impexp::csv_importer_masse("Resultats_etape_paces\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE, ligne_debut = 2) %>% 
    dplyr::transmute(import = purrr::map(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = purrr::map(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant() %>% 
    dplyr::rename(note_etape = note_elp, code_resultat = code_resultat_elp) %>% 
    dplyr::filter(code_resultat == "ADM")
  
  resultats_etape <- resultats_etape %>% 
    dplyr::anti_join(resultats_paces, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
    dplyr::bind_rows(resultats_paces)
  
  #### doublons session ####
  
  doublons <- resultats_etape %>% 
    divr::doublons(annee, code_etape, code_etudiant, inscription_premiere, lib_session) %>% 
    dplyr::mutate(admis = ifelse(code_resultat %in% c("ADM", "ADJ"), 1, NA_integer_)) %>% 
    dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere, lib_session, admis) %>% 
    dplyr::group_by(annee, code_etape, code_etudiant, inscription_premiere, lib_session) %>% 
    dplyr::filter(row_number() == 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-admis)
  
  resultats_etape <- resultats_etape %>% 
    dplyr::anti_join(doublons, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere", "lib_session")) %>% 
    dplyr::bind_rows(doublons) %>% 
    dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere, lib_session)
  
  #### Supression de Session 2 si : résultat Session 1 ADM et résultat Session 2 non-ADM ####
  
  suppression_session2 <- apogee::resultats_etape %>% 
    dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, lib_session, code_resultat) %>% 
    tidyr::spread(lib_session, code_resultat) %>% 
    impexp::normaliser_nom_champs() %>% 
    dplyr::filter(session_1 %in% c("ADM", "ADJ") & !session_2 %in% c(NA_character_, "ADM", "ADJ")) %>% 
    dplyr::mutate(lib_session = "Session 2")
  
  resultats_etape <- resultats_etape %>% 
    dplyr::anti_join(suppression_session2, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere", "lib_session"))
  
  #### Sauvegarde ####
  
  if (derniere_annee == TRUE) {
    resultats_etape <- divr::anti_join_bind(apogee::resultats_etape, resultats_etape, by = "annee")
  }
  
  save("resultats_etape", file = paste0(racine_packages, "apogee/data/resultats_etape.RData"))
}

#' data_resultats_diplome
#'
#' @param derniere_annee \dots
#' 
#' @export
#' @keywords internal
data_resultats_diplome <- function(derniere_annee = TRUE) {
  
  if (derniere_annee == TRUE) {
    n_csv <- -1
  } else {
    n_csv <- Inf
  }
  
  resultats_diplome <- impexp::csv_importer_masse("Resultats_diplome\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE, n_csv = n_csv, ligne_debut = 2) %>% 
    dplyr::transmute(import = purrr::map(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = purrr::map(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant()

  #### Suppression diplômés non-existants chez les inscrits ####
  
  resultats_diplome <- resultats_diplome %>%
    dplyr::semi_join(dplyr::select(apogee::inscrits, annee, code_etape, code_etudiant, inscription_premiere),
                     by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

  if (derniere_annee == TRUE) {
    resultats_diplome <- divr::anti_join_bind(apogee::resultats_diplome, resultats_diplome, by = "annee")
  }
  
  save("resultats_diplome", file = paste0(racine_packages, "apogee/data/resultats_diplome.RData"))
}

#' data_diplomes
#'
#' @param derniere_annee \dots
#' 
#' @export
#' @keywords internal
data_diplomes <- function(derniere_annee = TRUE) {
  
  if (derniere_annee == TRUE) {
    n_csv <- -1
  } else {
    n_csv <- Inf
  }
  
  diplomes <- impexp::csv_importer_masse("Diplomes\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE, n_csv = n_csv, ligne_debut = 2) %>% 
    dplyr::transmute(import = purrr::map(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = purrr::map(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant()
  
  #### Ajout diplômés Base Access ####
  
  ajout_diplomes <- impexp::access_importer("diplomes_ajout", paste0(racine_packages, "apogee/raw/Tables_ref_individus.accdb")) %>% 
    dplyr::select(-commentaire, -date_maj)
  
  ajout_diplomes %>%
    dplyr::semi_join(dplyr::filter(diplomes,
                                   code_resultat_diplome == "ADM"),
                     by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))
  
  diplomes <- diplomes %>% 
    dplyr::anti_join(ajout_diplomes, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
    dplyr::bind_rows(ajout_diplomes) %>% 
    dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere)
  
  #### Suppression diplômés non-existants chez les inscrits ####
  
  diplomes <- diplomes %>%
    dplyr::semi_join(dplyr::select(apogee::inscrits, annee, code_etape, code_etudiant, inscription_premiere),
                     by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

  if (derniere_annee == TRUE) {
    diplomes <- divr::anti_join_bind(apogee::diplomes, diplomes, by = "annee")
  }
  
  save("diplomes", file = paste0(racine_packages, "apogee/data/diplomes.RData"))
}

#' data_stats
#'
#' @export
#' @keywords internal
data_stats <- function() {
  
  stats <- apogee::inscrits %>%
    dplyr::left_join(apogee::individus, by = "code_etudiant") %>% 
    dplyr::left_join(apogee::individus_diplome_origine %>% 
                       dplyr::group_by(code_etudiant) %>% 
                       dplyr::filter(row_number() == n()) %>% 
                       dplyr::ungroup(),
                     by = "code_etudiant") %>% 
    dplyr::left_join(apogee::resultats_etape %>% 
                       dplyr::group_by(annee, code_etape, code_etudiant, inscription_premiere) %>%
                       dplyr::filter(row_number() == n()) %>%
                       dplyr::ungroup(),
                     by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
    dplyr::mutate(code_etape = apogee::histo_etape_succ_2(code_etape)) %>%
    tidyr::unnest(code_etape) %>% 
    dplyr::rename(code_etape = code_etape_succ) %>% 
    dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, annee_bac, code_bac, code_type_diplome_anterieur, elp_parcours, code_resultat)
  
  origine <- apogee::inscrits %>% 
    dplyr::mutate(annee = annee + 1) %>% 
    dplyr::filter(inscription_premiere == "O") %>% 
    dplyr::select(annee, code_etape_pre = code_etape, code_etudiant) %>% 
    dplyr::right_join(stats, by = c("annee", "code_etudiant")) %>% 
    dplyr::mutate(origine_gen1 = ifelse(!is.na(code_etape_pre), "Admission interne", "Admission externe"),
                  origine_annee = apogee::annee_etape(code_etape_pre),
                  histo_code_etape_pre = apogee::histo_etape_succ_2(code_etape_pre) %>%
                    purrr::map("code_etape_succ"),
                  origine_gen2 = dplyr::case_when(
                    (apogee::annee_etape(code_etape) - origine_annee) == 1 ~ "Flux normal",
                    purrr::map2_lgl(code_etape, histo_code_etape_pre, ~ .x %in% purrr::map(.y, 1)) ~ "Redoublement",
                    !is.na(code_etape_pre) ~ "Réorientation",
                    # Année L1
                    apogee::annee_etape(code_etape) == 1 & annee == annee_bac ~ "Néo-bacheliers",
                    apogee::annee_etape(code_etape) == 1 & (annee != annee_bac | is.na(annee_bac)) ~ "Bacheliers antérieurs",
                    # Années >= L2
                    TRUE ~ apogee::lib_type_diplome_anterieur(code_type_diplome_anterieur)),
                  bac = ifelse(origine_gen2 == "Néo-bacheliers",
                               dplyr::case_when(
                                 apogee::hier_bac_parent(code_bac) == "04" ~ "Bac S",
                                 apogee::hier_bac_parent(code_bac, parent_final = TRUE) == "S1" ~ "Autres bacs généraux",
                                 apogee::hier_bac_parent(code_bac, parent_final = TRUE) == "S2" ~ apogee::lib_bac("S2"),
                                 apogee::hier_bac_parent(code_bac, parent_final = TRUE) == "S3" ~ apogee::lib_bac("S3"),
                                 apogee::hier_bac_parent(code_bac, parent_final = TRUE) == "S4" ~ "Autre"),
                               NA_character_),
                  origine_gen3 = dplyr::case_when(
                    origine_gen2 %in% c("Flux normal", "Réorientation") ~ apogee::histo_etape_succ(code_etape_pre) %>% apogee::lib_etape(ville = FALSE, option = FALSE, particularite = FALSE),
                    origine_gen2 == "Néo-bacheliers" ~ bac,
                    origine_gen2 == "Bacheliers antérieurs" ~ apogee::lib_type_diplome_anterieur(code_type_diplome_anterieur)),
                  origine_gen4 = dplyr::case_when(
                    origine_gen3 == "Bac S" ~ apogee::lib_bac(code_bac) %>% 
                      stringr::str_match("Bac S - (.+)") %>% 
                      .[, 2],
                    origine_gen3 == "Autres bacs généraux" ~ apogee::hier_bac_parent(code_bac) %>% 
                      apogee::acronyme_bac() %>% 
                      paste("Bac", .),
                    TRUE ~ NA_character_
                  )) %>% 
    dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, code_etape_pre, dplyr::starts_with("origine_gen"))
  
  present_ts_examens <- stats %>% 
    dplyr::filter(annee < apogee::annee_en_cours(mois_debut = 11)) %>% 
    dplyr::mutate(present_ts_examens = dplyr::case_when(is.na(code_resultat) ~ NA_character_,
                                                        code_resultat %in% c("ADM", "ADJ", "AJ", "AJAC", "AT") ~ "Oui",
                                                        TRUE ~ "Non")) %>% 
    dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, present_ts_examens)
  
  present_1_examen <- stats %>% 
    dplyr::semi_join(apogee::resultats_elp %>% 
                       dplyr::filter(apogee::lib_resultat(code_resultat_elp) == "Admis" | note_elp > 0) %>% 
                       dplyr::mutate(code_etape = apogee::histo_etape_succ_2(code_etape)) %>%
                       tidyr::unnest(code_etape) %>%
                       dplyr::rename(code_etape = code_etape_succ),
                     by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
    dplyr::mutate(present_1_examen = "Oui") %>% 
    dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, present_1_examen) %>% 
    dplyr::left_join(stats, ., by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
    tidyr::replace_na(list(present_1_examen = "Non")) %>% 
    dplyr::filter(annee < apogee::annee_en_cours(mois_debut = 11)) %>% 
    dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, present_1_examen)
  
  reussite <- stats %>% 
    dplyr::mutate(reussite = ifelse(apogee::lib_resultat(code_resultat) == "Admis", "Oui", "Non")) %>% 
    dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, reussite)
  
  poursuite <- apogee::inscrits %>% 
    dplyr::mutate(annee = annee - 1) %>% 
    dplyr::arrange(code_etudiant, annee, apogee::annee_etape(code_etape)) %>% 
    dplyr::group_by(code_etudiant, annee) %>% 
    dplyr::filter(row_number() == 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(annee, code_etape_post = code_etape, code_etudiant) %>% 
    dplyr::right_join(stats, by = c("annee", "code_etudiant")) %>% 
    dplyr::filter(annee < apogee::annee_en_cours(mois_debut = 11)) %>% 
    dplyr::mutate(code_etape_post = apogee::histo_etape_succ(code_etape_post, elp_parcours),
                  reussite = ifelse(apogee::lib_resultat(code_resultat) %in% "Admis", "Réussite", "Echec"),
                  annee_etape_post = apogee::annee_etape(code_etape_post),
                  filiere = apogee::hier_etape_filiere(code_etape),
                  histo_code_etape_post = apogee::histo_etape_succ_2(code_etape_post) %>%
                    purrr::map("code_etape_succ"),
                  poursuite = dplyr::case_when(
                    purrr::map_lgl(histo_code_etape_post, is.null) ~ "Sortie UPS",
                    purrr::map2_lgl(code_etape, histo_code_etape_post, ~ .x %in% purrr::map(.y, 1)) ~ "Redoublement",
                    filiere == "Santé" & purrr::map_lgl(histo_code_etape_post, ~ any(apogee::hier_etape_filiere(.) == "Santé")) ~ "Année supérieure",
                    filiere == "Santé" & annee_etape_post - apogee::annee_etape(code_etape) >= 1 ~ "Année supérieure - Réorientation",
                    annee_etape_post - apogee::annee_etape(code_etape) >= 1 ~ "Année supérieure",
                    annee_etape_post - apogee::annee_etape(code_etape) <= 0 ~ "Réorientation"),
                  poursuite = ifelse(poursuite == "Sortie UPS", paste(poursuite, reussite, sep = " - "), poursuite)) %>% 
    dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, code_etape_post, poursuite)
  
  stats <- stats %>% 
    dplyr::left_join(origine, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
    dplyr::left_join(present_ts_examens, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
    dplyr::left_join(present_1_examen, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
    dplyr::left_join(reussite, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
    dplyr::left_join(poursuite, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))
  
  save("stats", file = paste0(racine_packages, "apogee/data/stats.RData"))
}
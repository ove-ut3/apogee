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
  
  individus_bac <- impexp::csv_importer_masse("Individus - Bac\\.csv", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE) %>% 
    tidyr::unnest() %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::arrange(code_etudiant, desc(annee_bac), code_mention_bac, code_type_etab_bac) %>% 
    dplyr::group_by(code_etudiant) %>% 
    dplyr::filter(row_number() == 1) %>% 
    dplyr::ungroup()
  
  individus <- dplyr::left_join(individus, individus_bac, by = "code_etudiant")
  
  individus_mail_ups <- impexp::csv_importer_masse("Individus - Mail UPS\\.csv", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE) %>% 
    tidyr::unnest() %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  
  individus <- dplyr::left_join(individus, individus_mail_ups, by = "code_etudiant")
  
  save("individus", file = paste0(racine_packages, "apogee/data/individus.RData"))
}

#' data_individus_origine
#'
#' @export
#' @keywords internal
data_individus_diplome_origine <- function() {
  
  individus_diplome_origine <- impexp::csv_importer_masse("Individus_diplome_origine\\.csv", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE) %>% 
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
  
  individus_formation_origine <- impexp::csv_importer_masse("Individus_formation_origine\\.csv", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE) %>% 
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
    tidyr::nest(!!quo_champ_nest, .key = !!nom_champ_nest)
  
  nest2 <- table %>%
    dplyr::select(!!!rlang::parse_quosure(paste0("-", nom_champ_nest))) %>%
    unique() %>%
    dplyr::full_join(nest2, by = cle)
  
  return(nest2)
}

#' data_inscrits
#'
#' @export
#' @keywords internal
data_inscrits <- function() {
  
  inscrits <- impexp::csv_importer_masse("Inscrits\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), ligne_debut = 2, archive_zip = TRUE) %>% 
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
  save("inscrits_cpge", file = paste0(racine_packages, "apogee/data/inscrits_cpge.RData"))
  
  inscrits <- inscrits %>% 
    dplyr::filter(!code_profil_etudiant %in% "CP",
                  !apogee::hier_etape_filiere(code_etape) %in% "CPGE")
  
  #### Sauvegarde finale ####
  save("inscrits", file = paste0(racine_packages, "apogee/data/inscrits.RData"))
}

#' data_inscrits_peda
#'
#' @export
#' @keywords internal
data_inscrits_peda <- function() {
  
  inscrits_peda <- impexp::csv_importer_masse("Inscrits_peda\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), ligne_debut = 2, archive_zip = TRUE) %>% 
    dplyr::transmute(import = purrr::map(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = purrr::map(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant()
  
  save("inscrits_peda", file = paste0(racine_packages, "apogee/data/inscrits_peda.RData"))
}

#' data_inscrits_elp
#'
#' @export
#' @keywords internal
data_inscrits_elp <- function() {
  
  inscrits_elp <- impexp::csv_importer_masse("Inscrits_ELP\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE) %>% 
    dplyr::transmute(import = lapply(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = lapply(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant() # %>% 
    # dplyr::semi_join(apogee::inscrits, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))
  
  save("inscrits_elp", file = paste0(racine_packages, "apogee/data/inscrits_elp.RData"))
}

#' data_resultats_elp
#'
#' @export
#' @keywords internal
data_resultats_elp <- function() {

  resultats_elp <- impexp::csv_importer_masse("Resultats_ELP\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE) %>% 
    dplyr::transmute(import = purrr::map(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = purrr::map(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant()
  
  save("resultats_elp", file = paste0(racine_packages, "apogee/data/resultats_elp.RData"))
}

#' data_resultats_etape
#'
#' @export
#' @keywords internal
data_resultats_etape <- function() {
  
  resultats_etape <- impexp::csv_importer_masse("Resultats_etape\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE) %>% 
    dplyr::transmute(import = purrr::map(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = purrr::map(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant()
  
  #### PACES ####
  resultats_paces <- impexp::csv_importer_masse("Resultats_etape_paces\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE) %>% 
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
  
  save("resultats_etape", file = paste0(racine_packages, "apogee/data/resultats_etape.RData"))
}

#' data_resultats_diplome
#'
#' @export
#' @keywords internal
data_resultats_diplome <- function() {
  
  resultats_diplome <- impexp::csv_importer_masse("Resultats_diplome\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE) %>% 
    dplyr::transmute(import = purrr::map(import, source.maj::renommer_champs, impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))),
                     import = purrr::map(import, source.maj::transcoder_champs, impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))) %>% 
    tidyr::unnest() %>% 
    apogee::doublon_maj_etudiant()

  #### Suppression diplômés non-existants chez les inscrits ####
  
  resultats_diplome <- resultats_diplome %>%
    dplyr::semi_join(dplyr::select(apogee::inscrits, annee, code_etape, code_etudiant, inscription_premiere),
                     by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

  save("resultats_diplome", file = paste0(racine_packages, "apogee/data/resultats_diplome.RData"))
}

#' data_diplomes
#'
#' @export
#' @keywords internal
data_diplomes <- function() {
  
  diplomes <- impexp::csv_importer_masse("Diplomes\\.csv$", chemin = paste0(racine_packages, "apogee/raw"), archive_zip = TRUE) %>% 
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

  save("diplomes", file = paste0(racine_packages, "apogee/data/diplomes.RData"))
}

#' data_stats
#'
#' @export
#' @keywords internal
data_stats <- function() {
  
  stats <- apogee::inscrits %>% 
    dplyr::select(annee, code_etape, code_etudiant, inscription_premiere) %>% 
    dplyr::mutate(inscrit_adm = 1) %>% 
    dplyr::left_join(apogee::inscrits_peda %>% 
                       dplyr::mutate(inscrit_peda = 1),
                     by = c("annee", "code_etape", "code_etudiant"))
  
  # n_elp <- 
  #   # apogee::resultats_elp %>% 
  #   # dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, code_elp) %>% 
  #   # unique() %>% 
  #   apogee::inscrits_elp %>%
  #   dplyr::filter(apogee::histo_etape_succ(code_etape) == "ELMAIE") %>% 
  #   # dplyr::filter(code_etudiant == "21503791" & annee == 2015) %>%
  #   dplyr::group_by(annee, code_etape, code_etudiant, inscription_premiere) %>% 
  #   dplyr::summarise(n_elp = n()) %>% 
  #   dplyr::ungroup()
  # 
  # n_elp_presence <- apogee::resultats_elp %>% 
  #   # dplyr::filter(code_etudiant == "21503791" & annee == 2015) %>% 
  #   dplyr::filter(apogee::histo_etape_succ(code_etape) == "ELMAIE") %>% 
  #   dplyr::filter(note_elp > 0) %>% 
  #   dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, code_elp) %>% 
  #   unique() %>% 
  #   dplyr::count(annee, code_etape, code_etudiant, inscription_premiere) %>% 
  #   dplyr::rename(n_elp_presence = n)
  # 
  # presence <- n_elp %>% 
  #   dplyr::left_join(n_elp_presence, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
  #   dplyr::left_join(apogee::resultats_etape %>% 
  #                      dplyr::filter(apogee::histo_etape_succ(code_etape) == "ELMAIE") %>% 
  #                      dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere, lib_session) %>% 
  #                      dplyr::group_by(annee, code_etape, code_etudiant, inscription_premiere) %>% 
  #                      dplyr::filter(row_number() == n()) %>% 
  #                      dplyr::ungroup(), 
  #                    by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
  #   dplyr::filter(n_elp != n_elp_presence)
  # 
  #   dplyr::left_join(apogee::resultats_elp, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere", "code_elp")) %>% 
  #   dplyr::mutate(presence = ifelse(!is.na(code_resultat_elp) | !is.na(note_elp), 1, 0))
  #   dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere, lib_session) %>% 
  #   dplyr::group_by(annee, code_etape, code_etudiant, inscription_premiere) %>% 
  #   dplyr::filter(row_number() == n()) %>% 
  #   dplyr::ungroup() %>% 
  #   dplyr::filter(code_resultat == "DEM") %>% 
  #   dplyr::left_join(apogee::resultats_elp, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))
  # dplyr::mutate(presence = ifelse(code_resultat %in% c("ADJ", "ADM", "ADMI", "ADJ", "AJAC", "AJAD", "AJD", "AM", "AT", "CMP", "NAR", "REO", "VA", "VAL"), 1, 0))
  
  
  presence <- apogee::resultats_etape %>% 
    dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere, lib_session) %>% 
    dplyr::group_by(annee, code_etape, code_etudiant, inscription_premiere) %>% 
    dplyr::filter(row_number() == n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(is.na(code_resultat))
    dplyr::count(code_resultat)
    # dplyr::filter(code_resultat == "DEM") %>% 
    dplyr::left_join(apogee::resultats_elp, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))
    dplyr::mutate(presence = ifelse(code_resultat %in% c("ADJ", "ADM", "ADMI", "ADJ", "AJAC", "AJAD", "AJD", "AM", "AT", "CMP", "NAR", "REO", "VA", "VAL"), 1, 0))
    
  
}
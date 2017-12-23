#' data_etape
#'
#' @export
#' @keywords internal
data_etape <- function() {
  
  #### Etape ####
  
  inscrits <- apogee::inscrits
  
  annee_premiere_etape <- inscrits %>% 
    dplyr::select(code_etape, annee_premiere_etape = annee) %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::filter(row_number() == 1) %>% 
    dplyr::ungroup()
  
  annee_derniere_etape <- inscrits %>% 
    dplyr::select(code_etape, annee_derniere_etape = annee) %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::filter(row_number() == n()) %>% 
    dplyr::ungroup()
  
  etape <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Etape.xlsx"), ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::supprimer_doublons_champ(code_type_diplome) %>% 
    source.maj::supprimer_doublons_champ(temoin_annee1_diplome) %>% 
    dplyr::rename(lib_etape_apogee = lib_etape) %>% 
    dplyr::left_join(importr::importer_table_access("etape", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")),
                     by = "code_etape") %>% 
    source.maj::recoder_champs(importr::importer_table_access("_recodage", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")),
                               source = "data_etape", 
                               champs_table = FALSE) %>% 
    dplyr::mutate(lib_etape_apogee = ifelse(lib_etape != lib_etape_apogee, FALSE, TRUE)) %>% 
    dplyr::select(-annee_etape_apogee) %>% 
    dplyr::left_join(annee_premiere_etape, by = "code_etape") %>% 
    dplyr::left_join(annee_derniere_etape, by = "code_etape")
  
  divr::doublons(etape, code_etape)
  
  save("etape", file = paste0(racine_packages, "apogee/data/etape.RData"))
  
  #### Historique ####
  
  etape_histo <- importr::importer_table_access("etape_histo", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
    dplyr::filter(is.na(suppression_histo))
  
  dplyr::filter(etape_histo, is.na(code_elp)) %>% 
    divr::doublons(code_etape, code_etape_succ)
  
  dplyr::filter(etape_histo, !is.na(code_elp)) %>% 
    divr::doublons(code_etape, code_elp, code_etape_succ)
  
  # Eclatements
  eclatement <- etape_histo %>% 
    divr::doublons(code_etape) %>% 
    dplyr::mutate(doublon = "éclatement")
  
  eclatement_elp <- etape_histo %>% 
    divr::doublons(code_etape, code_elp) %>% 
    dplyr::mutate(doublon_elp = "éclatement")
  
  etape_histo <- etape_histo %>% 
    dplyr::anti_join(eclatement, by = c("code_etape", "code_etape_succ")) %>% 
    dplyr::bind_rows(eclatement) %>% 
    dplyr::anti_join(eclatement_elp, by = c("code_etape", "code_elp", "code_etape_succ")) %>% 
    dplyr::bind_rows(eclatement_elp) %>% 
    dplyr::mutate(doublon = ifelse(!is.na(doublon_elp), doublon_elp, doublon)) %>% 
    dplyr::arrange(code_etape, code_elp, code_etape_succ)
  
  save("etape_histo", file = paste0(racine_packages, "apogee/data/etape_histo.RData"))
  
  #### Flux ####
  
  etape_flux <- importr::importer_table_access("etape_flux", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("etape_flux", file = paste0(racine_packages, "apogee/data/etape_flux.RData"))
  
  #### Etape - mention ####
  
  etape_mention <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Etape.xlsx"), nom_onglet = "Etape_mention", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::bind_rows(importr::importer_table_access("etape_mention_diplome", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::arrange(code_etape, code_mention_diplome)
  
  etape_mention <- etape_mention %>% 
    dplyr::filter(is.na(suppression)) %>% 
    dplyr::select(-suppression) %>% 
    dplyr::left_join(dplyr::filter(etape_mention, !is.na(suppression)), by = c("code_etape", "code_mention_diplome")) %>% 
    dplyr::mutate(code_mention_diplome = ifelse(!is.na(suppression), NA_character_, code_mention_diplome)) %>% 
    dplyr::select(-suppression) %>% 
    dplyr::arrange(code_etape, code_mention_diplome) %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::filter(!is.na(code_mention_diplome) | row_number() == 1) %>% 
    tidyr::nest(code_mention_diplome, .key = "code_mention_diplome")
  save("etape_mention", file = paste0(racine_packages, "apogee/data/etape_mention.RData"))
  
  #### Etape - domaine ####
  
  etape_domaine <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Etape.xlsx"), nom_onglet = "Etape_domaine", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::bind_rows(importr::importer_table_access("etape_domaine_diplome", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::arrange(code_etape, code_domaine_diplome)
  
  etape_domaine <- etape_domaine %>% 
    dplyr::filter(is.na(suppression)) %>% 
    dplyr::select(-suppression) %>% 
    dplyr::left_join(dplyr::filter(etape_domaine, !is.na(suppression)), by = c("code_etape", "code_domaine_diplome")) %>% 
    dplyr::mutate(code_domaine_diplome = ifelse(!is.na(suppression), NA_character_, code_domaine_diplome)) %>% 
    dplyr::select(-suppression) %>% 
    dplyr::arrange(code_etape, code_domaine_diplome) %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::filter(!is.na(code_domaine_diplome) | row_number() == 1) %>% 
    tidyr::nest(code_domaine_diplome, .key = "code_domaine_diplome")
  save("etape_domaine", file = paste0(racine_packages, "apogee/data/etape_domaine.RData"))
  
  #### Etape - finalité ####
  etape_finalite <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Etape.xlsx"), nom_onglet = "Etape_finalite", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::recoder_champs(importr::importer_table_access("_recodage", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")), source = "data_etape_finalite") %>% 
    dplyr::arrange(code_etape, code_finalite_diplome) %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::filter(!is.na(code_finalite_diplome) | row_number() == 1) %>% 
    dplyr::mutate(code_finalite_diplome = ifelse(is.na(code_finalite_diplome),
                                                 dplyr::recode(substr(code_etape, 2, 2),
                                                               "F" = "005",
                                                               "I" = "004",
                                                               "P" = "001",
                                                               "R" = "002",
                                                               .default = NA_character_),
                                                 code_finalite_diplome))
  
  divr::doublons(etape_finalite, code_etape)
  
  save("etape_finalite", file = paste0(racine_packages, "apogee/data/etape_finalite.RData"))
  
  #### Etape - Discipline SISE ####
  etape_sise_discipline <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Etape.xlsx"), nom_onglet = "Etape_discipline_sise") %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::mutate(code_discipline_sise = stringr::str_replace(code_discipline_sise, "^00", "") %>% 
                    caractr::paste_na("SD", ., sep = "")) %>% 
    dplyr::bind_rows(importr::importer_table_access("etape_discipline_sise", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       dplyr::filter(is.na(suppression)) %>% 
                       dplyr::select(-suppression)) %>% 
    dplyr::left_join(importr::importer_table_access("etape_discipline_sise", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")), by = c("code_etape", "code_discipline_sise")) %>% 
    dplyr::filter(is.na(suppression)) %>% 
    dplyr::select(-suppression) %>% 
    tidyr::nest(code_discipline_sise, .key = "code_discipline_sise")
  save("etape_sise_discipline", file = paste0(racine_packages, "apogee/data/etape_sise_discipline.RData"))
  
  #### Etape - secteur DUT et LP ####
  
  etape_secteur <- importr::importer_table_access("etape_secteur", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("etape_secteur", file = paste0(racine_packages, "apogee/data/etape_secteur.RData"))
  
  #### Etape - spécialité ####
  
  etape_specialite_diplome <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Etape.xlsx"), nom_onglet = "Etape_specialite") %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::anti_join(dplyr::filter(., !is.na(code_specialite_diplome)) %>% 
                       dplyr::mutate(code_specialite_diplome = NA_character_),
                     by = c("code_etape", "code_specialite_diplome")) %>% 
    tidyr::nest(code_specialite_diplome, .key = "code_specialite_diplome")
  save("etape_specialite_diplome", file = paste0(racine_packages, "apogee/data/etape_specialite_diplome.RData"))
}

#' data_sise
#'
#' @export
#' @keywords internal
data_sise <- function() {
  
  conv_etape_sise <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Etape.xlsx"), nom_onglet = "Etape_sise", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(importr::importer_table_access("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::filter(annee >= 2007) %>% 
    dplyr::mutate(code_diplome_sise = as.character(code_diplome_sise)) %>% 
    dplyr::left_join(importr::importer_table_access("diplome_sise", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       tidyr::drop_na(annee),
                     by = c("annee", "code_diplome_sise" = "code_diplome")) %>% 
    dplyr::mutate(code_diplome_sise = ifelse(!is.na(code_diplome_maj), code_diplome_maj, code_diplome_sise)) %>% 
    dplyr::select(code_etape, annee, code_diplome_sise) %>% 
    dplyr::left_join(importr::importer_table_access("diplome_sise", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       dplyr::filter(is.na(annee)) %>% 
                       dplyr::select(-annee),
                     by = c("code_diplome_sise" = "code_diplome")) %>% 
    dplyr::mutate(code_diplome_sise = ifelse(!is.na(code_diplome_maj), code_diplome_maj, code_diplome_sise)) %>% 
    dplyr::select(code_etape, annee, code_diplome_sise) %>% 
    unique() %>% 
    tidyr::nest(code_diplome_sise, .key = "code_diplome_sise")
  save("conv_etape_sise", file = paste0(racine_packages, "apogee/data/conv_etape_sise.RData"))
  
  sise_diplome <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Diplome.xlsx"), nom_onglet = "Diplome_sise", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(importr::importer_table_access("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::filter(annee >= 2007) %>% 
    dplyr::select(-annee) %>% 
    unique()
  
  sise_diplome <- sise_diplome %>% 
    dplyr::anti_join(divr::doublons(sise_diplome, code_diplome), by = "code_diplome")
  
  save("sise_diplome", file = paste0(racine_packages, "apogee/data/sise_diplome.RData"))
  
  sise_discipline <- importr::importer_table_access("sise_discipline", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("sise_discipline", file = paste0(racine_packages, "apogee/data/sise_discipline.RData"))
}

#' data_diplome
#'
#' @export
#' @keywords internal
data_diplome <- function() {
  
  diplome <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Diplome.xlsx"), ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("diplome", file = paste0(racine_packages, "apogee/data/diplome.RData"))
  
  diplome_type <- importr::importer_table_access("diplome_type", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("diplome_type", file = paste0(racine_packages, "apogee/data/diplome_type.RData"))
  
  diplome_anterieur_type <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Diplome.xlsx"), nom_onglet = "Diplome_anterieur_origine", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::add_row(code_type_diplome_anterieur = NA_character_, lib_type_diplome_anterieur = "Non-ventilé")
  save("diplome_anterieur_type", file = paste0(racine_packages, "apogee/data/diplome_anterieur_type.RData"))
}

#' data_composante
#'
#' @export
#' @keywords internal
data_composante <- function() {
  
  composante <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Composante.xlsx"), ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::full_join(importr::importer_table_access("composante", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       dplyr::rename(lib_composante_maj = lib_composante),
                     by = "code_composante") %>% 
    dplyr::mutate(lib_composante = ifelse(!is.na(lib_composante_maj), lib_composante_maj, lib_composante)) %>% 
    dplyr::select(-lib_composante_maj) %>% 
    tidyr::drop_na(code_composante)
  save("composante", file = paste0(racine_packages, "apogee/data/composante.RData"))
  
  composante_type <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Composante.xlsx"), nom_onglet = "Composante_type", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    tidyr::drop_na(code_type_composante)
  save("composante_type", file = paste0(racine_packages, "apogee/data/composante_type.RData"))
}

#' data_diplome_version
#'
#' @export
#' @keywords internal
data_diplome_version <- function() {
  
  #### Diplôme version ####
  diplome_version <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Diplome_version.xlsx"), ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(importr::importer_table_access("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    tidyr::nest(code_domaine_diplome, .key = "code_domaine_diplome")
  save("diplome_version", file = paste0(racine_packages, "apogee/data/diplome_version.RData"))
  
  #### Domaine ####
  diplome_domaine <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Diplome_version.xlsx"), nom_onglet = "Formation_domaine", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::full_join(importr::importer_table_access("diplome_domaine", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       dplyr::rename(maj_lib_domaine_diplome = lib_domaine_diplome),
                     by = "code_domaine_diplome") %>% 
    dplyr::mutate(lib_domaine_diplome = ifelse(!is.na(maj_lib_domaine_diplome), maj_lib_domaine_diplome, lib_domaine_diplome)) %>% 
    dplyr::select(-maj_lib_domaine_diplome)
  save("diplome_domaine", file = paste0(racine_packages, "apogee/data/diplome_domaine.RData"))
  
  #### Finalité ####
  diplome_finalite <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Diplome_version.xlsx"), nom_onglet = "Finalite", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::bind_rows(importr::importer_table_access("diplome_finalite_ajout", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("diplome_finalite", file = paste0(racine_packages, "apogee/data/diplome_finalite.RData"))
  
  #### Mention ####
  diplome_mention <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Diplome_version.xlsx"), nom_onglet = "Mention", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::full_join(importr::importer_table_access("diplome_mention", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       dplyr::rename(maj_lib_mention_diplome = lib_mention_diplome),
                     by = "code_mention_diplome") %>% 
    dplyr::mutate(lib_mention_diplome = ifelse(!is.na(maj_lib_mention_diplome), maj_lib_mention_diplome, lib_mention_diplome)) %>% 
    dplyr::select(-maj_lib_mention_diplome)
  save("diplome_mention", file = paste0(racine_packages, "apogee/data/diplome_mention.RData"))
  
  #### Spécialité ####
  diplome_specialite <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Diplome_version.xlsx"), nom_onglet = "Specialite") %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    tidyr::drop_na(code_specialite_diplome)
  save("diplome_specialite", file = paste0(racine_packages, "apogee/data/diplome_specialite.RData"))
}

#' data_inscription
#'
#' @export
#' @keywords internal
data_inscription <- function() {
  
  profil_etudiant <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Inscription.xlsx"), nom_onglet = "Profil_etudiant", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    tidyr::drop_na(code_profil_etudiant)
  save("profil_etudiant", file = paste0(racine_packages, "apogee/data/profil_etudiant.RData"))
  
  regime_inscription <- importr::importer_table_access("regime_inscription", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("regime_inscription", file = paste0(racine_packages, "apogee/data/regime_inscription.RData"))
  
  statut_etudiant <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Inscription.xlsx"), nom_onglet = "Statut_etudiant", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    tidyr::drop_na(code_statut_etudiant)
  save("statut_etudiant", file = paste0(racine_packages, "apogee/data/statut_etudiant.RData"))
  
  # Sexe
  sexe <- importr::importer_table_access("sexe", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("sexe", file = paste0(racine_packages, "apogee/data/sexe.RData"))
  
  # PCS
  pcs <- importr::importer_table_access("pcs", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("pcs", file = paste0(racine_packages, "apogee/data/pcs.RData"))
  
  # Bac
  bac <- importr::importer_table_access("bac", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("bac", file = paste0(racine_packages, "apogee/data/bac.RData"))
  
  bac_mention <- importr::importer_table_access("bac_mention", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("bac_mention", file = paste0(racine_packages, "apogee/data/bac_mention.RData"))
  
  bourse <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Inscription.xlsx"), nom_onglet = "Bourse", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("bourse", file = paste0(racine_packages, "apogee/data/bourse.RData"))
  
  situation_sociale <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Inscription.xlsx"), nom_onglet = "Situation_sociale", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("situation_sociale", file = paste0(racine_packages, "apogee/data/situation_sociale.RData"))
  
  # Type d'établissement
  etablissement_type <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Individu.xlsx"), "Type établissement") %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::add_row(code_type_etab = NA_character_, lib_type_etab = "Non-ventilé")
  save("etablissement_type", file = paste0(racine_packages, "apogee/data/etablissement_type.RData"))
}

#' data_elp
#'
#' @export
#' @keywords internal
data_elp <- function() {
  
  # ELP
  elp <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/ELP.xlsx"), ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::bind_rows(importr::importer_table_access("elp_ajout", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::arrange(code_elp)
  divr::doublons(elp, code_elp)
  save("elp", file = paste0(racine_packages, "apogee/data/elp.RData"))
  
  # ELP nature
  elp_nature <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/ELP.xlsx"), "ELP - Nature", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("elp_nature", file = paste0(racine_packages, "apogee/data/elp_nature.RData"))
  
  # ELP Période
  elp_periode <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/ELP.xlsx"), "ELP - Periode", ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("elp_periode", file = paste0(racine_packages, "apogee/data/elp_periode.RData"))
  
  # ELP histo
  elp_histo <- importr::importer_table_access("elp_histo", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("elp_histo", file = paste0(racine_packages, "apogee/data/elp_histo.RData"))
  
}

#' data_resultat
#'
#' @export
#' @keywords internal
data_resultat <- function() {
  
  resultat <- importr::importer_fichier_excel(paste0(racine_packages, "apogee/raw/Resultat.xlsx"), ligne_debut = 2) %>% 
    source.maj::renommer_champs(importr::importer_table_access("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("resultat", file = paste0(racine_packages, "apogee/data/resultat.RData"))
  
}
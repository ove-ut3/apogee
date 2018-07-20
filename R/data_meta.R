#' data_etape
#'
#' @export
#' @keywords internal
data_etape <- function() {
  
  #### Etape ####
  
  annee_premiere_etape <- dplyr::bind_rows(apogee::inscrits, apogee::inscrits_cpge) %>% 
    dplyr::arrange(annee, code_etape) %>% 
    dplyr::select(code_etape, annee_premiere_etape = annee) %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::filter(dplyr::row_number() == 1) %>% 
    dplyr::ungroup()
  
  annee_derniere_etape <- dplyr::bind_rows(apogee::inscrits, apogee::inscrits_cpge) %>% 
    dplyr::arrange(annee, code_etape) %>% 
    dplyr::select(code_etape, annee_derniere_etape = annee) %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::filter(dplyr::row_number() == n()) %>% 
    dplyr::ungroup()
  
  etape_diplome_type <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Etape.xlsx"), "Etape_diplome_type", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::select(-annee) %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::filter(dplyr::row_number() == n()) %>% 
    dplyr::ungroup() %>% 
    divr::anti_join_bind(impexp::access_importer("etape_diplome_type_ajout", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                           dplyr::select(-date_maj), ., by = "code_etape")
  
  etape_composante <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Etape.xlsx"), "Etape_composante", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::anti_join(impexp::access_importer("etape_composante", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       tidyr::drop_na(suppression),
                     by = c("code_etape", "code_composante")) %>% 
    tidyr::nest(code_composante, .key = "code_composante") %>% 
    dplyr::mutate(code_composante = purrr::map(code_composante, 1))
  
  etape <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Etape.xlsx"), ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::supprimer_doublons_champ(temoin_annee1_diplome) %>% 
    dplyr::rename(lib_etape_apogee = lib_etape) %>% 
    dplyr::left_join(impexp::access_importer("etape", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")),
                     by = "code_etape") %>% 
    dplyr::left_join(etape_diplome_type, by = "code_etape") %>% 
    dplyr::left_join(etape_composante, by = "code_etape") %>% 
    source.maj::recoder_champs(impexp::access_importer("_recodage", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")), source = "data_etape") %>% 
    dplyr::mutate(lib_etape_apogee = ifelse(lib_etape != lib_etape_apogee, FALSE, TRUE)) %>% 
    dplyr::select(-annee_etape_apogee) %>% 
    dplyr::left_join(annee_premiere_etape, by = "code_etape") %>% 
    dplyr::left_join(annee_derniere_etape, by = "code_etape") %>% 
    dplyr::mutate(actif = dplyr::if_else(annee_derniere_etape == apogee::annee_en_cours(), TRUE, FALSE, FALSE))
  
  etape_ville <- etape %>% 
    dplyr::filter(actif,
                  is.na(ville),
                  code_type_diplome %in% c("DUT", "Licence pr")) %>% 
    tidyr::unnest(code_composante) %>% 
    dplyr::left_join(dplyr::rename(apogee::composante, ville_composante = ville), 
                     by = "code_composante") %>% 
    dplyr::mutate(ville = ifelse(!is.na(ville_composante), ville_composante, ville),
                  lib_etape_lower = tolower(lib_etape))
  
  jointure <- dplyr::select(etape_ville, code_etape, code_type_diplome, ville, annee_etape, lib_etape_lower)
  
  etape_ville2 <- etape_ville %>% 
    dplyr::select(lib_etape_lower, ville, code_type_diplome, annee_etape) %>% 
    unique() %>% 
    dplyr::group_by(lib_etape_lower, code_type_diplome, annee_etape) %>% 
    dplyr::filter(n() >= 2) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(jointure, by = c("lib_etape_lower", "code_type_diplome", "annee_etape", "ville"))
  
  etape <- etape %>% 
    dplyr::left_join(dplyr::select(etape_ville2, code_etape, ville_maj = ville), by = "code_etape") %>% 
    dplyr::mutate(ville = ifelse(!is.na(ville_maj), ville_maj, ville)) %>% 
    dplyr::select(-ville_maj) %>% 
    source.maj::recoder_champs(impexp::access_importer("_recodage", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")), source = "data_etape")
  
  divr::doublons(etape, code_etape)

  save("etape", file = paste0(racine_packages, "apogee/data/etape.RData"))
  
  # Mise à jour du champ ACTIF dans la base Access
  impexp::access_importer("etape", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>%
    dplyr::filter(is.na(actif)) %>%
    dplyr::semi_join(dplyr::filter(etape, actif), by = "code_etape") %>%
    dplyr::pull(code_etape) %>%
    paste0("UPDATE etape SET ACTIF = 'O' WHERE CODE_ETAPE = '", .,"';") %>%
    impexp::access_executer_sql(paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  
  impexp::access_importer("etape", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>%
    dplyr::filter(!is.na(actif)) %>%
    dplyr::semi_join(dplyr::filter(etape, !actif), by = "code_etape") %>%
    dplyr::pull(code_etape) %>%
    paste0("UPDATE etape SET ACTIF = '' WHERE CODE_ETAPE = '", .,"';") %>%
    impexp::access_executer_sql(paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  
  # Suppression des lignes obsolètes
  impexp::access_importer("etape", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>%
    dplyr::mutate(code_etape_succ = apogee::histo_etape_succ_2(code_etape),
                  type_diplome = apogee::hier_etape_type_diplome(code_etape) %>% 
                    apogee::acronyme_type_diplome(),
                  etape_derniere_annee = apogee::etape_derniere_annee(code_etape)) %>%
    tidyr::unnest(code_etape_succ) %>%
    dplyr::filter((type_diplome %in% c("DUT", "LP", "M2", "M2 enseignement", "Préparation concours") & etape_derniere_annee < enquete.ip::annee_en_cours()) |
                    (!type_diplome %in% c("DUT", "LP", "M2", "M2 enseignement", "Préparation concours") & etape_derniere_annee < apogee::annee_en_cours())) %>% 
    dplyr::group_by(code_etape) %>%
    dplyr::filter(n() == 1) %>% 
    dplyr::pull(code_etape) %>%
    paste0("DELETE FROM etape WHERE CODE_ETAPE = '", .,"';") %>%
    impexp::access_executer_sql(paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  
  impexp::access_importer("etape", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
    dplyr::anti_join(dplyr::bind_rows(apogee::inscrits, apogee::inscrits_cpge), 
                     by = "code_etape") %>% 
    dplyr::anti_join(apogee::diplomes, by = "code_etape") %>% 
    dplyr::pull(code_etape) %>% 
    paste0("DELETE FROM etape WHERE CODE_ETAPE = '", .,"';") %>%
    impexp::access_executer_sql(paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  
  
  #### Etape -Intégration SCUIO-IP ####
  
  etape_scuio <- impexp::excel_importer("raw/scuio-ip/20180413_TableauGeneralOF_PassageCFVU.xlsx", regex_onglet = "(LICENCE|LPRO|MASTER)", ligne_debut = 2) %>% 
    tidyr::unnest() %>% 
    dplyr::select(code_etape = code_apogee, lib_etape_scuio = parcours, acronyme_etape_scuio = acronyme) %>% 
    dplyr::filter(stringr::str_detect(code_etape, "^[A-Z\\d]{6}$"),
                  !code_etape %in% c("ELMAEE")) %>% # Mathématiques enseignement
    dplyr::mutate(lib_etape_scuio = stringr::str_remove_all(lib_etape_scuio, "\\r\\n")) %>% 
    tidyr::drop_na(lib_etape_scuio) %>% 
    unique() %>% 
    dplyr::right_join(impexp::access_importer("etape", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")),
                      by = "code_etape") %>% 
    dplyr::mutate(lib_etape = ifelse(!is.na(lib_etape_scuio), lib_etape_scuio, lib_etape),
                  acronyme_etape = ifelse(!is.na(acronyme_etape_scuio), acronyme_etape_scuio, acronyme_etape)) %>% 
    dplyr::select(-lib_etape_scuio, -acronyme_etape_scuio) %>% 
    dplyr::arrange(code_etape)
    
    divr::doublons(etape_scuio, code_etape)
  
  writexl::write_xlsx(etape_scuio, "etape_scuio.xlsx")
  
  #### Historique ####
  
  etape_histo <- impexp::access_importer("etape_histo", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
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
  
  etape_flux <- impexp::access_importer("etape_flux", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("etape_flux", file = paste0(racine_packages, "apogee/data/etape_flux.RData"))
  
  #### Etape - mention ####
  
  etape_mention <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Etape.xlsx"), "Etape_mention", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::bind_rows(impexp::access_importer("etape_mention_diplome", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::arrange(code_etape, code_mention_diplome)
  
  etape_mention <- etape_mention %>% 
    dplyr::filter(is.na(suppression)) %>% 
    dplyr::select(-suppression) %>% 
    dplyr::left_join(dplyr::filter(etape_mention, !is.na(suppression)), by = c("code_etape", "code_mention_diplome")) %>% 
    dplyr::mutate(code_mention_diplome = ifelse(!is.na(suppression), NA_character_, code_mention_diplome)) %>% 
    dplyr::select(-suppression) %>% 
    dplyr::arrange(code_etape, code_mention_diplome) %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::filter(!is.na(code_mention_diplome) | dplyr::row_number() == 1) %>% 
    unique()
  
  divr::doublons(etape_mention, code_etape, code_mention_diplome)
  
  save("etape_mention", file = paste0(racine_packages, "apogee/data/etape_mention.RData"))
  
  #### Etape - domaine ####
  
  etape_domaine <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Etape.xlsx"), "Etape_domaine", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::bind_rows(impexp::access_importer("etape_domaine_diplome", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::arrange(code_etape, code_domaine_diplome)
  
  etape_domaine <- etape_domaine %>% 
    dplyr::filter(is.na(suppression)) %>% 
    dplyr::select(-suppression) %>% 
    dplyr::left_join(dplyr::filter(etape_domaine, !is.na(suppression)), by = c("code_etape", "code_domaine_diplome")) %>% 
    dplyr::mutate(code_domaine_diplome = ifelse(!is.na(suppression), NA_character_, code_domaine_diplome)) %>% 
    dplyr::select(-suppression) %>% 
    dplyr::arrange(code_etape, code_domaine_diplome) %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::filter(!is.na(code_domaine_diplome) | dplyr::row_number() == 1)
  
  divr::doublons(etape_domaine, code_etape, code_domaine_diplome)
  
  save("etape_domaine", file = paste0(racine_packages, "apogee/data/etape_domaine.RData"))
  
  #### Etape - finalité ####
  etape_finalite <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Etape.xlsx"), "Etape_finalite", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::recoder_champs(impexp::access_importer("_recodage", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")), source = "data_etape_finalite") %>% 
    dplyr::arrange(code_etape, code_finalite_diplome) %>% 
    dplyr::group_by(code_etape) %>% 
    dplyr::filter(!is.na(code_finalite_diplome) | dplyr::row_number() == 1) %>% 
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
  
  #### Etape - discipline SISE ####
  etape_sise_discipline <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Etape.xlsx"), "Etape_discipline_sise") %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::mutate(code_discipline_sise = stringr::str_remove(code_discipline_sise, "^00") %>% 
                    stringr::str_c("SD", .)) %>% 
    dplyr::bind_rows(impexp::access_importer("etape_discipline_sise", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       dplyr::filter(is.na(suppression)) %>% 
                       dplyr::select(-suppression)) %>% 
    dplyr::left_join(impexp::access_importer("etape_discipline_sise", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")), by = c("code_etape", "code_discipline_sise")) %>% 
    dplyr::filter(is.na(suppression)) %>% 
    dplyr::select(-suppression)
  
  save("etape_sise_discipline", file = paste0(racine_packages, "apogee/data/etape_sise_discipline.RData"))
  
  #### Etape - secteur DUT et LP ####
  
  etape_secteur <- impexp::access_importer("etape_secteur", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("etape_secteur", file = paste0(racine_packages, "apogee/data/etape_secteur.RData"))
  
  #### Etape - spécialité ####
  
  etape_specialite_diplome <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Etape.xlsx"), "Etape_specialite") %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::anti_join(dplyr::filter(., !is.na(code_specialite_diplome)) %>% 
                       dplyr::mutate(code_specialite_diplome = NA_character_),
                     by = c("code_etape", "code_specialite_diplome"))
  save("etape_specialite_diplome", file = paste0(racine_packages, "apogee/data/etape_specialite_diplome.RData"))
  
  #### Etape - cycle ####
  
  etape_cycle <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Etape.xlsx"), "Etape_cycle") %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("etape_cycle", file = paste0(racine_packages, "apogee/data/etape_cycle.RData"))
  
  #### Cycle
  cycle <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Etape.xlsx"), "Cycle") %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("cycle", file = paste0(racine_packages, "apogee/data/cycle.RData"))
  
}

#' data_sise
#'
#' @export
#' @keywords internal
data_sise <- function() {
  
  conv_etape_sise <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Etape.xlsx"), "Etape_sise", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::filter(annee >= 2007) %>% 
    dplyr::mutate(code_diplome_sise = as.character(code_diplome_sise)) %>% 
    dplyr::left_join(impexp::access_importer("diplome_sise", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       tidyr::drop_na(annee),
                     by = c("annee", "code_diplome_sise" = "code_diplome")) %>% 
    dplyr::mutate(code_diplome_sise = ifelse(!is.na(code_diplome_maj), code_diplome_maj, code_diplome_sise)) %>% 
    dplyr::select(code_etape, annee, code_diplome_sise) %>% 
    dplyr::left_join(impexp::access_importer("diplome_sise", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       dplyr::filter(is.na(annee)) %>% 
                       dplyr::select(-annee),
                     by = c("code_diplome_sise" = "code_diplome")) %>% 
    dplyr::mutate(code_diplome_sise = ifelse(!is.na(code_diplome_maj), code_diplome_maj, code_diplome_sise)) %>% 
    dplyr::select(code_etape, annee, code_diplome_sise) %>% 
    dplyr::left_join(impexp::access_importer("etape_diplome_sise", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       dplyr::filter(is.na(annee)) %>% 
                       dplyr::select(-annee),
                     by = "code_etape") %>% 
    dplyr::mutate(code_diplome_sise = ifelse(!is.na(code_diplome_maj), code_diplome_maj, code_diplome_sise)) %>% 
    dplyr::select(code_etape, annee, code_diplome_sise) %>% 
    unique()
  save("conv_etape_sise", file = paste0(racine_packages, "apogee/data/conv_etape_sise.RData"))
  
  sise_diplome <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Diplome.xlsx"), "Diplome_sise", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::filter(annee >= 2007) %>% 
    dplyr::select(-annee) %>% 
    unique()
  
  sise_diplome <- sise_diplome %>% 
    dplyr::anti_join(divr::doublons(sise_diplome, code_diplome), by = "code_diplome")
  
  save("sise_diplome", file = paste0(racine_packages, "apogee/data/sise_diplome.RData"))
  
  sise_discipline <- impexp::access_importer("sise_discipline", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("sise_discipline", file = paste0(racine_packages, "apogee/data/sise_discipline.RData"))
  
  sise_diplome_lib <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Diplome.xlsx"), "Diplome_sise_lib", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  
  save("sise_diplome_lib", file = paste0(racine_packages, "apogee/data/sise_diplome_lib.RData"))
}

#' data_diplome
#'
#' @export
#' @keywords internal
data_diplome <- function() {
  
  diplome <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Diplome.xlsx"), ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("diplome", file = paste0(racine_packages, "apogee/data/diplome.RData"))
  
  diplome_type <- impexp::access_importer("diplome_type", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("diplome_type", file = paste0(racine_packages, "apogee/data/diplome_type.RData"))
  
  diplome_anterieur_type <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Diplome.xlsx"), "Diplome_anterieur_origine", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::add_row(code_type_diplome_anterieur = NA_character_, lib_type_diplome_anterieur = "Non-ventilé")
  save("diplome_anterieur_type", file = paste0(racine_packages, "apogee/data/diplome_anterieur_type.RData"))
}

#' data_composante
#'
#' @export
#' @keywords internal
data_composante <- function() {
  
  composante <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Composante.xlsx"), ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::full_join(impexp::access_importer("composante", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       dplyr::rename(lib_composante_maj = lib_composante),
                     by = "code_composante") %>% 
    dplyr::mutate(lib_composante = ifelse(!is.na(lib_composante_maj), lib_composante_maj, lib_composante)) %>% 
    dplyr::select(-lib_composante_maj) %>% 
    tidyr::drop_na(code_composante)
  save("composante", file = paste0(racine_packages, "apogee/data/composante.RData"))
  
  composante_type <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Composante.xlsx"), "Composante_type", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    tidyr::drop_na(code_type_composante)
  save("composante_type", file = paste0(racine_packages, "apogee/data/composante_type.RData"))
  
  composante_histo <- impexp::access_importer("composante_histo", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("composante_histo", file = paste0(racine_packages, "apogee/data/composante_histo.RData"))
  
}

#' data_diplome_version
#'
#' @export
#' @keywords internal
data_diplome_version <- function() {
  
  #### Diplôme version ####
  diplome_version <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Diplome_version.xlsx"), ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    source.maj::transcoder_champs(impexp::access_importer("_contents", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    tidyr::nest(code_domaine_diplome, .key = "code_domaine_diplome") %>% 
    dplyr::mutate(code_domaine_diplome = purrr::map(code_domaine_diplome, ~ .[[1]]))
  save("diplome_version", file = paste0(racine_packages, "apogee/data/diplome_version.RData"))
  
  #### Domaine ####
  diplome_domaine <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Diplome_version.xlsx"), "Formation_domaine", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::full_join(impexp::access_importer("diplome_domaine", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       dplyr::rename(maj_lib_domaine_diplome = lib_domaine_diplome),
                     by = "code_domaine_diplome") %>% 
    dplyr::mutate(lib_domaine_diplome = ifelse(!is.na(maj_lib_domaine_diplome), maj_lib_domaine_diplome, lib_domaine_diplome)) %>% 
    dplyr::select(-maj_lib_domaine_diplome)
  save("diplome_domaine", file = paste0(racine_packages, "apogee/data/diplome_domaine.RData"))
  
  #### Finalité ####
  diplome_finalite <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Diplome_version.xlsx"), "Finalite", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::bind_rows(impexp::access_importer("diplome_finalite_ajout", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("diplome_finalite", file = paste0(racine_packages, "apogee/data/diplome_finalite.RData"))
  
  #### Mention ####
  diplome_mention <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Diplome_version.xlsx"), "Mention", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::full_join(impexp::access_importer("diplome_mention", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")) %>% 
                       dplyr::rename(maj_lib_mention_diplome = lib_mention_diplome),
                     by = "code_mention_diplome") %>% 
    dplyr::mutate(lib_mention_diplome = ifelse(!is.na(maj_lib_mention_diplome), maj_lib_mention_diplome, lib_mention_diplome)) %>% 
    dplyr::select(-maj_lib_mention_diplome)
  save("diplome_mention", file = paste0(racine_packages, "apogee/data/diplome_mention.RData"))
  
  #### Mention - historique ####
  
  diplome_mention_histo <- impexp::access_importer("diplome_mention_histo", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("diplome_mention_histo", file = paste0(racine_packages, "apogee/data/diplome_mention_histo.RData"))
  
  #### Mention - compatibilité licence et master ####
  
  diplome_mention_lm <- impexp::access_importer("diplome_mention_lm", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("diplome_mention_lm", file = paste0(racine_packages, "apogee/data/diplome_mention_lm.RData"))
  
  #### Spécialité ####
  diplome_specialite <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Diplome_version.xlsx"), "Specialite") %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    tidyr::drop_na(code_specialite_diplome)
  save("diplome_specialite", file = paste0(racine_packages, "apogee/data/diplome_specialite.RData"))
}

#' data_inscription
#'
#' @export
#' @keywords internal
data_inscription <- function() {
  
  profil_etudiant <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Inscription.xlsx"), "Profil_etudiant", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    tidyr::drop_na(code_profil_etudiant)
  save("profil_etudiant", file = paste0(racine_packages, "apogee/data/profil_etudiant.RData"))
  
  regime_inscription <- impexp::access_importer("regime_inscription", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("regime_inscription", file = paste0(racine_packages, "apogee/data/regime_inscription.RData"))
  
  statut_etudiant <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Inscription.xlsx"), "Statut_etudiant", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    tidyr::drop_na(code_statut_etudiant)
  save("statut_etudiant", file = paste0(racine_packages, "apogee/data/statut_etudiant.RData"))
  
  # Sexe
  sexe <- impexp::access_importer("sexe", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("sexe", file = paste0(racine_packages, "apogee/data/sexe.RData"))
  
  # PCS
  pcs <- impexp::access_importer("pcs", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("pcs", file = paste0(racine_packages, "apogee/data/pcs.RData"))
  
  # Bac
  bac <- impexp::access_importer("bac", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("bac", file = paste0(racine_packages, "apogee/data/bac.RData"))
  
  bac_mention <- impexp::access_importer("bac_mention", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("bac_mention", file = paste0(racine_packages, "apogee/data/bac_mention.RData"))
  
  bourse <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Inscription.xlsx"), "Bourse", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("bourse", file = paste0(racine_packages, "apogee/data/bourse.RData"))
  
  situation_sociale <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Inscription.xlsx"), "Situation_sociale", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("situation_sociale", file = paste0(racine_packages, "apogee/data/situation_sociale.RData"))
  
  # Type d'établissement
  etablissement_type <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Individu.xlsx"), "Type établissement") %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::add_row(code_type_etab = NA_character_, lib_type_etab = "Non-ventilé")
  save("etablissement_type", file = paste0(racine_packages, "apogee/data/etablissement_type.RData"))
}

#' data_elp
#'
#' @export
#' @keywords internal
data_elp <- function() {
  
  # ELP
  elp <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/ELP.xlsx"), ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::bind_rows(impexp::access_importer("elp_ajout", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))) %>% 
    dplyr::arrange(code_elp)
  divr::doublons(elp, code_elp)
  save("elp", file = paste0(racine_packages, "apogee/data/elp.RData"))
  
  # ELP nature
  elp_nature <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/ELP.xlsx"), "ELP - Nature", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("elp_nature", file = paste0(racine_packages, "apogee/data/elp_nature.RData"))
  
  # ELP Période
  elp_periode <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/ELP.xlsx"), "ELP - Periode", ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("elp_periode", file = paste0(racine_packages, "apogee/data/elp_periode.RData"))
  
  # ELP histo
  elp_histo <- impexp::access_importer("elp_histo", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("elp_histo", file = paste0(racine_packages, "apogee/data/elp_histo.RData"))
  
}

#' data_resultat
#'
#' @export
#' @keywords internal
data_resultat <- function() {
  
  resultat <- impexp::excel_importer(paste0(racine_packages, "apogee/raw/Resultat.xlsx"), ligne_debut = 2) %>% 
    source.maj::renommer_champs(impexp::access_importer("_rename", paste0(racine_packages, "apogee/raw/Tables_ref.accdb")))
  save("resultat", file = paste0(racine_packages, "apogee/data/resultat.RData"))
  
}

#' data_academie
#'
#' @export
#' @keywords internal
data_academie <- function() {
  
  academie <- impexp::access_importer("academie", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("academie", file = paste0(racine_packages, "apogee/data/academie.RData"))
  
  departement_academie <- impexp::access_importer("departement_academie", paste0(racine_packages, "apogee/raw/Tables_ref.accdb"))
  save("departement_academie", file = paste0(racine_packages, "apogee/data/departement_academie.RData"))
}
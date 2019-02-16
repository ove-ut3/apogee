# Mise à jour du champ ACTIF dans la base Access
impexp::access_import("etape", "data-raw/Tables_ref.accdb") %>%
  dplyr::filter(is.na(actif)) %>%
  dplyr::semi_join(dplyr::filter(etape, actif), by = "code_etape") %>%
  dplyr::pull(code_etape) %>%
  paste0("UPDATE etape SET ACTIF = 'O' WHERE CODE_ETAPE = '", .,"';") %>%
  impexp::access_execute_sql("data-raw/Tables_ref.accdb")

impexp::access_import("etape", "data-raw/Tables_ref.accdb") %>%
  dplyr::filter(!is.na(actif)) %>%
  dplyr::semi_join(dplyr::filter(etape, !actif), by = "code_etape") %>%
  dplyr::pull(code_etape) %>%
  paste0("UPDATE etape SET ACTIF = '' WHERE CODE_ETAPE = '", .,"';") %>%
  impexp::access_execute_sql("data-raw/Tables_ref.accdb")

# Suppression des lignes obsolètes
impexp::access_import("etape", "data-raw/Tables_ref.accdb") %>%
  dplyr::mutate(code_etape_succ = apogee::histo_etape_succ_2(code_etape),
                type_diplome = apogee::hier_etape_type_diplome(code_etape) %>% 
                  apogee::acronyme_type_diplome(),
                etape_derniere_annee = apogee::etape_derniere_annee(code_etape)) %>%
  tidyr::unnest(code_etape_succ) %>%
  dplyr::filter((type_diplome %in% c("DUT", "LP", "M2", "M2 enseignement", "Préparation concours") & etape_derniere_annee < apogee::annee_en_cours() - 4) |
                  (!type_diplome %in% c("DUT", "LP", "M2", "M2 enseignement", "Préparation concours") & etape_derniere_annee < apogee::annee_en_cours())) %>% 
  dplyr::group_by(code_etape) %>%
  dplyr::filter(dplyr::n() == 1) %>% 
  dplyr::pull(code_etape) %>%
  paste0("DELETE FROM etape WHERE CODE_ETAPE = '", .,"';") %>%
  impexp::access_execute_sql("data-raw/Tables_ref.accdb")

impexp::access_import("etape", "data-raw/Tables_ref.accdb") %>% 
  dplyr::anti_join(dplyr::bind_rows(apogee::inscrits, apogee::inscrits_cpge), 
                   by = "code_etape") %>% 
  dplyr::anti_join(apogee::diplomes, by = "code_etape") %>% 
  dplyr::pull(code_etape) %>% 
  paste0("DELETE FROM etape WHERE CODE_ETAPE = '", .,"';") %>%
  impexp::access_execute_sql("data-raw/Tables_ref.accdb")
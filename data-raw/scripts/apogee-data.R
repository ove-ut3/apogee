source("data-raw/scripts/utils.R")

#### Inscrits ####

inscrits <- list.files("data-raw", pattern = "Inscrits\\.zip$", recursive = TRUE, full.names = TRUE) %>% 
  paste("unzip -p", .) %>% 
  purrr::map_df( ~ data.table::fread(cmd = ., skip = 1, encoding = "UTF-8")) %>% 
  dplyr::as_tibble() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>% 
  doublon_maj_etudiant() %>%
  patchr::recode_formula(
    impexp::access_import("_recodage", access_base_path) %>%
      dplyr::filter(source == "data_inscrits")
  ) %>%
  dplyr::mutate(
    inscription_annulee = !inscription_en_cours | !is.na(date_annulation) | inscription_resiliee | !is.na(date_resiliation),
    inscription_annulee = dplyr::if_else(is.na(inscription_annulee), FALSE, inscription_annulee)
  )

inscrits_annules <- inscrits %>%
  dplyr::filter(inscription_annulee) %>%
  dplyr::select(-inscription_annulee) %>%
  tidyr::nest(code_composante = code_composante, code_version_etape = code_version_etape) %>%
  dplyr::mutate_at(c("code_composante", "code_version_etape"), purrr::map, 1)

usethis::use_data(inscrits_annules, overwrite = TRUE)

inscrits <- inscrits %>%
  dplyr::filter(!inscription_annulee) %>%
  dplyr::select(-inscription_annulee) %>%
  tidyr::nest(code_composante = code_composante, code_version_etape = code_version_etape) %>%
  dplyr::mutate_at(c("code_composante", "code_version_etape"), purrr::map, 1)

# Ajout ELP parcours
elp_parcours <- apogee::inscrits_elp %>%
  dplyr::inner_join(
    apogee::etape_histo %>%
      tidyr::drop_na(code_elp) %>%
      dplyr::bind_rows(impexp::access_import("elp_parcours", access_base_path)) %>%
      dplyr::mutate(elp_parcours = code_elp) %>%
      tidyr::separate_rows(code_elp, sep = ";"),
    by = c("code_etape", "code_elp")
  ) %>%
  dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, elp_parcours, ville) %>%
  unique() %>%
  dplyr::anti_join(
    patchr::duplicate(., annee, code_etape, code_etudiant, inscription_premiere),
    by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")
  )

inscrits <- inscrits %>%
  dplyr::left_join(elp_parcours, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

# Ajout inscrits Base Access

inscrits <- impexp::access_import("inscrits_ajout", "data-raw/data/Tables_ref_individus.accdb") %>%
  dplyr::select(-commentaire, -date_maj) %>%
  dplyr::anti_join(inscrits, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>%
  tidyr::nest(code_composante = code_composante, code_version_etape = code_version_etape) %>%
  dplyr::mutate_at(c("code_composante", "code_version_etape"), purrr::map, 1) %>%
  dplyr::bind_rows(inscrits) %>%
  dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere)

# CPGE

inscrits_cpge <- inscrits %>%
  dplyr::left_join(apogee::etape %>%
    dplyr::select(code_etape, particularite),
  by = "code_etape"
  ) %>%
  dplyr::filter(code_profil_etudiant == "CP" | particularite == "CPGE") %>%
  dplyr::select(-particularite)

usethis::use_data(inscrits_cpge, overwrite = TRUE)

inscrits <- inscrits %>%
  dplyr::anti_join(inscrits_cpge, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

# Sauvegarde finale

usethis::use_data(inscrits, overwrite = TRUE)

#### Inscrits péda ####

inscrits_peda <- list.files("data-raw", pattern = "Inscrits_peda\\.zip$", recursive = TRUE, full.names = TRUE) %>% 
  paste("unzip -p", .) %>% 
  purrr::map_df( ~ data.table::fread(cmd = ., skip = 1, encoding = "UTF-8")) %>% 
  dplyr::as_tibble() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>%
  doublon_maj_etudiant()

usethis::use_data(inscrits_peda, overwrite = TRUE)

#### Inscrits ELP ####

inscrits_elp <- list.files("data-raw", pattern = "Inscrits_ELP.*?\\.zip$", recursive = TRUE, full.names = TRUE) %>% 
  paste("unzip -p", .) %>% 
  purrr::map_df( ~ data.table::fread(cmd = ., skip = 1, encoding = "UTF-8")) %>% 
  dplyr::as_tibble() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>%
  doublon_maj_etudiant()

usethis::use_data(inscrits_elp, overwrite = TRUE)

#### Résultats ELP ####

resultats_elp <- list.files("data-raw", pattern = "Resultats_ELP.*?\\.zip$", recursive = TRUE, full.names = TRUE) %>% 
  paste("unzip -p", .) %>% 
  purrr::map_df( ~ data.table::fread(cmd = ., skip = 1, encoding = "UTF-8")) %>% 
  dplyr::as_tibble() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>%
  doublon_maj_etudiant() %>%
  dplyr::mutate(
    present_examen = dplyr::case_when(
      note > 0 | apogee::hier_resultat_parent(code_resultat) == "ADM" ~ TRUE,
      TRUE ~ FALSE
    )
  )

usethis::use_data(resultats_elp, overwrite = TRUE)

#### Résultats Etape ####

resultats_etape <- list.files("data-raw", pattern = "Resultats_etape\\.zip$", recursive = TRUE, full.names = TRUE) %>% 
  paste("unzip -p", .) %>% 
  purrr::map_df( ~ data.table::fread(cmd = ., skip = 1, encoding = "UTF-8")) %>% 
  dplyr::as_tibble() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>%
  doublon_maj_etudiant()

# PACES
resultats_paces <- list.files("data-raw", pattern = "Resultats_etape_paces\\.zip$", recursive = TRUE, full.names = TRUE) %>% 
  paste("unzip -p", .) %>% 
  purrr::map_df( ~ data.table::fread(cmd = ., skip = 1, encoding = "UTF-8")) %>% 
  dplyr::as_tibble() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>%
  doublon_maj_etudiant()

resultats_etape <- resultats_etape %>%
  dplyr::anti_join(resultats_paces, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>%
  dplyr::bind_rows(resultats_paces)

# doublons session

doublons <- resultats_etape %>%
  patchr::duplicate(annee, code_etape, code_etudiant, inscription_premiere, lib_session) %>%
  dplyr::mutate(admis = dplyr::if_else(apogee::hier_resultat_parent(code_resultat) == "ADM", 1L, NA_integer_)) %>%
  dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere, lib_session, admis) %>%
  dplyr::group_by(annee, code_etape, code_etudiant, inscription_premiere, lib_session) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-admis)

resultats_etape <- resultats_etape %>%
  dplyr::anti_join(doublons, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere", "lib_session")) %>%
  dplyr::bind_rows(doublons) %>%
  dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere, lib_session)

# Supression de Session 2 si : résultat Session 1 ADM et résultat Session 2 non-ADM

suppression_session2 <- apogee::resultats_etape %>%
  dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, lib_session, code_resultat) %>%
  tidyr::spread(lib_session, code_resultat) %>%
  janitor::clean_names() %>%
  dplyr::filter(apogee::hier_resultat_parent(session_1) == "ADM" & !apogee::hier_resultat_parent(session_2) %in% c(NA_character_, "ADM")) %>%
  dplyr::mutate(lib_session = "Session 2")

resultats_etape <- resultats_etape %>%
  dplyr::anti_join(suppression_session2, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere", "lib_session"))

# Supprimer les résultats étape non-existants chez les inscrits
resultats_etape <- resultats_etape %>%
  dplyr::semi_join(apogee::inscrits, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere"))

# Sauvegarde

usethis::use_data(resultats_etape, overwrite = TRUE)

#### Résultats diplôme ####

resultats_diplome <- list.files("data-raw", pattern = "Resultats_diplome\\.zip$", recursive = TRUE, full.names = TRUE) %>% 
  paste("unzip -p", .) %>% 
  purrr::map_df( ~ data.table::fread(cmd = ., skip = 1, encoding = "UTF-8")) %>% 
  dplyr::as_tibble() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>% 
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>%
  doublon_maj_etudiant()

# Ajout diplômés Base Access

ajout_diplomes <- impexp::access_import("diplomes_ajout", "data-raw/data/Tables_ref_individus.accdb") %>%
  dplyr::select(-commentaire, -date_maj)

resultats_diplome <- resultats_diplome %>%
  dplyr::anti_join(ajout_diplomes, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>%
  dplyr::bind_rows(ajout_diplomes) %>%
  dplyr::arrange(annee, code_etape, code_etudiant, inscription_premiere)

# Suppression diplômés non-existants chez les inscrits

resultats_diplome <- resultats_diplome %>%
  dplyr::semi_join(dplyr::select(apogee::inscrits, annee, code_etape, code_etudiant, inscription_premiere),
    by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")
  )

# Suppression des étudiants de L1, L2 et M1

resultats_diplome <- resultats_diplome %>%
  dplyr::filter(!apogee::hier_etape_type_diplome(code_etape) %in% c("LMD/L1", "LMD/L2", "LMD/M1", "LMD/M1 ENS"))

# Suppression des étudiants de DUT en 1ère année

resultats_diplome <- resultats_diplome %>%
  dplyr::filter(
    !(apogee::hier_etape_type_diplome(code_etape) == "DUT" & apogee::annee_etape(code_etape) == 1)
  )

# Suppression des sessions multiples doublons

resultats_diplome <- resultats_diplome %>%
  dplyr::filter(!(lib_session %in% c("Session 2", "Session unique") & session == "Session 1"))

usethis::use_data(resultats_diplome, overwrite = TRUE)

#### Individus ####

individus <- data.table::fread(cmd = "unzip -p data-raw/data/Toutes_annees/Individus.zip", skip = 1, encoding = "UTF-8") %>%
  dplyr::as_tibble() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>%
  dplyr::mutate_at("date_naissance", patchr::fix_birth_date) %>%
  dplyr::semi_join(dplyr::bind_rows(inscrits, inscrits_annules, inscrits_cpge),
    by = "code_etudiant"
  )

individus_bac <- data.table::fread(cmd = "unzip -p data-raw/data/Toutes_annees/Individus_bac.zip", skip = 1, encoding = "UTF-8") %>%
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>%
  dplyr::arrange(code_etudiant, desc(annee_bac), code_mention_bac, code_type_etab_bac) %>%
  dplyr::group_by(code_etudiant) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::ungroup()

individus <- dplyr::left_join(individus, individus_bac, by = "code_etudiant")

# Ajout des DAEU B pour l'année du bac
annee_bac_daeu <- individus %>%
  dplyr::filter(is.na(annee_bac)) %>%
  dplyr::select(code_etudiant) %>%
  dplyr::inner_join(
    apogee::resultats_diplome %>%
      dplyr::filter(
        apogee::hier_resultat_parent(code_resultat) == "ADM",
        apogee::hier_etape_type_diplome(code_etape) == "DAEU"
      ) %>%
      dplyr::group_by(code_etudiant) %>% 
      dplyr::filter(dplyr::row_number() == 1) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        code_bac = "DAEB",
        code_type_etab_bac = "00",
        code_etab_bac = "0311384L",
        code_departement_bac = "031"
      ) %>%
      dplyr::select(code_etudiant, annee_bac = annee, code_bac, code_type_etab_bac, code_etab_bac, code_departement_bac),
    by = "code_etudiant"
  )

individus <- patchr::df_update(individus, annee_bac_daeu, by = "code_etudiant")

# Ajout des diplômes d'accès à l'enseignement supérieur étrangers pour l'année du bac
annee_bac_etr <- individus %>%
  dplyr::filter(is.na(annee_bac)) %>%
  dplyr::select(code_etudiant) %>%
  dplyr::inner_join(
    apogee::individus_diplome_origine %>%
      dplyr::filter(
        code_type_diplome_origine == "1",
        annee_diplome_obtenu >= 2000,
        as.integer(substr(code_etudiant, 2, 3)) > as.integer(substr(annee_diplome_obtenu, 2, 3)),
        stringr::str_detect(code_departement_pays_dernier_diplome, "^[1-9]") | code_departement_pays_dernier_diplome %in% c(NA_character_, "099")
      ) %>%
      dplyr::group_by(code_etudiant) %>% 
      dplyr::filter(dplyr::row_number() == 1) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(code_bac = "0031") %>%
      dplyr::select(code_etudiant, annee_bac = annee_diplome_obtenu, code_bac, code_etab_bac = code_etab_diplome_obtenu),
    by = "code_etudiant"
  )

individus <- patchr::df_update(individus, annee_bac_etr, by = "code_etudiant")

individus_mail_ups <- data.table::fread(cmd = "unzip -p data-raw/data/Toutes_annees/Individus_mail_ups.zip", skip = 1, encoding = "UTF-8") %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  patchr::transcode(impexp::access_import("_contents", access_base_path))

individus <- dplyr::left_join(individus, individus_mail_ups, by = "code_etudiant")

individus_departement_naissance <- data.table::fread(cmd = "unzip -p data-raw/data/Toutes_annees/Individus_departement_naissance.zip", skip = 1, encoding = "UTF-8") %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  patchr::transcode(impexp::access_import("_contents", access_base_path))

individus <- dplyr::left_join(individus, individus_departement_naissance, by = "code_etudiant")

usethis::use_data(individus, overwrite = TRUE)

#### Individus - origine ####

individus_diplome_anterieur <- data.table::fread(cmd = "unzip -p data-raw/data/Toutes_annees/Individus_diplome_origine.zip", skip = 1, encoding = "UTF-8") %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>%
  dplyr::arrange(code_etudiant, annee_diplome_obtenu)

individus_diplome_externe <- data.table::fread(cmd = "unzip -p data-raw/data/Toutes_annees/Individus_diplome_externe.zip", skip = 1, encoding = "UTF-8") %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>%
  dplyr::mutate(code_type_diplome_externe = stringr::str_pad(code_type_diplome_externe, 3, "left", "0")) %>%
  dplyr::arrange(code_etudiant, annee_diplome_externe) %>%
  tidyr::drop_na(code_type_diplome_externe)

individus_diplome_origine <- individus_diplome_anterieur %>%
  dplyr::full_join(individus_diplome_externe,
    by = c("code_etudiant", c("annee_diplome_obtenu" = "annee_diplome_externe"))
  ) %>%
  dplyr::left_join(impexp::access_import("diplome_externe_anterieur", access_base_path) %>%
    dplyr::rename(code_type_diplome_anterieur_maj = code_type_diplome_anterieur),
  by = "code_type_diplome_externe"
  ) %>%
  dplyr::mutate(
    code_type_diplome_anterieur = dplyr::case_when(
      code_type_diplome_anterieur %in% c(NA_character_, "N", "U", "X") ~ code_type_diplome_anterieur_maj,
      code_type_diplome_anterieur == "N" & !is.na(code_type_diplome_externe) ~ "LI",
      TRUE ~ code_type_diplome_anterieur
    )
  ) %>%
  tidyr::drop_na(code_type_diplome_anterieur) %>%
  dplyr::mutate(ordre = dplyr::case_when(
    code_type_diplome_anterieur == code_type_diplome_anterieur_maj ~ 2,
    code_type_diplome_anterieur %in% c("I", "Q", "N", "U", "X") ~ 3,
    code_type_diplome_anterieur %in% c("Y", "Z") ~ 4,
    TRUE ~ 1
  )) %>%
  dplyr::arrange(code_etudiant, annee_diplome_obtenu, ordre) %>%
  dplyr::group_by(code_etudiant, annee_diplome_obtenu) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::select(code_etudiant, annee_diplome_obtenu, code_type_diplome_origine = code_type_diplome_anterieur, code_etab_diplome_obtenu, code_departement_pays_dernier_diplome) %>%
  dplyr::semi_join(
    dplyr::bind_rows(inscrits, inscrits_annules, inscrits_cpge),
    by = "code_etudiant"
  )

usethis::use_data(individus_diplome_origine, overwrite = TRUE)

individus_situation_annee_precedente <- data.table::fread(cmd = "unzip -p data-raw/data/Toutes_annees/Individus_situation_annee_precedente.zip", skip = 1, encoding = "UTF-8") %>% 
  dplyr::as_tibble() %>% 
  patchr::rename(impexp::access_import("_rename", access_base_path)) %>%
  patchr::transcode(impexp::access_import("_contents", access_base_path)) %>%
  dplyr::semi_join(
    dplyr::bind_rows(inscrits, inscrits_annules, inscrits_cpge),
    by = "code_etudiant"
  )

usethis::use_data(individus_situation_annee_precedente, overwrite = TRUE)

#### Stats ####

stats <- apogee::inscrits %>%
  dplyr::left_join(apogee::individus, by = "code_etudiant") %>% 
  dplyr::left_join(apogee::inscrits_peda %>% 
                     dplyr::mutate(inscrits_peda = 1),
                   by = c("annee", "code_etape", "code_etudiant")) %>% 
  dplyr::left_join(apogee::inscrits_cpge %>% 
                     dplyr::select(annee, code_etape, code_etudiant) %>% 
                     dplyr::mutate(inscrits_cpge = 1),
                   by = c("annee", "code_etape", "code_etudiant")) %>% 
  dplyr::left_join(apogee::individus_diplome_origine %>% 
                     dplyr::group_by(code_etudiant) %>% 
                     dplyr::filter(dplyr::row_number() == n()) %>% 
                     dplyr::ungroup(),
                   by = "code_etudiant") %>% 
  dplyr::left_join(apogee::resultats_etape %>% 
                     dplyr::group_by(annee, code_etape, code_etudiant, inscription_premiere) %>%
                     dplyr::filter(dplyr::row_number() == n()) %>%
                     dplyr::ungroup(),
                   by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
  dplyr::left_join(apogee::resultats_diplome %>% 
                     dplyr::group_by(annee, code_etape, code_etudiant, inscription_premiere) %>%
                     dplyr::filter(dplyr::row_number() == n()) %>%
                     dplyr::ungroup(),
                   by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
  dplyr::mutate(code_etape = apogee::histo_etape_succ_2(code_etape)) %>% 
  tidyr::unnest(code_etape, .drop = FALSE) %>% 
  dplyr::group_by(annee, code_etudiant, code_etape, inscription_premiere) %>% 
  dplyr::filter(dplyr::row_number() == n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, inscrits_peda, inscrits_cpge, code_composante, date_naissance, sexe, code_nationalite, annee_bac, code_departement_bac, code_bac, code_type_diplome_anterieur, code_bourse, code_regime_inscription, elp_parcours, code_resultat, code_resultat_diplome)

origine <- apogee::inscrits %>% 
  dplyr::mutate(annee = annee + 1) %>% 
  dplyr::filter(inscription_premiere == "O") %>% 
  dplyr::select(annee, code_etape_pre = code_etape, code_etudiant) %>% 
  dplyr::right_join(stats, by = c("annee", "code_etudiant")) %>% 
  dplyr::mutate(origine_gen1 = ifelse(!is.na(code_etape_pre), "Admission interne", "Admission externe"),
                origine_annee = apogee::annee_etape(apogee::histo_etape_succ(code_etape_pre)),
                histo_code_etape_pre = apogee::histo_etape_succ_2(code_etape_pre),
                mention_diplome_pre = purrr::map(histo_code_etape_pre, apogee::hier_etape_mention) %>% 
                  purrr::map(purrr::flatten_chr) %>% 
                  purrr::map(apogee::histo_mention_diplome_succ),
                mention_diplome = purrr::map(code_etape, apogee::hier_etape_mention) %>% 
                  purrr::map(purrr::flatten_chr) %>% 
                  purrr::map(apogee::histo_mention_diplome_succ),
                mention_diplome_comp = ifelse(apogee::hier_etape_type_diplome(code_etape) == "LMD/M1",
                                              purrr::map(mention_diplome, ~ apogee::compatibilite_mention_diplome_m(.[[1]])),
                                              mention_diplome),
                origine_gen2 = dplyr::case_when(
                  (apogee::annee_etape(code_etape) - origine_annee) == 1 & purrr::map2_lgl(mention_diplome_comp, mention_diplome_pre, ~ any(.x[[1]] %in% .y[[1]])) ~ "Flux normal",
                  (apogee::annee_etape(code_etape) - origine_annee) == 1 ~ "Flux latéral",
                  purrr::map2_lgl(code_etape, histo_code_etape_pre, ~ .x %in% .y) ~ "Redoublement",
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
                  origine_gen2 %in% c("Flux normal", "Flux latéral", "Réorientation") ~ apogee::histo_etape_succ(code_etape_pre) %>% apogee::lib_etape(ville = FALSE, option = FALSE, particularite = FALSE),
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
                     tidyr::unnest(code_etape),
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

reussite_diplome <- stats %>% 
  dplyr::mutate(reussite_diplome = ifelse(apogee::lib_resultat(code_resultat_diplome) == "Admis", "Oui", "Non")) %>% 
  dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, reussite_diplome)

poursuite <- apogee::inscrits %>% 
  dplyr::mutate(annee = annee - 1) %>% 
  dplyr::arrange(code_etudiant, annee, apogee::annee_etape(code_etape)) %>% 
  dplyr::group_by(code_etudiant, annee) %>% 
  dplyr::filter(dplyr::row_number() == 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(annee, code_etape_post = code_etape, code_etudiant) %>% 
  dplyr::right_join(stats, by = c("annee", "code_etudiant")) %>% 
  dplyr::filter(annee < apogee::annee_en_cours(mois_debut = 11)) %>% 
  dplyr::mutate(code_etape_post = apogee::histo_etape_succ(code_etape_post, elp_parcours),
                reussite = ifelse(apogee::lib_resultat(code_resultat) %in% "Admis", "Réussite", "Echec"),
                annee_etape_post = apogee::annee_etape(code_etape_post),
                filiere = apogee::hier_etape_filiere(code_etape),
                histo_code_etape_post = apogee::histo_etape_succ_2(code_etape_post),
                mention_diplome = purrr::map(code_etape, apogee::hier_etape_mention) %>% 
                  purrr::map(purrr::flatten_chr) %>% 
                  purrr::map(apogee::histo_mention_diplome_succ),
                mention_diplome_comp = ifelse(apogee::hier_etape_type_diplome(code_etape) == "LMD/L3",
                                              purrr::map(mention_diplome, ~ apogee::compatibilite_mention_diplome_l(.[[1]])),
                                              mention_diplome),
                mention_diplome_post = purrr::map(histo_code_etape_post, apogee::hier_etape_mention) %>% 
                  purrr::map(purrr::flatten_chr) %>% 
                  purrr::map(apogee::histo_mention_diplome_succ),
                poursuite = dplyr::case_when(
                  is.na(code_etape_post) ~ "Sortie UPS",
                  purrr::map2_lgl(code_etape, histo_code_etape_post, ~ .x %in% .y) ~ "Redoublement",
                  filiere == "Santé" & purrr::map_lgl(histo_code_etape_post, ~ any(apogee::hier_etape_filiere(.) == "Santé")) ~ "Année supérieure",
                  filiere == "Santé" & annee_etape_post - apogee::annee_etape(code_etape) >= 1 ~ "Année supérieure - Réorientation",
                  (annee_etape_post - apogee::annee_etape(code_etape) >= 1) & purrr::map2_lgl(mention_diplome_comp, mention_diplome_post, ~ any(.x[[1]] %in% .y[[1]])) ~ "Année supérieure - Flux normal",
                  (annee_etape_post - apogee::annee_etape(code_etape) >= 1) ~ "Année supérieure - Flux latéral",
                  (annee_etape_post - apogee::annee_etape(code_etape) <= 0) ~ "Réorientation"),
                poursuite = ifelse(poursuite == "Sortie UPS", paste(poursuite, reussite, sep = " - "), poursuite)) %>% 
  dplyr::select(annee, code_etape, code_etudiant, inscription_premiere, code_etape_post, poursuite)

situation_ups_post <- apogee::resultats_etape %>% 
  dplyr::right_join(dplyr::select(apogee::inscrits, annee, code_etudiant, code_etape, inscription_premiere),
                    by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
  dplyr::filter(inscription_premiere == "O") %>% 
  dplyr::group_by(annee, code_etudiant, code_etape, inscription_premiere) %>% 
  dplyr::filter(dplyr::row_number() == n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(annee_post = annee, code_etudiant, code_etape_post = code_etape, code_resultat_post = code_resultat) %>% 
  dplyr::right_join(dplyr::select(stats, annee, code_etudiant, code_etape, inscription_premiere),
                    by = "code_etudiant") %>% 
  dplyr::filter(annee_post > annee) %>% 
  tidyr::nest(annee_post, code_etape_post, code_resultat_post, .key = "situation_ups_post")

stats <- stats %>% 
  dplyr::left_join(origine, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>%
  dplyr::left_join(present_ts_examens, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>%
  dplyr::left_join(present_1_examen, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>%
  dplyr::left_join(reussite, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>%
  dplyr::left_join(reussite_diplome, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>%
  dplyr::left_join(poursuite, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>%
  dplyr::left_join(situation_ups_post, by = c("annee", "code_etape", "code_etudiant", "inscription_premiere")) %>% 
  dplyr::mutate(situation_ups_post = purrr::map_if(situation_ups_post, is.null, ~ dplyr::tibble()))

devtools::use_data(stats, overwrite = TRUE)

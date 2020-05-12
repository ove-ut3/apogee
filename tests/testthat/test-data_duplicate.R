test_that("Pas de doublons dans les tables de données", {
  expect_equal(nrow(patchr::duplicate(individus, code_etudiant)), 0)
  expect_equal(nrow(patchr::duplicate(individus_diplome_origine, code_etudiant, annee_diplome_obtenu)), 0)
  expect_equal(nrow(patchr::duplicate(individus_situation_annee_precedente, code_etudiant, annee)), 0)
  expect_equal(nrow(patchr::duplicate(inscrits, annee, code_etape, code_etudiant, inscription_premiere)), 0)
  expect_equal(nrow(patchr::duplicate(inscrits_cpge, annee, code_etape, code_etudiant, inscription_premiere)), 0)
  expect_equal(nrow(patchr::duplicate(inscrits_elp, annee, code_etape, code_etudiant, inscription_premiere, code_elp)), 0)
  expect_equal(nrow(patchr::duplicate(inscrits_peda, annee, code_etape, code_etudiant)), 0)
  expect_equal(nrow(patchr::duplicate(resultats_elp, annee, code_etape, code_etudiant, inscription_premiere, code_elp, lib_session)), 0)
  expect_equal(nrow(patchr::duplicate(resultats_etape, annee, code_etape, code_etudiant, inscription_premiere, lib_session)), 0)
  expect_equal(nrow(patchr::duplicate(resultats_diplome, annee, code_etape, code_etudiant, inscription_premiere, lib_session)), 0)
})

test_that("Pas de doublons dans les tables de métadonnées", {
  
  etape_mention <- apogee::etape_mention %>% 
    dplyr::mutate(lib_mention = apogee::lib_mention_diplome(code_mention_diplome)) %>% 
    patchr::duplicate(code_etape, lib_mention)
  
  expect_true(nrow(etape_mention) == 0)
  
  # Tester si les tables access ne sont pas redondantes avec des mises à jours qui ont été faites sur apogée depuis
  
  # tester si de nouvelles formations sont apparues depuis la denière update
  
  # apogee::histo_etape_succ("EDCHM1", code_elp = "EDCHC03P;EDCHC04P", multiple = TRUE) == "EDCHAE"
})

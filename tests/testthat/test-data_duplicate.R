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

# test_that("Pas de doublons dans les tables de métadonnées", {
#   expect_equal(nrow(patchr::duplicate(inscrits, annee, code_etape, code_etudiant, inscription_premiere)), 0)
# })
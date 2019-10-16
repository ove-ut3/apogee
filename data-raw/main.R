# Données brutes
rezip <- list.files(pattern = "\\.zip$", recursive = TRUE, full.names = TRUE) %>% 
  pbapply::pblapply(apogee::rezip_csv)

source("data-raw/scripts/apogee-data.R", encoding = "UTF-8")
# Penser à builder apogée

# Méta-données
source("data-raw/scripts/composante.R", encoding = "UTF-8")
source("data-raw/scripts/diplome.R", encoding = "UTF-8")
source("data-raw/scripts/diplome_version.R", encoding = "UTF-8")
source("data-raw/scripts/elp.R", encoding = "UTF-8")
source("data-raw/scripts/etape.R", encoding = "UTF-8")
source("data-raw/scripts/inscription.R", encoding = "UTF-8")
source("data-raw/scripts/resultat.R", encoding = "UTF-8")
source("data-raw/scripts/individu.R", encoding = "UTF-8")
source("data-raw/scripts/academie.R", encoding = "UTF-8")
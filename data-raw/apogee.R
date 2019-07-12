# Données brutes
rezip <- list.files(pattern = "\\.zip$", recursive = TRUE, full.names = TRUE) %>% 
  pbapply::pblapply(apogee::rezip_csv)

source("data-raw/apogee-data.R", encoding = "UTF-8")

# Méta-données
source("data-raw/composante.R", encoding = "UTF-8")
source("data-raw/diplome.R", encoding = "UTF-8")
source("data-raw/diplome_version.R", encoding = "UTF-8")
source("data-raw/elp.R", encoding = "UTF-8")
source("data-raw/etape.R", encoding = "UTF-8")
source("data-raw/inscription.R", encoding = "UTF-8")
source("data-raw/resultat.R", encoding = "UTF-8")
source("data-raw/sise.R", encoding = "UTF-8")

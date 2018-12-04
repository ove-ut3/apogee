# Rezip des CSV (gain de place)
list.files(pattern = "\\.zip$", recursive = TRUE, full.names = TRUE) %>% 
  purrr::walk(apogee::rezip_csv)

# Données brutes
source("data-raw/utils.R")
source("data-raw/apogee-data.R")

developr::package_build()

# Méta-données
source("data-raw/composante.R")
source("data-raw/diplome.R")
source("data-raw/diplome_version.R")
source("data-raw/elp.R")
source("data-raw/etape.R")
source("data-raw/inscription.R", encoding = "UTF-8")
source("data-raw/resultat.R")
source("data-raw/sise.R")

developr::package_build()

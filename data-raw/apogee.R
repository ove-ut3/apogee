# Rezip des CSV (gain de place)
rezip <- list.files(pattern = "\\.zip$", recursive = TRUE, full.names = TRUE) %>% 
  pbapply::pblapply(apogee::rezip_csv)

# Tables import
tables <- impexp::access_tables("data-raw/Tables_ref.accdb") %>% 
  stringr::str_subset("^_")

developr::access_rda(access_path = "data-raw/Tables_ref.accdb",
                     data_path = "data/",
                     tables,
                     tables_rda = stringr::str_remove(tables, "^_"))

remotes::install_local(upgrade = "never", force = TRUE)

# Données brutes
source("data-raw/apogee-data.R")
remotes::install_local(upgrade = "never", force = TRUE)

# Méta-données
source("data-raw/composante.R")
source("data-raw/diplome.R")
source("data-raw/diplome_version.R")
source("data-raw/elp.R")
source("data-raw/etape.R")
source("data-raw/inscription.R", encoding = "UTF-8")
source("data-raw/resultat.R")
source("data-raw/sise.R")
remotes::install_local(upgrade = "never", force = TRUE)

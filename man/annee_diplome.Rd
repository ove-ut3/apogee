% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{annee_diplome}
\alias{annee_diplome}
\title{Renvoie l'annee de diplome a partir du code etape}
\usage{
annee_diplome(code_etape)
}
\arguments{
\item{code_etape}{Un vecteur de code étape.}
}
\value{
Un vecteur contenant les années de diplôme.

Jeu de données source : \code{apogee::etape}.\cr
Il est créé à partir d'Apogée et de la table "etape" de la base Access "Tables_ref.accdb" (projet Apogée).
}
\description{
Renvoie l'année de diplôme à partir du code étape au sein d'un diplôme.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NStabeller.R
\name{tabSkjemaTilknyttet}
\alias{tabSkjemaTilknyttet}
\title{Tabell: Antall og andel moder"skjema som har ulike typer registreringsskjema}
\usage{
tabSkjemaTilknyttet(
  Data = AlleTab,
  moderSkjema = "Hoved",
  datoUt = 0,
  datoFra = "2017-01-01",
  datoTil = Sys.Date()
)
}
\arguments{
\item{moderSkjema}{Hvilket skjema man skal knytte oppfølgingene til}

\item{datoUt}{datofiltrering basert på innleggelse (0), utskriving (1)}

\item{datoFra}{startdato, fra og med Eks. '2010-01-01'    # min og max dato i utvalget vises alltid i figuren.}

\item{datoTil}{sluttdato, fra og med Eks. '2013-05-25'}
}
\description{
Tabell: Antall og andel moder"skjema som har ulike typer registreringsskjema
}

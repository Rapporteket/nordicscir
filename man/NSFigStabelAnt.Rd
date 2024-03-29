% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NSFigStabelAnt.R
\name{NSFigStabelAnt}
\alias{NSFigStabelAnt}
\title{Stabelplott som viser fordeling av valgt variabel}
\usage{
NSFigStabelAnt(
  RegData,
  outfile = "",
  valgtVar = "AAisFAis",
  hentData = 0,
  register = "norscir",
  preprosess = 1,
  datoFra = "2010-01-01",
  datoTil = "2050-01-01",
  datoUt = 0,
  AIS = "",
  enhetsUtvalg = 0,
  reshID = 0,
  minald = 0,
  maxald = 130,
  erMann = 99,
  traume = "alle",
  nivaaUt = 99,
  ...
)
}
\arguments{
\item{RegData}{- ei dataramme med alle nødvendige variable fra registeret.}

\item{outfile}{- navn på fil figuren skrives ned til.}

\item{valgtVar}{- variabelen det skal genereres resultat for}

\item{hentData}{Gjør spørring mot database hvis data ikke er levert fra andre kilder.
0: Nei, RegData gis som input til funksjonen (Standard)
1: Ja}

\item{register}{Må angis hvis hentData=1. Valg: 'norscir' (standard), 'nordicscir'}

\item{preprosess}{Preprosesser data
0: Nei
1: Ja (Standard)}

\item{datoUt}{Om man skal velge inn eller ut-dato som grunnlag for datofiltrering}

\item{reshID}{- avdelingsid for egen avdeling, standard: 0-hele landet/Norden.}

\item{figurtype}{angir hvilken figurtype som skal lages: andeler, gjsnGrVar}
}
\description{
Stabelplott som viser fordeling av valgt variabel
}

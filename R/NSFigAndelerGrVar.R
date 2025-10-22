#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en valgt grupperingsvariabel,
#' f.eks. sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' Andel som mottar sykepenger er definert som svaralternativene: 'Sykemeldt',
#'        'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item ABMI: Målt og veid ved innleggelse?
#'     \item FBMI: Målt og veid ved utskriving?
#'		}
#'
#' @inheritParams NSFigAndeler
#' @inheritParams NSUtvalgEnh
#' @param grVar Tekstvariabel som angir hva skal resultatene grupperes på.
#'                ShNavn-sykehus/avdeling
#'                Fylke- Pasienten bor i det akutelle fylket
#' @param Ngrense Minste antall registreringer for at ei gruppe skal bli vist
#' @importFrom rapFigurer figtype
#' @return Figur med...
#'
#' @export

NSFigAndelerGrVar <- function(RegData=0,preprosess=1, # hentData=0,
                                valgtVar='ABMI', datoFra='2015-01-01', datoTil=Sys.Date(),
                                minald=0, maxald=130, erMann='',
                                enhetsUtvalg=0, grVar='ShNavn',
                                Ngrense=10, reshID=0, outfile='', ...) {
  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = paste0('AndelPrShus: ',valgtVar))
  }

    # Ngr <- RegData$Total
    # Ngrtxt <- as.character(Ngr)
    # AndelHele <- 100*sum(RegData$RegNKR)/sum(RegData$Total)   #RegData$DG_nkr[indLandet]
    # AndelerGr <- 100*RegData$RegNKR/RegData$Total #RegData$DG_nkr #[-indLandet]
    # fargepalett='BlaaOff'
    # utvalgTxt <- ''
    # sortAvtagende <- T
    # AntGr <- length(AndelerGr)
    # GrNavn <- RegData$ShNavn
    # hovedgrTxt <- 'Hele landet'
    # N <- sum(RegData$Total)
    # grVar <- 'ShNavn'
    # Ngrense <- 0


    # if (hentData == 1) {
    #   RegData <- NSRegDataSQLV2V3()
    # }

    # Preprosessere data
    if ((preprosess==1) & (dim(RegData)[1] >1)){
      RegData <- NSPreprosess(RegData=RegData)}

        #------- Tilrettelegge variable
    NSVarSpes <- NSVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'andelGrVar')
    RegData <- NSVarSpes$RegData
    sortAvtagende <- NSVarSpes$sortAvtagende
    # KImaalGrenser <- NSVarSpes$KImaalGrenser

    RegData[ ,grVar] <- factor(RegData[ ,grVar])

    #------- Gjøre utvalg
    if (reshID==0) {enhetsUtvalg <- 0}

    NSUtvalg <- NSUtvalgEnh(RegData=RegData, reshID=reshID, datoFra=datoFra, datoTil=datoTil,
                                # minald=minald, maxald=maxald, erMann=erMann,
                                enhetsUtvalg=enhetsUtvalg)
    fargepalett <- NSUtvalg$fargepalett
    hovedgrTxt <- NSUtvalg$hovedgrTxt
    utvalgTxt <- NSUtvalg$utvalgTxt
    ind <- NSUtvalg$ind
    RegData <- NSUtvalg$RegData
    #}
    #---------------Beregninger
    # Variabelen Variabel er definert som indikatorvariabel for den valgte variabelen.
    if(dim(RegData)[1] > 0) {
      RegData <- RegData[which(RegData[ ,grVar] != ''),] #Tar ut registreringer uten grupperingsnavn
      RegData[ ,grVar] <- as.factor(RegData[ ,grVar])
      Ngr <- table(RegData[ ,grVar])
      Ngrtxt <- as.character(Ngr)
    }	else {Ngr <- 0}

    N <- dim(RegData)[1]
    AntGr <- length(which(Ngr >= Ngrense))	#Alle som har gyldig resultat
    AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
    AndelerGr <- round(100*tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)/Ngr,2)

    GrNavn <- names(Ngr)
    xAkseTxt <- "Andel opphold (%)"

    indGrUt <- which(Ngr < Ngrense)
    if (sum(indGrUt)>0) {
      AndelGrUt <- sum(AndelerGr[indGrUt]*Ngr[indGrUt], na.rm = T)/sum(Ngr[indGrUt])
      AndelerGr <- c(AndelerGr[-indGrUt],AndelGrUt)
      GrUtNavn <- paste0(length(indGrUt), ' avd. med N<',Ngrense)
      Ngrtxt <- c(Ngr[-indGrUt],sum(Ngr[indGrUt]))
      GrNavn <- c(GrNavn[-indGrUt], GrUtNavn)
    }
    Tittel <- NSVarSpes$tittel

    fargepalett <- NSUtvalg$fargepalett
  #}
  sortInd <- order(as.numeric(AndelerGr), decreasing=sortAvtagende, na.last = FALSE)
  AndelerGrSort <- AndelerGr[sortInd]
  GrNavnSort <- GrNavn[sortInd]
  Ngrtxt <- Ngrtxt[sortInd]

  andeltxtUsort <- paste0(sprintf('%.1f',AndelerGr), ' %')
  andeltxt <- andeltxtUsort[sortInd]


  FigDataParam <- list(AggVerdier=AndelerGrSort,
                       AggTot=AndelHele,
                       N=N,
                       Ngr=as.numeric(Ngrtxt),
                       soyletxt=andeltxt,
                       grtxt=GrNavnSort,
                       Tittel=Tittel,
                       utvalgTxt=utvalgTxt,
                       fargepalett =fargepalett

  )


  #-----------Figur---------------------------------------
  if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
    FigTypUt <- rapFigurer::figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    if (!is.null(dim(RegData))) { #>0
      tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
    } else {tekst <- 'Ingen registrerte data for dette utvalget'}
    title(main=Tittel)
    text(0.5, 0.6, tekst, cex=1.2)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    if ( outfile != '') {dev.off()}

  } else {

    #--------------------------FIGUR---------------------------------------------------
    #----------- Figurparametre ------------------------------
    cexShNavn <- 0.9 #0.85

    FigTypUt <- rapFigurer::figtype(outfile, height=3*800, fargepalett=fargepalett)
    farger <- FigTypUt$farger
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.8)
    #NB: strwidth oppfører seg ulikt avh. av device...
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    xmax <- min(max(AndelerGrSort, na.rm=T),100)*1.15
    pos <- rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4], #main=Tittel,
                       xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(GrNavnSort), font.main=1, #xlab='Andel (%)',
                       las=1, cex.names=cexShNavn*0.9))
    ybunn <- 0.1
    ytopp <- max(pos)+0.4

    pos <- rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4],
                       xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(GrNavnSort), font.main=1, #xlab='Andel (%)',
                       las=1, cex.names=cexShNavn*0.9, add=T))
    mtext('Andel (%)', side=1, line=2)
    #Linje for hele landet/utvalget:
    lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
    legend('topright', xjust=1, cex=1, lwd=2, col=farger[2],
           legend=paste0(hovedgrTxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N),
           bty='o', bg='white', box.col='white')
    mtext(at=pos+max(pos)*0.0045, paste0(GrNavnSort,' (',Ngrtxt , ')'), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg


    title(Tittel, line=1, font.main=1, cex.main=1.3)

    text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
         las=1, cex=0.9, adj=0, col=farger[1])	#Andeler, hvert sykehus

    mtext(at=max(pos)+0.4*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=1, adj=1, line=0.25)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}
    #----------------------------------------------------------------------------------
  }
  return(invisible(FigDataParam))
}


#' Stabelplott som viser fordeling av valgt variabel
#'
#'
#' @param RegData - ei dataramme med alle nødvendige variable fra registeret.
#' @param outfile - navn på fil figuren skrives ned til.
#' @param reshID - avdelingsid for egen avdeling, standard: 0-hele landet/Norden.
#' @param valgtVar - variabelen det skal genereres resultat for
#' @param preprosess Preprosesser data
#'                 0: Nei
#'                 1: Ja (Standard)
#' @param hentData Gjør spørring mot database hvis data ikke er levert fra andre kilder.
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#' @param register Må angis hvis hentData=1. Valg: 'norscir' (standard), 'nordicscir'
#' @param figurtype angir hvilken figurtype som skal lages: andeler, gjsnGrVar
#' @param datoUt Om man skal velge inn eller ut-dato som grunnlag for datofiltrering
#'
#' @export

NSFigStabelAnt <- function(RegData, outfile='', valgtVar='AAisFAis',
                          hentData=0, register='norscir', preprosess=1,
                          datoFra='2010-01-01', datoTil='2050-01-01', datoUt=0, AIS='',
                          enhetsUtvalg=0, reshID = 0,
                          minald=0, maxald=130, erMann=99, traume='alle',nivaaUt=99, ...) {

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]], msg = "AndelStabel, figur")
  }
  if (hentData == 1) {
    RegData <- NSRegDataSQL(valgtVar=valgtVar, register = register)
  }

  #Evt. ta inn denne igjen...: if ( dim(RegData)[1] == 0 ) {} else   {

  if (preprosess == 1) {
    RegData <- NSPreprosesser(RegData)
  }

  #--------------- Tilrettelegge variable og gjøre utvalg ------------------------------
  NSVarSpes <- NSVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype='antGr')
  RegData <- NSVarSpes$RegData

  Utvalg <- NSUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, datoUt=datoUt,
                        minald=minald, maxald=maxald, erMann=erMann, traume=traume,
                        AIS=AIS, nivaaUt=nivaaUt, enhetsUtvalg=enhetsUtvalg)
  RegData <- Utvalg$RegData
  utvalgTxt <- Utvalg$utvalgTxt
  yAkseTxt <- "Antall pasienter"

  #RegData$GrVar <- as.factor(RegData[ ,grVar])

  #--------------- Gjøre beregninger ------------------------------
  AggVerdier <- table(RegData$VariabelGrPost, RegData$VariabelGr)[ ,length(table(RegData$VariabelGr)):1]
  N <- table(RegData$VariabelGr)
  grtxt <- NSVarSpes$grtxt
  cexgr <- NSVarSpes$cexgr
  anttxt <- rev(paste0(' (N=', N,')'))
  legendtxt <- rev(rownames(AggVerdier))
  stabelVar <- switch(valgtVar,
                    AAisFAis = 'AIS, ut',
                    KontFAis = 'AIS, kontr.')

  FigDataParam <- list(AggVerdier=AggVerdier,
                       N=N,
                       Ngr=AggVerdier,
                       KImaal <- NSVarSpes$KImaal,
                       grtxt=NSVarSpes$grtxt,
                       tittel=NSVarSpes$tittel,
                       retn=NSVarSpes$retn,
                       xAkseTxt=NSVarSpes$xAkseTxt,
                       yAkseTxt=yAkseTxt,
                       utvalgTxt=Utvalg$utvalgTxt,
                       fargepalett=Utvalg$fargepalett)


  #-----------Figur---------------------------------------
  if (sum(N) < 5) {
    farger <- rapFigurer::figtype(outfile)$farger
    plot.new()
    title(NSVarSpes$tittel)	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, paste0('Færre enn ', 5, ' registreringer,'), cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {

    #foreløpig standard med 5 farger...
    FigTypUt <- rapFigurer::figtype(outfile, fargepalett = 'BlaaOffAlle')
    farger <- FigTypUt$farger[2:6] #[1:ncol(AggVerdier)]

    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- switch(NSVarSpes$retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.8))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
    antGr <- length(grtxt)
    fargeHoved <- rev(farger)[1:antGr]
    cexleg <- 1	#Størrelse på legendtekst


    if (NSVarSpes$retn == 'V' ) {
      #Vertikale søyler eller linje
      ymax <- max(AggVerdier,na.rm=T)*1.15
      pos <-   barplot(AggVerdier, beside=FALSE, ylab=yAkseTxt,
                       cex.lab=cexleg, sub=NSVarSpes$xAkseTxt, cex.sub=cexleg,
                       col=fargeHoved, border = NA, ylim=c(0, ymax)) #border='white'
      mtext(at=pos, anttxt, side=1, las=1, cex=0.9, adj=0.5, line=1.8)
      legend('topright', title = stabelVar, legendtxt,
             border=NA, fill=c(rev(fargeHoved)), bty='n', ncol=1, cex=cexleg)
    }

    if (NSVarSpes$retn == 'H') { #Ikke tilrettelagt

      #Horisontale søyler
      xmax <- min(max(c(AggVerdier),na.rm=T)*1.25, 100)
      pos <-   barplot(AggVerdier[,ncol(AggVerdier):1], horiz=TRUE, beside=T, xlab="Andel pasienter (%)",
                       cex.lab=cexleg, cex.sub=cexleg, axisnames =FALSE, #names.arg = grtxtpst, #sub=NSVarSpes$xAkseTxt,
                       col=fargeHoved, border='white', xlim=c(0, xmax)) #, ylim=c(0, ymax))
      mtext(at=pos[2,]+0.1, text=rev(grtxtpst), side=2, las=1, cex=cexgr, adj=1, line=0.25)
      legend('topright', legendtxt,
             border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
    }

    title(NSVarSpes$tittel, line=1, font.main=1, cex.main=1.5)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=cexgr, adj=0, col=farger[1], line=c(3+0.9*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}

    #           } #Nok observasjoner
  }  #Figur


  return(invisible(FigDataParam))

}

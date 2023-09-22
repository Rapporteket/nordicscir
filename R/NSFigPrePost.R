#' Søylediagram som viser resultat av valgt variabel, målt ved to tidspunkter
#'
#' Funksjon som genererer en figur med som viser endring i en variabels fordeling ved to ulike tidspunkter.
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams NSFigAndeler
#' @param RegData er her KontrollH, dvs. Kontrollskjema koblet til hovedskjema
#' @param valgtVar Hvilken variabel som skal visualiseres
#' @param enhetsUtvalg foreløpig 0:hele landet/Norden (standard),
#'                       2:eget sykehus
#'                       4:eget land
#'
#' @return Søylediagram som fordeling av valgt variabel, ved operasjon, samt 1. kontroll
#'
#' @export

NSFigPrePost  <- function(RegData, valgtVar='KontUtTil', datoFra='2019-01-01', datoTil=Sys.Date(),
                            enhetsUtvalg = 0, datoUt=0, reshID = 0, velgAvd=0,
                            minald=0, maxald=130, register='norscir',
                          erMann=9, traume='alle', AIS='', nivaaUt=99,
                            Ngrense=10, outfile='', preprosess=0, hentData=0,...)
{

  if (hentData == 1) {
    RegData <- NSRegDataSQL(valgtVar, register)
  }

  # Hvis RegData ikke har blitt preprosessert.
  if (preprosess==1){
    RegData <- NSPreprosess(RegData=RegData)
  }

  NSVarSpes <- NSVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype='prepost')
  RegData <- NSVarSpes$RegData
  tittel <- NSVarSpes$tittel
  retn <- NSVarSpes$retn
  grtxt <- NSVarSpes$grtxt

  Utvalg <- NSUtvalgEnh(RegData=RegData,datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                        erMann=erMann, traume=traume, AIS=AIS, nivaaUt=nivaaUt,
                        enhetsUtvalg = enhetsUtvalg, reshID = reshID, datoUt=datoUt)
  RegData <- Utvalg$RegData
  utvalgTxt <- Utvalg$utvalgTxt
  # medSml <- Utvalg$medSml
  # smltxt <- Utvalg$smltxt

#---------------BEREGNINGER --------------------------
  #AggVerdier <- list(Hoved = 0, Rest =0)
  #N <- list(Hoved = 0, Rest =0)   #Nevner
  #Ngr <- list(Hoved = 0, Rest =0) #Teller
  #ind <- #Utvalg$ind

  NgrPre <- table(RegData$VariabelGr) #[ind$Hoved]
  NgrPost <- table(RegData$VariabelGrPost) #[ind$Hoved]
  N <- dim(RegData)[1]  #sum(NgrPre)
  AggVerdierPre <- 100*NgrPre/N
  AggVerdierPost <- 100*NgrPost/N
  AggVerdierPP <- cbind(AggVerdierPre, AggVerdierPost)


  # if (medSml==1) {
  #   Ngr$Rest <- table(RegData$VariabelGr[ind$Rest])
  #   N$Rest <- sum(Ngr$Rest)	#length(ind$Rest)- Kan inneholde NA
  #   AggVerdier$Rest <- 100*Ngr$Rest/N$Rest
  # }

  grtxt <- NSVarSpes$grtxt
  #grtxt2 <- paste0(' (', sprintf('%.1f',AggVerdierPP), '%)')
  cexgr <- NSVarSpes$cexgr
  hovedgrTxt <- Utvalg$hovedgrTxt

  #-----------Figur---------------------------------------
  FigTypUt <- rapFigurer::figtype(outfile)
  farger <- FigTypUt$farger
  NutvTxt <- length(utvalgTxt)
  vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

  if (dim(RegData)[1] < 10 ) {
    plot.new()
    title(main=tittel)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'Færre enn 10 registreringer i ', cex=1.2)
    text(0.55, 0.6, 'den valgte datafiltreringa', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {

    #-----------Figur---------------------------------------

    if (retn == 'V' ) { #Benytter denne til å vise ulike grupper av en variabel
      #Vertikale søyler eller linje
      ymax <- min(max(c(AggVerdierPP),na.rm=T)*1.25, 110)
      pos <- barplot(t(AggVerdierPP), beside=TRUE, horiz=FALSE, las=txtretn, ylab="",
                     sub='Andel pasienter (%)',
                     cex.names=1, col=farger[c(1,3)], border='white', ylim=c(0, ymax))
    }

    if (retn == 'H') {
      #Horisontale søyler
      xmax <- min(max(AggVerdierPP,na.rm=T)*1.25, 100)
      pos <- barplot(t(AggVerdierPP), beside=TRUE, horiz=TRUE, main='', las=1,
                     col=farger[c(1,3)], border='white', font.main=1,  xlim=c(0,xmax),
                     cex.names=1, xlab='Andel pasienter (%)')
      # legend('top', c('Før', 'Etter',paste0('N=',N)), bty='n',
      #        fill=farger[c(1:3,NA)], border=NA, ncol=3, cex=0.9)
    }

    legend('top', c('Utskriving', '1.kontroll', paste0('N=', N)), bty='n',
           fill=farger[c(1,3,NA)], border=NA, ncol=4, cex=0.9)
    title(tittel, font.main=1)	#line=0.5,
    #Tekst som angir hvilket utvalg som er gjort
    utvpos <- 3+length(tittel)-1	#Startlinje for teksten
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}

  }
}

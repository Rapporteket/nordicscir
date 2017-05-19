#' Søylediagram som viser fordeling av valgt variabel
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Aldersfordeling, 15-årige grupper 
#'     \item AAis: A-E+U, innleggelse
#'     \item FAis: A-E+U, kontroll
#'     \item DagerRehab: Lengde på rehab.opphold (Utskrevet – Innl. rehab dato)
#'     \item DagerTilRehab: Tid fra skadedato til oppstart rehab [trauma/ikke], 5d interv
#'     \item OpphTot: Lengde på opphold – totalt (HosptlDy) 20d… >200, 
#'     \item Permisjon: Antall døgn ute av sykehus 
#'     \item UtTil: Hva pasienten skrives ut til. [PlaceDis]
#'     \item SkadeArsak: Årsak til skade [variabel: Scietiol]
#'     \item Pustehjelp[VentAssi]
#'     \item LivsGen:  Fornøydhet med livet
#'     \item LivsFys:  Fornøydhet med fysisk helse
#'     \item LivsPsyk:  Fornøydhet med psykisk helse
#'     \item UrinInkontinens: Ufrivillig urinlekkasje
#'     \item UrinKirInngr: Kirurgiske inngrep i urinveiene
#'     \item UrinLegemidler: Bruk av legemidler som påvirker urinveiene
#'     \item UrinLegemidlerHvilke: Legemidler som påvirker urinveiene
#'     \item UrinTomBlareHoved: Blæretømming, hovedmetode
#'     \item UrinTomBlareTillegg: Blæretømming, tilleggsmetode
#'     \item TarmAvfmiddel: Bruk av perorale avføringsmidler
#'     \item TarmAvfmiddelHvilke: Perorale avføringsmidler
#'     \item TarmInkontinens: Hyppighet av fekal inkontinens
#'     \item TarmKirInngrep: Kirurgisk inngrep i mage/–tarm kanalen
#'     \item TarmKirInngrepHvilke: Kirurgiske inngrep i mage/–tarm kanalen
#'     \item TarmAvfHoved: Avføring, hovedmetode
#'     \item TarmAvfTillegg: Avføring, tilleggsmetode
#'    }
#' 
#' @param RegData - ei dataramme med alle nødvendige variable fra registeret.
#' @param outfile - navn på fil figuren skrives ned til.
#' @param reshID - avdelingsid for egen avdeling, standard: 0-hele landet.
#' @param valgtVar - variabelen det skal genereres resultat for 
#' @param datoFra <- '2010-01-01'    # min og max dato i utvalget vises alltid i figuren.
#' @param datoTil <- '2013-05-25'
#' @param erMann - kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#' @param minald - alder, fra og med
#' @param maxald - alder, til og med
#' @param traume - 'ja','nei', standard: ikke valgt
#' @param AIS - AISgrad ved innleggelse alle(''), velge en eller flere fra A,B,C,D,E,U
#' @param paratetra - Nivå ved utreise, flervalgs tetraplegi, paraplegi, ukjent
#' @param enhetsUtvalg - 1:eget sykehus, 0:hele landet (standard) 
#' @param preprosess Preprosesser data
#'                 0: Nei
#'                 1: Ja (Standard)
#' @param hentData Gjør spørring mot database hvis data ikke er levert fra andre kilder.
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#' 
#' @export

NSFigAndeler <- function(RegData, outfile='', valgtVar,
                         datoFra='2010-01-01', datoTil='2050-01-01', AIS='',
                         minald=0, maxald=130, erMann=99, traume='',paratetra=99,
                         enhetsUtvalg=1, reshID, hentData=0, preprosess=1) {
      
      if (hentData == 1) {
            RegData <- NSRegDataSQL(valgtVar=valgtVar)
      }
      if (preprosess == 1) {
            RegData <- NSPreprosesser(RegData)
      }
    
      #--------------- Tilrettelegge variable og gjøre utvalg ------------------------------
      
      NSVarSpes <- NSVarTilrettelegg(RegData=RegData, valgtVar=valgtVar)
	  RegData <- NSVarSpes$RegData
      
      
      Utvalg <- NSUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                         erMann=erMann, traume=traume, AIS=AIS, paratetra=paratetra,
                         enhetsUtvalg = enhetsUtvalg, reshID = reshID)
      RegData <- Utvalg$RegData
      utvalgTxt <- Utvalg$utvalgTxt
      medSml <- Utvalg$medSml
      
     #--------------- Gjøre beregninger ------------------------------
      AggVerdier <- list(Hoved = 0, Rest =0)
      N <- list(Hoved = 0, Rest =0)   #Nevner
      Ngr <- list(Hoved = 0, Rest =0) #Teller 
      Nfig <- list(Hoved = 0, Rest =0) #figurtekst: N i legend
      ind <- Utvalg$ind
      variable <- NSVarSpes$variable
      flerevar <- NSVarSpes$flerevar
      
      Ngr$Hoved <- switch(as.character(flerevar), 
                          '0' = table(RegData$VariabelGr[ind$Hoved]),
                          '1' = apply(RegData[ind$Hoved,variable], MARGIN=2, 
                                      FUN=function(x) sum(x == 1, na.rm=T)))
      #N$ gjelder selv om totalutvalget er ulikt for de ulike variablene i flerevar
      N$Hoved <- switch(as.character(flerevar), 
                        '0' = sum(Ngr$Hoved),	#length(ind$Hoved)- Kan inneholde NA
                        '1' = apply(RegData[ind$Hoved,variable], MARGIN=2, 
                                    FUN=function(x) sum(x %in% 0:1, na.rm=T)))
      AggVerdier$Hoved <- 100*Ngr$Hoved/N$Hoved
      
      
      
      if (medSml==1) {
            Ngr$Rest <- switch(as.character(flerevar), 
                               '0' = table(RegData$VariabelGr[ind$Rest]),
                               '1' = apply(RegData[ind$Rest,variable], MARGIN=2, 
                                           FUN=function(x) sum(x == 1, na.rm=T)))
            N$Rest <- switch(as.character(flerevar), 
                             '0' = sum(Ngr$Rest),	#length(ind$Rest)- Kan inneholde NA
                             '1' = apply(RegData[ind$Rest,variable], MARGIN=2, 
                                         FUN=function(x) sum(x %in% 0:1, na.rm=T)))
            AggVerdier$Rest <- 100*Ngr$Rest/N$Rest
      }
      
      if(flerevar==1) {
            Nfig$Hoved <- max(N$Hoved)
                  #ifelse(min(N$Hoved)==max(N$Hoved),
                  #              min(N$Hoved[1]), 
                  #               paste0(min(N$Hoved),'-',max(N$Hoved)))
            Nfig$Rest <- max(N$Rest)
                  #ifelse(min(N$Rest)==max(N$Rest),
                  #              min(N$Rest[1]), 
                  #              paste0(min(N$Rest),'-',max(N$Rest)))
      } else {
            Nfig <- N}
      
      
      
      
      #grtxt <- paste0(rev(NSVarSpes$grtxt), ' (', rev(sprintf('%.1f',AggVerdier$Hoved)), '%)') 
      grtxt <- NSVarSpes$grtxt
      grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
      cexgr <- NSVarSpes$cexgr
      hovedgrTxt <- Utvalg$hovedgrTxt
      yAkseTxt='Andel pasienter (%)'
      
      FigDataParam <- list(AggVerdier=AggVerdier, N=N, 
                           Ngr=Ngr,	
                           KImaal <- NSVarSpes$KImaal,
                           #soyletxt=soyletxt,
                           grtxt2=grtxt2, 
                           grtxt=NSVarSpes$grtxt,
                           tittel=NSVarSpes$tittel, 
                           retn=NSVarSpes$retn, 
                           xAkseTxt=NSVarSpes$xAkseTxt,
                           yAkseTxt=yAkseTxt,
                           utvalgTxt=Utvalg$utvalgTxt, 
                           fargepalett=Utvalg$fargepalett, 
                           medSml=medSml,
                           hovedgrTxt=hovedgrTxt,
                           smltxt=Utvalg$smltxt)
   
#-----------Figur---------------------------------------
       
      #-----Hvis få registreringer: ---------------------
      Ngrense <- 1      ## ENDRE TIL 5 FØR LEGGES PÅ RAPPORTEKET
      if (sum(N$Hoved) < Ngrense | (medSml ==1 & sum(N$Rest)< Ngrense)) {
            FigTypUt <- rapbase::figtype(outfile)
            farger <- FigTypUt$farger
            plot.new()
            title(NSVarSpes$tittel)	#, line=-6)
            legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
            text(0.5, 0.6, 'Færre enn 5 egne registreringer,', cex=1.2)
            text(0.5, 0.5, 'eller færre enn 5 i sammenlikningsgruppa', cex=1.2)
            if ( outfile != '') {dev.off()}
      } else {
            
            #Inn parametre: xAkseTxt, grtxt, grtxt2, tittel, AggVerdier
            #Plottspesifikke parametre:
            
            #Plottspesifikke parametre:
            FigTypUt <- rapbase::figtype(outfile, fargepalett=Utvalg$fargepalett)
            #Tilpasse marger for å kunne skrive utvalgsteksten
            NutvTxt <- length(utvalgTxt)
            grtxtpst <- paste0(rev(grtxt), ' (', rev(sprintf('%.1f',AggVerdier$Hoved)), '%)')
            vmarg <- switch(NSVarSpes$retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.8))
            par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
            
            farger <- FigTypUt$farger
            fargeHoved <- farger[1]
            fargeRest <- farger[3]
            antGr <- length(grtxt)
            lwdRest <- 3	#tykkelse på linja som repr. landet
            cexleg <- 1	#Størrelse på legendtekst
            
            
            if (NSVarSpes$retn == 'V' ) {
                  #Vertikale søyler eller linje
                  if (grtxt2[1] == '') {grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')}
                  ymax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.15
                  pos <- barplot(as.numeric(AggVerdier$Hoved), beside=TRUE, ylab="Andel pasienter (%)",	#las=txtretn, 
                                 cex.lab=cexleg, sub=NSVarSpes$xAkseTxt, cex.sub=cexleg,	col=fargeHoved, border='white', ylim=c(0, ymax))	#farger[c(1,3)] #names.arg=grtxt, cex.names=cexgr,
                  mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
                  mtext(at=pos, grtxt2, side=1, cex=cexgr-0.1, adj=0.5, line=1.5) #las=txtretn, 
                  if (medSml == 1) {
                        points(pos, as.numeric(AggVerdier$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
                        legend('top', c(paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'), paste0('Landet forøvrig (N=', Nfig$Rest,')')),
                               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=NA,
                               lwd=lwdRest, ncol=2, cex=cexleg)
                  } else {
                        legend('top', paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'),
                               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
                  }
            }
            
            if (NSVarSpes$retn == 'H') {
                  #Horisontale søyler
                  ymax <- antGr*1.4
                  xmax <- min(max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.25, 100)
                  #par('fig'=c(0.1, 1, 0, 0.9))
                  pos <- barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel,
                                 cex.lab=cexleg,col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0,ymax))	#
                  mtext(at=pos+0.1, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)	#text=rev(grtxt)
                  if (medSml == 1) {
                        points(as.numeric(rev(AggVerdier$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
                        legend('topleft', c(paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'), paste0('Landet forøvrig (N=', Nfig$Rest,')')),
                               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=NA,
                               lwd=lwdRest, ncol=2, cex=cexleg)
                  } else {
                        legend('top', paste0(hovedgrTxt, ' (N=', Nfig$Hoved,')'),
                               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
                  }
            }
            
            title(NSVarSpes$tittel, line=1, font.main=1, cex.main=1.5)
            
            #Tekst som angir hvilket utvalg som er gjort
            mtext(utvalgTxt, side=3, las=1, cex=cexgr, adj=0, col=farger[1], line=c(3+0.9*((NutvTxt-1):0)))
            
            par('fig'=c(0, 1, 0, 1))
            if ( outfile != '') {dev.off()}
            
      }
}

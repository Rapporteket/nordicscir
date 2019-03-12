#' Søylediagram som viser flere kvalitetsindikatorer
#'
#' Funksjonen genererer en figur med ei gruppe kvalitetsindikatorer.
#' 
#' Argumentet \emph{valgtVar} kan bare være "kvalInd".
#'
#' @param NSdata - Alle nødvendige variable fra hvert skjema.
#' @param outfile - navn på fil figuren skrives ned til.
#' @param reshID - avdelingsid for egen avdeling, standard: 0-hele landet.
#' @param valgtVar - variabelen det skal genereres resultat for 
#' @param preprosess Preprosesser data
#'                 0: Nei
#'                 1: Ja (Standard)
#' @param hentData Gjør spørring mot database hvis data ikke er levert fra andre kilder.
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#' @param figurtype angir hvilken figurtype som skal lages: andeler, gjsnGrVar
#'
#'
#' @return Søylediagram samling av kvalitetsindikatorer
#'
#' @export


NSFigKvalInd <- function(NSdata, outfile='', valgtVar,
                           datoFra='2010-01-01', datoTil='2050-01-01', AIS='',
                           minald=0, maxald=130, erMann=99, traume='',paratetra=99,
                           enhetsUtvalg=1, reshID, hentData=0, preprosess=1) {

  if (hentData == 1) { #Ikke oppdatert NSRegDataSQL til å ta høyde for denne. IKKE hent data...!
        RegData <- NSRegDataSQL(valgtVar='kvalInd')
  }

  # Hvis NSdata ikke har blitt preprosessert.
 if (preprosess == 1){
         HovedSkjema <- NSPreprosesser(HovedSkjema)
# Usikker på om skal koble her. Bedre å vente til utvalg er gjort slik at utvalg gjøres bare en gang.
                  # LivskvalitetH <- KobleMedHoved(HovedSkjema,Livskvalitet)
         # KontrollH <- KobleMedHoved(HovedSkjema,Kontroll)
         # UrinH <- KobleMedHoved(HovedSkjema,Urin)
         # TarmH <- KobleMedHoved(HovedSkjema,Tarm)
         # Aktivitet <- KobleMedHoved(AktivFunksjon, AktivTilfredshet) #[,-which(names(AktivFunksjon)=='HovedskjemaGUID')]
         # AktivitetH <- KobleMedHoved(HovedSkjema, Aktivitet)
         # FunksjonH <- KobleMedHoved(HovedSkjema, AktivFunksjon)
         # TilfredsH <- AktivitetH
   }


  '%i%' <- intersect

  #--------------- Gjøre utvlag og Tilrettelegge variable ------------------------------
  
  Utvalg <- NSUtvalg(RegData=HovedSkjema, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                     erMann=erMann, traume=traume, AIS=AIS, paratetra=paratetra,
                     enhetsUtvalg = enhetsUtvalg, reshID = reshID)
  RegData <- Utvalg$RegData
  utvalgTxt <- Utvalg$utvalgTxt
  medSml <- Utvalg$medSml
  #smltxt <- Utvalg$smltxt
  #hovedgrTxt <- Utvalg$hovedgrTxt
  
   # KImaal <- 

  finnRegData(valgtVar='', Data = AlleTab)

    tittel <- 'Kvalitetsindikatorer'
    
    grNavn <- c('Nevrologisk klassifikasjon v/innkomst og utreise', #Grønn>90%, Gul 75-90
               'Kartlagt og vurdert blærefunksjon', #Grønn>90%, Gul 80-90
               'Blæretømmingsregime, paraplegi', ##Grønn>75%, Gul 50-75
               'SKrevet ut til sykehjem', #Grønn<10%, Gul 10-20
               'Kartlagt og vurdert tarmfunksjon', #Grønn>90%, Gul 80-90
               'Rapportert livskvalitet under primæropphold',  #Grønn>90%, Gul 75-90
               'Registreringsforsinkelse < 1 måned')
               # ,'Kartlagt og vurdert funksjon ift. aktivitet og deltagelse',
               # 'Selvrapportert tilfredshet ift. aktivitet og deltagelse',
               # 'Etterlevelse av anbefaling for oppfølging')

    # Klassifisert ved både inn- og utreise:
          HovedSkjema$NevrKlassIU <- 0
          HovedSkjema$NevrKlassIU[which((HovedSkjema$AAis %in% 1:5) & (HovedSkjema$FAis %in% 1:5))] <- 1
 #Kartlagt og vurdert blærefunksjon
          UrinH$BlaFunk <- 'tom'
 #Andel personer med paraplegi, som ved utreise fra ryggmargsskadeavdeling har 
#normalisert blæretømming (EmbladM1=TRUE) eller intermitterende kateterisering (EmbladM7=TRUE) som hovedmetode.
                # >= as.Date('2015-01-01')
                
  #SKrevet ut til sykehjem              
                
          
    # variable <- c('PostOpKomplReop', 'LapKomplikasjoner', 'HysKomplikasjoner',
    #               'LapKonvertert', 'HysKonvertert', 'Opf0') #, 'Innen6uker')
    # 
    # #Reoperasjon som følge av komplikasjon
    # #Kode 0: Nei, 1:Ja
    # RegData$PostOpKomplReop <- NA
    # RegData$PostOpKomplReop[which(RegData$Opf0Komplikasjoner %in% 0:1)
    #                         %i% which(RegData$Opf0Status == 1)] <- 0
    # RegData$PostOpKomplReop[which(RegData$Opf0Reoperasjon == 1)] <- 1
    # 
    # RegData$Opf0 <- 1
    # datoTil <- min(as.POSIXlt(datoTil), as.POSIXlt(Sys.Date() - 8*7))
    # RegData$Opf0[RegData$InnDato>datoTil] <- NA
    # RegData$Opf0[RegData$Opf0metode %in% 1:2] <- 0

        ind <- Utvalg$ind
        if (medSml == 0) {ind$Rest <- 0}
        N <- list(Hoved = length(ind$Hoved), Rest = length(ind$Rest))
        Nfig <- N
        Ngr <- N
        AggVerdier <- list(Hoved = 0, Rest = 0)
        xakseTxt <- 'Andel (%)'
        xmax <- 100
        indUtHoved <- NULL
        indUtRest <- NULL
        
    Ngr$Hoved <- apply(RegData[ind$Hoved,variable], MARGIN=2,
                                    FUN=function(x) sum(x == 1, na.rm=T))
    #N$ gjelder selv om totalutvalget er ulikt for de ulike variablene i flerevar
    N$Hoved <- apply(RegData[ind$Hoved,variable], MARGIN=2,
                                  FUN=function(x) sum(x %in% 0:1, na.rm=T))
    AggVerdier$Hoved <- 100*Ngr$Hoved/N$Hoved

    if (Utvalg$medSml==1) {
      Ngr$Rest <- apply(RegData[ind$Rest,variable], MARGIN=2,
                                     FUN=function(x) sum(x == 1, na.rm=T))
      N$Rest <- apply(RegData[ind$Rest,variable], MARGIN=2,
                                   FUN=function(x) sum(x %in% 0:1, na.rm=T))
      AggVerdier$Rest <- 100*Ngr$Rest/N$Rest
      }

    indUtHoved <- N$Hoved < Ngrense
    indUtRest <- N$Rest < Ngrense
    AggVerdier$Hoved[indUtHoved] <- NA
    AggVerdier$Rest[indUtRest] <- NA

    xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.15
    grtxt <- paste0(grNavn, ' (N=', N$Hoved, ')')
    grtxt[indUtHoved] <-   paste0(grNavn[indUtHoved], ' (N<', Ngrense, ')')


      Nfig$Hoved <- ifelse(min(N$Hoved)==max(N$Hoved),
                           min(N$Hoved[1]),
                           paste0(min(N$Hoved),'-',max(N$Hoved)))
      Nfig$Rest <- ifelse(min(N$Rest)==max(N$Rest),
                          min(N$Rest[1]),
                          paste0(min(N$Rest),'-',max(N$Rest)))

  soyletxt <- sprintf(paste0('%.1f'), AggVerdier$Hoved)
  soyletxt[indUtHoved] <- ''

  cexgr <- 1-ifelse(length(soyletxt)>20, 0.25*length(soyletxt)/60, 0)
  grtxt2 <- paste0(sprintf('%.1f',AggVerdier$Hoved), '%') #paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')


  ###-----------Figur---------------------------------------
  if ( max(N$Hoved) < Ngrense | 	(Utvalg$medSml ==1 & max(N$Rest)< Ngrense)) {
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main=tittel)	#
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, paste0('Færre enn ', Ngrense, ' "egne" registreringer eller \n
                          færre enn ', Ngrense, ' i sammenligningsgruppe'), cex=1.2)
    if ( outfile != '') {dev.off()}

  } else {


    ###Innparametre til evt. funksjon: subtxt, grtxt, grtxt2, tittel, AggVerdier, utvalgTxt, retn, cexgr
    FigTypUt <- figtype(outfile, fargepalett=Utvalg$fargepalett)
    #Tilpasse marger for ? kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.65)
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 3	#tykkelse på linja som repr. landet
    cexleg <- 1	#Størrelse på legendtekst
    cexgr <- 1

      pos <- barplot(as.numeric(AggVerdier$Hoved), horiz=TRUE, beside=TRUE, las=1, xlab=xakseTxt, #main=tittel,
                     col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
      if (Nfig$Hoved>0) {mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25)}
      soyleXpos <- 1.12*xmax*max(strwidth(soyletxt, units='figure')) # cex=cexgr
      text(x=soyleXpos+xmax*0.02, y=pos+0.1, soyletxt, las=1, cex=cexgr, adj=1, col=farger[4])	#AggVerdier, hvert sykehus

      if (Utvalg$medSml == 1) {
        points(as.numeric(AggVerdier$Rest), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste0(Utvalg$hovedgrTxt, ' (N=', Nfig$Hoved,')'),
                        paste0(Utvalg$smltxt, ' (N=', Nfig$Rest,')')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
               lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
      } else {
        legend('top', paste0(Utvalg$hovedgrTxt, ' (N=', Nfig$Hoved,')'),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
      }

    title(tittel, line=1, font.main=1)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    if ( outfile != '') {dev.off()}
  }

  UtData <- list(paste0(toString(tittel),'.'), AggVerdier, N, grtxt )
  names(UtData) <- c('tittel', 'AggVerdier', 'Antall', 'GruppeTekst')
  return(invisible(UtData))
}


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
#' @param reshID - avdelingsid for egen avdeling, standard: 0-hele landet/Norden.
#' @param valgtVar - variabelen det skal genereres resultat for
#' @param preprosess Preprosesser data
#'                 0: Nei
#'                 1: Ja (Standard)
#' @param hentData Gjør spørring mot database hvis data ikke er levert fra andre kilder.
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#' @param regsiter Må angis hvis hentData=1. Valg: 'norscir' (standard), 'nordicscir'
#' @param figurtype angir hvilken figurtype som skal lages: andeler, gjsnGrVar
#' @param datoUt Om man skal velge inn eller ut-dato som grunnlag for datofiltrering
#'
#' @export

NSFigAndelerSh <- function(RegData, outfile='', valgtVar='Alder',
                           hentData=0, register='norscir', preprosess=1,
                         datoFra='2010-01-01', datoTil='2050-01-01', datoUt=0, AIS='',
                         minald=0, maxald=130, erMann=99, traume='alle',nivaaUt=99, ...) {

   if ("session" %in% names(list(...))) {
      rapbase::repLogger(session = list(...)[["session"]], msg = "Andeler per sykehus/land, figur")
   }
   if (hentData == 1) {
            RegData <- NSRegDataSQL(valgtVar=valgtVar, register = register)
      }

      if ( dim(RegData)[1] == 0 ) {
            print('RegData er tom')
            FigDataParam <- list(AggVerdier=NA,
                                 N=0,
                                 Ngr=0,
                                 KImaal <- '',
                                 grtxt2='',
                                 grtxt='',
                                 tittel='Ingen registreringer',
                                 retn='',
                                 xAkseTxt='',
                                 yAkseTxt='',
                                 utvalgTxt='',
                                 fargepalett='',
                                 hovedgrTxt='',
                                 smltxt='')

            FigTypUt <- rapFigurer::figtype(outfile)
            plot.new()
            text(0.5, 0.6, paste0('Ingen registreringer i Rapportdatabasen'), cex=1.2)
            if ( outfile != '') {dev.off()}
      } else {

            if (preprosess == 1) {
                  RegData <- NSPreprosesser(RegData)
            }

            #--------------- Tilrettelegge variable og gjøre utvalg ------------------------------
            NSVarSpes <- NSVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype='andeler')
            RegData <- NSVarSpes$RegData

            #NSUtvalg -> NSUtvalgEnh
            Utvalg <- NSUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                               erMann=erMann, traume=traume, AIS=AIS, nivaaUt=nivaaUt, datoUt=datoUt)
            RegData <- Utvalg$RegData
            utvalgTxt <- Utvalg$utvalgTxt
            #Benytter variabelen LandKode til å sjekke om det er nordiske eller norske data
            nordisk <- ifelse(sum(as.numeric(unique(RegData$LandKode)))==1, 0, 1)
            grVar <- c('ShNavn', 'Land')[nordisk+1]
            RegData$GrVar <- as.factor(RegData[ ,grVar])

            #--------------- Gjøre beregninger ------------------------------
            #      AggVerdier <- list(Hoved = 0, Rest =0)
            #N <- table(RegData$GrVar)   #Nevner
            variable <- NSVarSpes$variable
            flerevar <- NSVarSpes$flerevar

            # dataAgg <- aggregate(RegData$Alder, by=list(RegData$VariabelGr,RegData$GrVar),
            #                      FUN='length')
            # dataAgg <- aggregate(RegData$Alder, by=list(RegData$VariabelGr,RegData$GrVar),
            #                  FUN = function(x){})
            # ggplot2::ggplot(dataAgg, aes(fill=Group.2, y=x, x=Group.1)) +
            #       geom_bar(position="dodge", stat="identity")
            # ggplot2::ggplot(dataAgg, aes(fill=Group.1, y=x, x=Group.2)) +
            #       geom_bar(position="fill", stat="identity")
            # #geom_bar(position="stack", stat="identity")

            Ngr <- switch(as.character(flerevar),
                                '0' = t(table(RegData$VariabelGr, RegData$GrVar)),
                                '1' = apply(X=RegData[,variable], MARGIN = 2,
                                            FUN=function(x) table(RegData$GrVar[x==1]))
                          ) #function(x) sum(x == 1, na.rm=T))
            #N$ gjelder selv om totalutvalget er ulikt for de ulike variablene i flerevar
            N <- switch(as.character(flerevar),
                              '0' = t(table(RegData$GrVar)), #sum(Ngr),
                              '1' = apply(RegData[ ,variable], MARGIN=2,
                                          FUN=function(x) table(RegData$GrVar)))
                                          #FUN=function(x) sum(x %in% 0:1, na.rm=T)))


            AggVerdier <- switch(as.character(flerevar),
                           '0' = 100*prop.table(ftable(RegData[ ,c('GrVar', 'VariabelGr')]),1), #100*t(sweep(Ngr, MARGIN=2, N, `/`)), #
                           '1' = 100*Ngr/N) #100*t(sweep(Ngr, MARGIN=2, N, `/`)))
          AggTot <- switch(as.character(flerevar),
                           '0' = 100*prop.table(table(RegData[ ,'VariabelGr'])),
                           '1' = 100*apply(X=RegData[,variable], MARGIN = 2,
                                           FUN=function(x) sum(x==1)/length(x %in% 0:1)))
Ngrense <- 5
indNgrense <- N < Ngrense
AggVerdier[indNgrense] <- NA

            if(flerevar==1) {
                  Nfig <- apply(N, MARGIN = 1, FUN = max)
            } else {
                  Nfig <- N}



            grtxt <- NSVarSpes$grtxt
            grtxt2 <- paste0('(', sprintf('%.1f',AggTot), '%)')
            grtxtpst <- paste0(grtxt, '\n(', sprintf('%.1f',AggTot), '%)')
            cexgr <- NSVarSpes$cexgr
            enhTxt <- rownames(Ngr) #attributes(AggVerdier)$row.vars$GrVar
            anttxt <- paste0(' (N=', Nfig,')')
            anttxt[Nfig < Ngrense] <- paste0(' (N < ', Ngrense, ')')
            legendtxt <- paste0(enhTxt, anttxt)
            yAkseTxt='Andel pasienter (%)'

            FigDataParam <- list(AggVerdier=AggVerdier,
                                 N=N,
                                 Ngr=Ngr,
                                 Nfig=Nfig,
                                 KImaal <- NSVarSpes$KImaal,
                                 grtxt2=grtxt2,
                                 grtxt=NSVarSpes$grtxt,
                                 tittel=NSVarSpes$tittel,
                                 retn=NSVarSpes$retn,
                                 xAkseTxt=NSVarSpes$xAkseTxt,
                                 yAkseTxt=yAkseTxt,
                                 utvalgTxt=Utvalg$utvalgTxt,
                                 fargepalett=Utvalg$fargepalett)


            #-----------Figur---------------------------------------
            #FigurAndeler <- function(     ){
            #-----Hvis få registreringer: ---------------------
            #Ngrense <- 5      # 5
            if (sum(Nfig) < Ngrense) {
                  FigTypUt <- rapFigurer::figtype(outfile)
                  farger <- FigTypUt$farger
                  plot.new()
                  title(NSVarSpes$tittel)	#, line=-6)
                  legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
                  text(0.5, 0.6, paste0('Færre enn ', Ngrense, ' registreringer,'), cex=1.2)
                  if ( outfile != '') {dev.off()}
            } else {

                  #Plottspesifikke parametre:
                  FigTypUt <- rapFigurer::figtype(outfile, fargepalett=Utvalg$fargepalett)
                  #Tilpasse marger for å kunne skrive utvalgsteksten
                  NutvTxt <- length(utvalgTxt)
                  vmarg <- switch(NSVarSpes$retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.8))
                  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

                  antGr <- length(grtxt)
                  farger <- FigTypUt$farger
                  fargeHoved <- rev(farger)[1:length(enhTxt)]
                  cexleg <- 1	#Størrelse på legendtekst


                  if (NSVarSpes$retn == 'V' ) {
                        #Vertikale søyler eller linje
                        #if (grtxt2[1] == '') {grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier), '%)')}
                        ymax <- max(AggVerdier,na.rm=T)*1.15
                        pos <-   barplot(AggVerdier, beside=T, ylab="Andel pasienter (%)",
                                         cex.lab=cexleg, sub=NSVarSpes$xAkseTxt, cex.sub=cexleg,
                                         col=fargeHoved, border='white', ylim=c(0, ymax))
                        mtext(at=pos[2,], grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
                        mtext(at=pos[2,], grtxt2, side=1, cex=cexgr-0.1, adj=0.5, line=1.5) #las=txtretn,
                        mtext(c('Hele landet:', 'Norden')[nordisk+1],
                              at=-2, side=1, cex=cexgr-0.1, adj=0, line=1.5)
                        legend('top', legendtxt,
                                     border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
                  }

                  if (NSVarSpes$retn == 'H') {

                        #Horisontale søyler
                        xmax <- min(max(c(AggVerdier),na.rm=T)*1.25, 100)
                        #dplyr::arrange(-dplyr::row_number(AggVerdier))
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

            } #Nok observasjoner
      }  #Figur


      return(invisible(FigDataParam))

      }

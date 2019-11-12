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
#' @param preprosess Preprosesser data
#'                 0: Nei
#'                 1: Ja (Standard)
#' @param hentData Gjør spørring mot database hvis data ikke er levert fra andre kilder.
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#' @param figurtype angir hvilken figurtype som skal lages: andeler, gjsnGrVar
#' @param datoUt Om man skal velge inn eller ut-dato som grunnlag for datofiltrering
#' 
#' @export

NSFigAndelerSh <- function(RegData, outfile='', valgtVar, hentData=0, preprosess=1, 
                         datoFra='2010-01-01', datoTil='2050-01-01', datoUt=0, AIS='',
                         minald=0, maxald=130, erMann=99, traume='alle',nivaaUt=99) {
      
      if (hentData == 1) {
            RegData <- NSRegDataSQL(valgtVar=valgtVar)
      }
      
      if ( dim(RegData)[1] == 0 ) { 
            print('RegData er tom')
            FigDataParam <- list(AggVerdier=NA, 
                                 N=0, 
                                 Ngr=0,	
                                 KImaal <- '',
                                 #soyletxt=soyletxt,
                                 grtxt2='', 
                                 grtxt='',
                                 tittel='Ingen registreringer', 
                                 retn='', 
                                 xAkseTxt='',
                                 yAkseTxt='',
                                 utvalgTxt='', 
                                 fargepalett='', 
                                 #medSml='',
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
            
            
            Utvalg <- NSUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                               erMann=erMann, traume=traume, AIS=AIS, nivaaUt=nivaaUt, datoUt=datoUt)
            RegData <- Utvalg$RegData
            utvalgTxt <- Utvalg$utvalgTxt
            #medSml <- Utvalg$medSml
            
            #--------------- Gjøre beregninger ------------------------------
            #      AggVerdier <- list(Hoved = 0, Rest =0)
            N <- table(RegData$ShNavn)   #Nevner
            #Ngr <- tapply(RegData) #Teller 
            #Nfig <- list(Hoved = 0, Rest =0) #figurtekst: N i legend
            variable <- NSVarSpes$variable
            flerevar <- NSVarSpes$flerevar
            
            # dataAgg <- aggregate(RegData$Alder, by=list(RegData$VariabelGr,RegData$ShNavn),
            #                      FUN='length')
            # dataAgg <- aggregate(RegData$Alder, by=list(RegData$VariabelGr,RegData$ShNavn), 
            #                  FUN = function(x){})
            # ggplot2::ggplot(dataAgg, aes(fill=Group.2, y=x, x=Group.1)) + 
            #       geom_bar(position="dodge", stat="identity")
            # ggplot2::ggplot(dataAgg, aes(fill=Group.1, y=x, x=Group.2)) + 
            #       geom_bar(position="fill", stat="identity")
            # #geom_bar(position="stack", stat="identity")
            
            Ngr <- switch(as.character(flerevar), 
                                '0' = table(RegData$VariabelGr, RegData$ShNavn),
                                '1' = apply(RegData[ ,variable], MARGIN=2, 
                                            FUN=function(x) sum(x == 1, na.rm=T)))
            #N$ gjelder selv om totalutvalget er ulikt for de ulike variablene i flerevar
            N <- switch(as.character(flerevar), 
                              '0' = table(RegData$ShNavn), #sum(Ngr),	
                              '1' = apply(RegData[ ,variable], MARGIN=2, 
                                          FUN=function(x) sum(x %in% 0:1, na.rm=T)))
            AggVerdier <- 100*prop.table(ftable(RegData[ ,c('ShNavn', 'VariabelGr')]),1)
          AggTot <- 100*prop.table(table(RegData[ ,'VariabelGr']))
            
            
           #  #-------Eksempel
           #  specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
           #  condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
           #  value <- abs(rnorm(12 , 0 , 15))
           #  data <- data.frame(specie,condition,value)
           #  ggplot(data, aes(fill=condition, y=value, x=specie)) + 
           #        geom_bar(position="dodge", stat="identity")
           # #------------------
            
           
            if(flerevar==1) {
                  Nfig <- max(N)
                  #ifelse(min(N$Hoved)==max(N$Hoved),
                  #              min(N$Hoved[1]), 
                  #               paste0(min(N$Hoved),'-',max(N$Hoved)))
                  Nfig <- max(N)
                  #ifelse(min(N$Rest)==max(N$Rest),
                  #              min(N$Rest[1]), 
                  #              paste0(min(N$Rest),'-',max(N$Rest)))
            } else {
                  Nfig <- N}
            
            
            
            
            grtxt <- NSVarSpes$grtxt
            #grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier), '%)')
            grtxt2 <- paste0('(', sprintf('%.1f',AggTot), '%)')
            grtxtpst <- paste0(grtxt, '\n(', sprintf('%.1f',AggTot), '%)')
            cexgr <- NSVarSpes$cexgr
            hovedgrTxt <- levels(RegData$ShNavn) #attributes(AggVerdier)$row.vars$ShNavn
            yAkseTxt='Andel pasienter (%)'
            
            FigDataParam <- list(AggVerdier=AggVerdier, 
                                 N=N, 
                                 Ngr=Ngr,	
                                 Nfig=Nfig,
                                 KImaal <- NSVarSpes$KImaal,
                                 #soyletxt=soyletxt,
                                 grtxt2=grtxt2, 
                                 grtxt=NSVarSpes$grtxt,
                                 tittel=NSVarSpes$tittel, 
                                 retn=NSVarSpes$retn, 
                                 xAkseTxt=NSVarSpes$xAkseTxt,
                                 yAkseTxt=yAkseTxt,
                                 utvalgTxt=Utvalg$utvalgTxt, 
                                 fargepalett=Utvalg$fargepalett) 
                                 #,hovedgrTxt=hovedgrTxt)
            
            #-----------Figur---------------------------------------
            #FigurAndeler <- function(     ){
            #-----Hvis få registreringer: ---------------------
            Ngrense <- 5      # 5
            if (sum(Nfig) < Ngrense) {
                  FigTypUt <- rapFigurer::figtype(outfile)
                  farger <- FigTypUt$farger
                  plot.new()
                  title(NSVarSpes$tittel)	#, line=-6)
                  legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
                  text(0.5, 0.6, paste0('Færre enn ', Ngrense, ' registreringer,'), cex=1.2)
                  if ( outfile != '') {dev.off()}
            } else {
                  
                  #Inn parametre: xAkseTxt, grtxt, grtxt2, tittel, AggVerdier
                  #Plottspesifikke parametre:
                  
                  #Plottspesifikke parametre:
                  FigTypUt <- rapFigurer::figtype(outfile, fargepalett=Utvalg$fargepalett)
                  #Tilpasse marger for å kunne skrive utvalgsteksten
                  NutvTxt <- length(utvalgTxt)
                  vmarg <- switch(NSVarSpes$retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.8))
                  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
                  
                  farger <- FigTypUt$farger
                  fargeHoved <- rev(farger[c(1,2,4)])
                  antGr <- length(grtxt)
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
                        mtext('(Hele landet:)',  at=-2, side=1, cex=cexgr-0.1, adj=0, line=1.5) 
                        legend('top', paste0(hovedgrTxt, ' (N=', Nfig,')'),
                                     border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
                  }
                  
                  if (NSVarSpes$retn == 'H') {
                        
                        #Horisontale søyler
                        #ymax <- antGr*3*1.4
                        xmax <- min(max(c(AggVerdier),na.rm=T)*1.25, 100)
                        #dplyr::arrange(-dplyr::row_number(AggVerdier))
                        pos <-   barplot(AggVerdier[,ncol(AggVerdier):1], horiz=TRUE, beside=T, xlab="Andel pasienter (%)", 
                                         cex.lab=cexleg, cex.sub=cexleg, #sub=NSVarSpes$xAkseTxt, 
                                         col=fargeHoved, border='white', xlim=c(0, xmax)) #, ylim=c(0, ymax))
                        mtext(at=pos[2,]+0.1, text=rev(grtxtpst), side=2, las=1, cex=cexgr, adj=1, line=0.25)	
                              legend('top', paste0(hovedgrTxt, ' (N=', Nfig,')'),
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
      
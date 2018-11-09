#' Funksjon som genererer en figur med gjennomsnitt/median
#'
#' Funksjon som genererer en figur med gjennomsnitt/median for hvert sykehus
#' og kan ta inn ulike numeriske variable. Funksjonen er delvis skrevet for å
#' kunne brukes til andre grupperingsvariable enn sykehus.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Aldersfordeling, 15-årige grupper 
#'     \item DagerRehab: antall dager med rehabilitering [RehabDy]
#'     \item DagerTilRehab: antall dager før rehabilitering [BeforeRehDy]
#'     \item OpphTot: Lengde på opphold – totalt [HosptlDy] 
#'     \item Permisjon: Antall døgn ute av sykehus [OutOfHosptlDy]
#'    }
#'    
#' @inheritParams NSFigAndeler
#' @inheritParams NSUtvalg
#' @param valgtMaal - 'med' = median. Alt annet gir gjennomsnitt
#'
#' @export

NSFigGjsnGrVar <- function(RegData, valgtVar, valgtMaal='gjsn', grVar='ShNavn',
                              datoFra='2010-01-01', datoTil='2050-12-31', 
					AIS='', minald=0, maxald=130, erMann='', traume='', paratetra=99, 
					preprosess=1, outfile='', hentData=0) {

      if (hentData == 1) {
            RegData <- NSRegDataSQL(valgtVar=valgtVar)
      }
      if (preprosess == 1) {
            RegData <- NSPreprosesser(RegData)
      }

      #------- Tilrettelegge variable
      NSVarSpes <- NSVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype='gjsnGrVar')
      RegData <- NSVarSpes$RegData
      
      #------- Gjøre utvalg
      Utvalg <- NSUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
                         erMann=erMann, traume=traume, AIS=AIS, paratetra=paratetra)
      RegData <- Utvalg$RegData
      utvalgTxt <- Utvalg$utvalgTxt
            
  RegData[ ,grVar] <- factor(RegData[ ,grVar]) 
#Grupper som ikke har registreringer vil nå ikke komme med i oversikta. Gjøres dette tidligere, vil alle
#grupper komme med uansett om de ikke har registreringer.
  
  '%i%' <- intersect

  N <- dim(RegData)[1]
  Ngrense <- 10		#Minste antall registreringer for at et sykehus skal bli vist
  if(N>0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}
  Ngrtxt <- paste0(' (', as.character(Ngr),')') 
  indGrUt <- which(Ngr < Ngrense)
  if (length(indGrUt)==0) { indGrUt <- 0}
  Ngrtxt[indGrUt] <- paste0(' (<', Ngrense,')')	
  indGrUt <- which(Ngr < Ngrense)

  vt <- switch(valgtVar, 
               Alder='alder ved innleggelse',
               DagerRehab='antall dager med rehabilitering',
               DagerTilRehab='antall dager før rehabilitering',
               OpphTot= 'totalt opphold',
               RegForsinkelse= 'registreringsforsinkelse',
               LivsGen= 'tilfredshet med livet',
            LivsFys = 'tilfredshet, fysisk helse',
            LivsPsyk = 'tilfredshet, psykisk helse'
               )
  
  
  t1 <- switch(valgtMaal,
               med = 'Median ',
               gjsn = 'Gjennomsnittlig ')
  tittel <- paste0(t1, vt)

  KIHele <- c(0,0)    
  KIned <- c(0,0)
  KIhele <- c(0,0)
  dummy0 <- NA #-0.0001

    #Kommer ut ferdig sortert!
  if (valgtMaal=='med') {
        MedIQR <- plot(RegData[ ,grVar], RegData$Variabel, notch=TRUE, plot=FALSE)
        MedIQR$stats[ ,indGrUt] <- dummy0
        MedIQR$conf[ ,indGrUt] <- dummy0
        sortInd <- order( MedIQR$stats[3,], decreasing=NSVarSpes$sortAvtagende, na.last = FALSE) 
        Midt <- as.numeric(MedIQR$stats[3, sortInd])
        KIned <- MedIQR$conf[1, sortInd]
        KIopp <- MedIQR$conf[2, sortInd]
        MedIQRHele <-  boxplot.stats(RegData$Variabel, do.conf = TRUE)
        MidtHele <- as.numeric(MedIQRHele$stats[3])	#median(RegData$Variabel)
        KIHele <- MedIQRHele$conf
        #Hvis vil bruke vanlige konf.int:
        #j <- ceiling(N/2 - 1.96*sqrt(N/4))
        #k <- ceiling(N/2 + 1.96*sqrt(N/4))
        #KIHele <- sort(RegData$Variabel)[c(j,k)]
        #The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).) 
        #They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared, 
        #and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give 
        #roughly a 95% confidence interval for the difference in two medians. 	
  } 
  
  if (valgtMaal=='gjsn') {	#Gjennomsnitt er standard, men må velges.
              Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
              SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
              MidtHele <- mean(RegData$Variabel)	#mean(RegData$Variabel)
              KIHele <- MidtHele + sd(RegData$Variabel)/sqrt(N)*c(-2,2)
        Gjsn[indGrUt] <- dummy0
        SE[indGrUt] <- 0
        sortInd <- order(Gjsn, decreasing=NSVarSpes$sortAvtagende, na.last = FALSE) 
        Midt <- Gjsn[sortInd] #as.numeric(gjsn[sortInd])
        KIned <- Midt - 2*SE[sortInd]
        KIopp <- Midt + 2*SE[sortInd]
  }
  
  
  Ngr <- Ngr[sortInd] #list(Hoved=Ngr[sortInd], Rest=0)
  GrNavnSort <- paste0(names(Ngr), Ngrtxt[sortInd])
  soyletxt <- sprintf('%.1f', Midt) #sprintf(paste0('%.', AntDes,'f'), Midt) 
  indUT <- which(is.na(Midt))  #Rydd slik at bare benytter indGrUt
  soyletxt[indUT] <- ''
  KIned[indUT] <- NA
  KIopp[indUT] <- NA
  AggVerdier <- list(Hoved=Midt, Rest=0, KIned=KIned, KIopp=KIopp, KIHele=KIHele)
  
  
  #indGrOK <- which(Midt>0)
  xmax <-  min(1.1*max(c(Midt, KIned, KIopp), na.rm=T), 1.5*max(Midt, na.rm = T))
  xlabt <- switch(valgtVar, Alder='alder (år)',
                  DagerRehab='dager',
                  DagerTilRehab='dager',
                  OpphTot= 'dager',
                  RegForsinkelse = 'dager')
  
  #Se NSFigSoyler for forklaring av innhold i lista gjsnGrVarData
  SentralmaalTxt <- switch(valgtMaal,
                     med = 'Median',
                     gjsn = 'Gjennomsnitt')
  
  GjsnGrVarData <- list(AggVerdier=AggVerdier, #Endres til Soyleverdi? Evt. AggVerdier
                        AggTot=MidtHele, #Til AggVerdiTot?
                        N=list(Hoved=N), 
                        Ngr=Ngr,
                        grtxt2='', 
                        medKI=1,
                        KImaal = NSVarSpes$KImaal,
                        soyletxt=soyletxt,
                        grtxt=GrNavnSort,
                        valgtMaal=valgtMaal,
                        tittel=tittel,    #NSVarSpes$tittel, 
                        SentralmaalTxt = SentralmaalTxt,
                        #yAkseTxt=yAkseTxt, 
                        retn='H', 
                        xAkseTxt=NSVarSpes$xAkseTxt,
                        grTypeTxt='sykehus',   #NSUtvalg$grTypeTxt,			 
                        utvalgTxt=Utvalg$utvalgTxt, 
                        fargepalett=Utvalg$fargepalett, 
                        medSml=Utvalg$medSml, 
                        smltxt=Utvalg$smltxt)
  

  #FigDataParam skal inn som enkeltparametre i funksjonskallet
  lagFig <- 0
  if (lagFig == 1) {
        cexgr <- 1-ifelse(length(soyletxt)>20, 0.25*length(soyletxt)/60, 0)
        NIRFigSoyler(RegData, AggVerdier=AggVerdier, AggTot=MidtHele, Ngr=Ngr, N=list(Hoved=N), cexgr=cexgr, 
                     tittel=tittel, valgtMaal=valgtMaal,
                     smltxt=NIRUtvalg$smltxt, yAkseTxt=yAkseTxt,utvalgTxt=NIRUtvalg$utvalgTxt, 
                     grTypeTxt=NIRUtvalg$grTypeTxt,  fargepalett=NIRUtvalg$fargepalett, grtxt=GrNavnSort, 
                     soyletxt=soyletxt,  grVar=grVar, medKI=medKI, KImaal = NIRVarSpes$KImaal,
                     medSml=NIRUtvalg$medSml, xAkseTxt=NIRVarSpes$xAkseTxt, outfile=outfile)
  }

#--------------------------FIGUR---------------------------------------------------

    if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
          rapbase::figtype(outfile)
          plot.new()
          if (dim(RegData)[1]>0) {
                tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
                title(main=tittel, cex=0.95)
                legend('topleft',utvalgTxt, bty='n', cex=0.9)
          } else {
                tekst <- 'Ingen registrerte data for dette utvalget' }
          text(0.5, 0.5, tekst,cex=1)	#, family="sans")
          if ( outfile != '') {dev.off()}
    } else {
          #--------------------------------------------------------
          
          #Plottspesifikke parametre:
    FigTypUt <- rapbase::figtype(outfile, fargepalett=Utvalg$fargepalett)
    farger <- FigTypUt$farger
    cexleg <- 1.1	#Størrelse på legendtekst
    cexgr <- 1.1
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexgr)*0.9)
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    #Må def. pos først for å få strek for hele gruppa bak søylene
    ### reverserer for å slippe å gjøre det på konf.int
    pos <- rev(barplot(rev(as.numeric(Midt)), horiz=T, border=NA, col=farger[3], axes = FALSE,
                   xlim=c(0,xmax), ylim=c(0.3,4.1), xlab='', las=1) )	#xlim=c(0,ymax),
    ybunn <- 0.1
    ytopp <- 3.7	#c(0, max(posKI) + min(posKI))
    polygon( c(rep(KIHele[1],2), rep(KIHele[2],2)), c(ybunn, ytopp, ytopp, ybunn),
             col=farger[4], border=farger[4])
    lines(x=rep(MidtHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=3)
    #	legend('topright', xjust=1, fill=c(farger[4],'white'), border=c(farger[4],'white'), cex=0.8, #lwd=2, #col=farger[2],
    #		legend='95% konf.int., alle sykehus', bty='o', bg='white', box.col='white')
    legend('topright', fill=c('white', farger[4]),  border='white', lwd=2, cex=cexleg,
           col=c(farger[2], farger[4]), seg.len=0.6, merge=TRUE, bty='n',
           c(paste0('Alle sykehus: ', sprintf('%.1f', MidtHele), ', N=', N), 
             paste0('95% konf.int., alle (',
                   sprintf('%.1f', KIHele[1]), '-', sprintf('%.1f', KIHele[2]), ')')))
    barplot(rev(as.numeric(Midt)), horiz=T, border=NA, col=farger[3], xlim=c(0, xmax), add=TRUE,
            xlab=NSVarSpes$xAkseTxt, cex.lab=cexleg+0.1, cex.sub=cexleg+0.1, cex.axis=cexleg, las=1)
    title(tittel, line=1.1, font.main=1, cex.main=1.5)
    title('med 95% konfidensintervall', font.main=1, line=0)

    soyleXpos <- 1.3*xmax*max(strwidth(soyletxt, units='figure', cex = cexgr)) # cex=cexgr
    text(x=soyleXpos, y=pos+0.1, soyletxt, las=1, cex=cexgr, adj=1, col=farger[1])	#AggVerdier, hvert sykehus
    
    mtext(at=pos+0.15, GrNavnSort, side=2, las=1, cex=cexgr+0.1, adj=1, line=0.25)	#Hvis vil legge på navn som eget steg
    options(warn = -1)	#Unngå melding om KI med lengde 0. Fungerer av en eller annen grunn ikke i pdf.
    arrows(x0=Midt*0.999, y0=pos, x1=KIopp, y1=pos,
           length=0.5/max(pos), code=2, angle=90, lwd=2, col=farger[1])
    arrows(x0=Midt*1.001, y0=pos, x1=KIned, y1=pos,
           length=0.5/max(pos), code=2, angle=90, lwd=2, col=farger[1])

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=cexleg, adj=0, col=farger[1], line=c(3+0.9*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}
    #----------------------------------------------------------------------------------
    }
  return(invisible(GjsnGrVarData))
}

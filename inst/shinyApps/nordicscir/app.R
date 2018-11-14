#Resultattjeneste for NordicScir
library(nordicscir)
library(shiny)
library(knitr)
library(lubridate)
#ibrary(shinyBS) # Additional Bootstrap Controls


ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
      title = 'NORSK SPINNALSKADEREGISTER med FIKTIVE data',
      tabPanel("Viktigste resultater/Oversiktsside",
               #fluidRow(
               #column(width=5,
               h2("Dæsjbord?  - vise viktigste variable/resultater?", align='center' ),
               h2("Evt. gi tilbakemelding på hva som skal være på sida", align='center' ),
               br(),
               
               h2("Månedsrapport"), #),
               
               #downloadButton('mndRapp'),
               downloadButton(outputId = 'mndRapp', label='Månedsrapport-virker ikke på server', 
                              class = "butt"),
               tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
 
               br(),
               br(),
               tags$ul(tags$b('Andre ting å ta stilling til: '),
                       tags$li("Navn på faner"), 
                       tags$li("Layout på sider ?"), 
                       tags$li("Ønskes annen organisering av innhold?"), 
                       tags$li("Hvilke utvalgs/filtreringsmuligheter skal vi ha i de ulike fanene"), 
                       tags$li("Innhold i tabeller som vises i tilknytning til figurer.")
               ),
               br(),
               tags$ul(tags$b('Kommer: '),
                       tags$li("Riktig antall desimaler i tabeller"),
                        tags$li("Mulighet for å laste ned tabeller")
               )
      ), #tab
      
      #-----Registreringsoversikter------------
      tabPanel("Registreringsoversikter",
               sidebarPanel(width=3,
                            h3('Utvalg'),
                            conditionalPanel(condition = "input.ark == 'Belegg'",
                                             dateInput(inputId = 'sluttDatoReg', label = 'Velg sluttdato', language="nb",
                                                       value = Sys.Date(), max = Sys.Date() )
                            ),
                           conditionalPanel(
                                  condition = "input.ark == 'Belegg'",
                                  selectInput(inputId = "tidsenhetReg", label="Velg tidsenhet",
                                              choices = rev(c('År'= 'Aar', 'Måned'='Mnd')))),
                            conditionalPanel(
                                  condition = "input.ark == 'Belegg'",
                                  selectInput(inputId = 'enhetsNivaa', label='Enhetsnivå',
                                              choices = c("Hele landet" = 0, "Egen enhet" = 2))
                                  ),
                           conditionalPanel(
                                  condition = "input.ark == 'Oppfølgingsskjema'",
                                  dateRangeInput(inputId = 'datovalgReg', start = "2017-07-01", end = Sys.Date(),
                                                 label = "Tidsperiode", separator="t.o.m.", language="nb")
                            )
               ),
               
               mainPanel(
                     tabsetPanel(id='ark',
                                 tabPanel('Belegg',
                                          #p('Tabellene viser siste 12 måneder eller siste 5 år'),
                                          uiOutput("undertittelBelegg"),
                                          p("Velg tidsperiode ved å velge sluttdato/tidsenhet i menyen til venstre"), #em(
                                          br(),
                                          h3('Antall registreringer'),
                                          fluidRow(tableOutput("tabAntOpphShMnd12")),
                                          br(),
                                          h3("Belegg på rehabiliteringsavdelinga - ønskes flere/andre variable?"), 
                                          #uiOutput("undertittelBelegg"),
                                          fluidRow( tableOutput("tabBelegg"))
                                 ),
                                 tabPanel('Oppfølgingsskjema',
                                          h3("Antall registreringsskjema med ulike oppfølgingsskjema"),
                                          tableOutput('tabAntTilknyttedeSkjema'),
                                          br(),
                                          h3("Andel (%) registreringsskjema med ulike oppfølgingsskjema"),
                                          tableOutput("tabAndelTilknyttedeSkjema"),
                                          br(),
                                          h3('Antall skjema registrert på opphold i valgte tidsperiode'),
                                          tableOutput('AntallSkjema')
                                 )
                     ))
      ), #tab Registreringsoversikter

#--------Fordelinger-----------            
      tabPanel("Fordelinger",
               sidebarPanel(width = 3,
                            selectInput(
                                  inputId = "valgtVar", label="Velg variabel",
                                  choices = c('Alder' = 'Alder', 
                                              'Ais, innleggelse' = 'AAis' ,
                                              'Ais, kontroll' = 'FAis', 
                                              'Lengde på rehab.opphold' = 'DagerRehab', 
                                              'Tid fra skade til oppstart rehab.' = 'DagerTilRehab', 
                                              'Opphold, totalt antall dager' = 'OpphTot', 
                                              #'Fjern? Permisjon (ant. døgn ute av sykehus) ' = 'Permisjon',
                                              'Utskrevet til' = 'UtTil',
                                              'Skadeårsak ' = 'SkadeArsak',
                                              #'Fjern? Pustehjelp' = 'Pustehjelp[VentAssi]',
                                              'Livskval.: Tilfredshet med livet' = 'LivsGen',
                                              'Livskval.: Tilfredshet med fysisk helse' = 'LivsFys',
                                              'Livskval.: Tilfredshet med psykisk helse' = 'LivsPsyk',
                                              'Ufrivillig urinlekkasje' = 'UrinInkontinens', 
                                              'Urin: Kirurgiske inngrep' = 'UrinKirInngr',
                                              'Urin: Legemiddelbruk' = 'UrinLegemidler',
                                              'Urin: Legemiddelbruk, hvilke' = 'UrinLegemidlerHvilke',
                                              'Urin: Blæretømming, hovedmetode' = 'UrinTomBlareHoved',
                                              'Urin: Blæretømming, tilleggsmetode' = 'UrinTomBlareTillegg',
                                              'Tarm: Avføring, hovedmetode' = 'TarmAvfHoved',
                                              'Tarm: Avføring, tilleggsmetode' = 'TarmAvfTillegg',
                                              'Tarm: Avføringsmiddelbruk' = 'TarmAvfmiddel',
                                              'Tarm: Avføringsmidler, hvilke' = 'TarmAvfmiddelHvilke',
                                              'Tarm: Fekal inkontinens' = 'TarmInkontinens',
                                              'Tarm: Kirurgisk inngrep' = 'TarmKirInngrep',
                                              'Tarm: Kirurgiske inngrep, hvilke' = 'TarmKirInngrepHvilke'
                                  )
                            ),
                            dateRangeInput(inputId = 'datovalg', start = "2017-07-01", end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            selectInput(inputId = "erMann", label="Kjønn",
                                        choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                            ),
                            sliderInput(inputId="alder", label = "Alder", min = 0,
                                        max = 110, value = c(0, 110)
                            ),
                            selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                                        choices = c("Egen mot resten av landet"=1, 
                                                    "Hele landet"=0, 
                                                    "Egen enhet"=2)
                            ),

                            selectInput(inputId = 'AIS', label='AIS-grad',
                                        multiple = T, #selected=0,
                                        choices = c("Alle"=0,
                                                    "A"=1, 
                                                    "B"=2,
                                                    "C"=3,
                                                    "D"=4,
                                                    "E"=5
                                                    )
                            ),
                            selectInput(inputId = 'traume', label='Traume',
                                        choices = c("Alle"=' ', #'ikke'
                                                    "Traume"='ja', 
                                                    "Ikke traume"='nei')
                            ),
                            selectInput(inputId = 'paratetra', label='Nivå ved utreise',
                                        choices = c("Alle" = 99,
                                                    "Paraplegi" = 0, 
                                                    "Tetraplegi" = 1,
                                                    "Ukjent" = 9)
                            )
                            #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
                            #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
               ),
               mainPanel(
                     tabsetPanel(
                           tabPanel(
                                 'Figur',
                                 plotOutput('fordelinger')),
                           tabPanel(
                                 'Tabell',
                                 uiOutput("tittelFord"),
                                 br(),
                                 tableOutput('fordelingTab'))
                     )
               )
      ), #tab Fordelinger

#------------Sykehusvise resultater------------
      tabPanel("Sykehusvise resultater",
               sidebarPanel(width = 3,
                            selectInput(
                                  inputId = "valgtVarGjsnGrVar", label="Velg variabel",
                                  choices = c('Alder' = 'Alder', 
                                              'Lengde på rehab.opphold' = 'DagerRehab', 
                                              'Tid fra skade til oppstart rehab.' = 'DagerTilRehab', 
                                              'Opphold, totalt antall dager' = 'OpphTot', 
                                              'Registreringsforsinkelse' = 'RegForsinkelse',
                                              'Livskval.: Tilfredshet med livet' = 'LivsGen',
                                              'Livskval.: Tilfredshet med fysisk helse' = 'LivsFys',
                                              'Livskval.: Tilfredshet med psykisk helse' = 'LivsPsyk'
                                             )
                            ),
                            dateRangeInput(inputId = 'datovalgGjsnGrVar', start = "2017-07-01", end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            selectInput(inputId = "erMannGjsnGrVar", label="Kjønn",
                                        choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                            ),
                            sliderInput(inputId="alderGjsnGrVar", label = "Alder", min = 0,
                                        max = 110, value = c(0, 110)
                            ),
                            selectInput(inputId = 'AISGjsnGrVar', label='AIS-grad',
                                        multiple = T, #selected=0,
                                        choices = c("Alle"=0,
                                                    "A"=1, 
                                                    "B"=2,
                                                    "C"=3,
                                                    "D"=4,
                                                    "E"=5
                                        )
                            ),
                            selectInput(inputId = 'traumeGjsnGrVar', label='Traume',
                                        choices = c("Alle"=' ', #'ikke'
                                                    "Traume"='ja', 
                                                    "Ikke traume"='nei')
                            ),
                            selectInput(inputId = 'paratetraGjsnGrVar', label='Nivå ved utreise',
                                        choices = c("Alle" = 99,
                                                    "Paraplegi" = 0, 
                                                    "Tetraplegi" = 1,
                                                    "Ukjent" = 9)
                            ),
                            selectInput(inputId = "sentralmaal", label="Velg gjennomsnitt/median ",
                                                               choices = c("Gjennomsnitt"='gjsn', "Median"='med'))
                            
               ),
               mainPanel(
                     tabsetPanel(
                           tabPanel(
                                 'Figur',
                                 plotOutput('gjsnGrVar')),
                           tabPanel(
                                 'Tabell',
                                 uiOutput("tittelGjsnGrVar"),
                                 br(),
                                 h5('Testtekst'),
                                 tableOutput('gjsnGrVarTab'))
                     )
               )
               
) #GjsnGrVar 

)

#----- Define server logic required to draw a histogram-------
server <- function(input, output) {

 #NB: Skal bare forholde oss til oppfølgingsskjema som er tilknyttet et gyldig Hovedskjema
      
      context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
      if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
                  
                  registryName <- "nordicscir"
                  dbType <- "mysql"
 #IKKE klar. Må plukke ut datasett koblet til Hovedskjema. Finn hvilke variable fra hovedskjema som trenger å være
#                  med for å gjøre alle filtreringer
                  
                  qLivs <- paste0('SELECT  UPPER(HovedskjemaGUID) AS HovedskjemaGUID, UPPER(SkjemaGUID) AS SkjemaGUID
                      FROM LifeQualityFormDataContract')
                  qKontr <- paste0('SELECT  UPPER(HovedskjemaGUID) AS HovedskjemaGUID, UPPER(SkjemaGUID) AS SkjemaGUID,
                        HealthUnitShortName, NoControl, CNum
                       FROM ControlFormDataContract')
                  qUrin <- paste0('SELECT  UPPER(HovedskjemaGUID) AS HovedskjemaGUID, UPPER(SkjemaGUID) AS SkjemaGUID 
                      FROM UrinaryTractFunctionFormDataContract')
                  qTarm <- paste0('SELECT  UPPER(HovedskjemaGUID) AS HovedskjemaGUID, UPPER(SkjemaGUID) AS SkjemaGUID 
                      FROM BowelFunctionFormDataContract')
                  qFunk <- paste0('SELECT  UPPER(HovedskjemaGUID) AS HovedskjemaGUID, UPPER(SkjemaGUID) AS SkjemaGUID 
                      FROM ActivityAndParticipationPerformanceFormDataContract')
                  qTilf <- paste0('SELECT  UPPER(HovedskjemaGUID) AS HovedskjemaGUID, UPPER(SkjemaGUID) AS SkjemaGUID
                      FROM ActivityAndParticipationSatisfactionFormDataContract')
                  
                  #RegData <- NSRegDataSQL(valgtVar = valgtVar) #datoFra = datoFra, datoTil = datoTil)
                  HovedSkjema <- NSRegDataSQL() #datoFra = datoFra, datoTil = datoTil)
                  Livskvalitet <- rapbase::LoadRegData(registryName, qLivs, dbType)
                  Kontroll <- rapbase::LoadRegData(registryName, qKontr, dbType)
                  Urin <- rapbase::LoadRegData(registryName, qUrin, dbType)
                  Tarm <- rapbase::LoadRegData(registryName, qTarm, dbType)
                  AktivFunksjon <- rapbase::LoadRegData(registryName, qFunk, dbType)
                  AktivTilfredshet <- rapbase::LoadRegData(registryName, qTilf, dbType)
                  } #hente data på server

      #Hente data og evt. parametre som er statistke i appen
      if (!exists('HovedSkjema')){
            #Tulledata:
            data('NordicScirFIKTIVEdata', package = 'nordicscir')
            # data('HovedSkjemaTull', package = 'nordicscir')
            # data('LivskvalitetTull', package = 'nordicscir')
            # data('KontrollTull', package = 'nordicscir')
            # data('UrinTull', package = 'nordicscir')
            # data('TarmTull', package = 'nordicscir')
            # data('AktivFunksjonTull', package = 'nordicscir')
            # data('AktivTilfredshetTull', package = 'nordicscir')
            
            
            #Laste ekte data lokalt
            # dato <- 'FormDataContract2018-11-01' #2017-05-24
            # sti <- 'A:/NordicScir/'
            # HovedSkjema <- read.table(paste0(sti, 'Main',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            # Livskvalitet <- read.table(paste0(sti, 'LifeQuality',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            # Kontroll <- read.table(paste0(sti, 'Control', dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            # Urin <- read.table(paste0(sti, 'UrinaryTractFunction', dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            # Tarm <- read.table(paste0(sti, 'BowelFunction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            # AktivFunksjon <- read.table(paste0(sti, 'ActivityAndParticipationPerformance',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            # AktivTilfredshet <- read.table(paste0(sti, 'ActivityAndParticipationSatisfaction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
      }  
            Livskvalitet$HovedskjemaGUID <- toupper(Livskvalitet$HovedskjemaGUID)
            Kontroll$HovedskjemaGUID <- toupper(Kontroll$HovedskjemaGUID)
            Urin$HovedskjemaGUID <- toupper(Urin$HovedskjemaGUID)
            Tarm$HovedskjemaGUID <- toupper(Tarm$HovedskjemaGUID)
            AktivFunksjon$HovedskjemaGUID <- toupper(AktivFunksjon$HovedskjemaGUID)
            AktivTilfredshet$HovedskjemaGUID <- toupper(AktivTilfredshet$HovedskjemaGUID)
            
            
            # sum(Aktivitet$HovedskjemaGUID %in% HovedSkjema$SkjemaGUID)
            # sum(AktivFunksjon$HovedskjemaGUID %in% HovedSkjema$SkjemaGUID)
            # sum(AktivTilfredshet$HovedskjemaGUID %in% HovedSkjema$SkjemaGUID)
            # sum(AktivTilfredshet$HovedskjemaGUID %in% AktivFunksjon$SkjemaGUID)
      #}
      
      HovedSkjema <- NSPreprosesser(HovedSkjema)
      LivskvalitetH <- KobleMedHoved(HovedSkjema,Livskvalitet)
      KontrollH <- KobleMedHoved(HovedSkjema,Kontroll)
      UrinH <- KobleMedHoved(HovedSkjema,Urin)
      TarmH <- KobleMedHoved(HovedSkjema,Tarm)
      Aktivitet <- KobleMedHoved(AktivFunksjon, AktivTilfredshet) #[,-which(names(AktivFunksjon)=='HovedskjemaGUID')]
      AktivitetH <- KobleMedHoved(HovedSkjema, Aktivitet)
      FunksjonH <- KobleMedHoved(HovedSkjema, AktivFunksjon)
      TilfredsH <- AktivitetH

      #RegData <- KobleMedHoved(HovedSkjema,Livskvalitet, alleHovedskjema = T) #SKAL iKKE BRUKES
      AlleTab <- list(HovedSkjema=HovedSkjema, 
                      LivskvalitetH=LivskvalitetH, 
                      KontrollH=KontrollH, 
                      UrinH=UrinH, 
                      TarmH=TarmH, 
                      FunksjonH=FunksjonH, 
                      TilfredsH = TilfredsH, 
                      AktivitetH = AktivitetH)
      reshID <- 107627
      
      
      
            output$mndRapp = downloadHandler(
                  filename = 'NSmndRapp.pdf',
                  content = function(file) {
                        src <- normalizePath('NSmndRapp.pdf')
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        file.copy(src, 'NSmndRapp.pdf', overwrite = TRUE)
                        out = knit2pdf(system.file('NSmndRapp.Rnw', package = 'nordicscir'), clean = TRUE)
                        file.rename(out, file) # move pdf to file for downloading
                  },

                  contentType = 'application/pdf'
            )
      
            
            output$tabBelegg <- renderTable(
            tabBelegg(RegData = HovedSkjema, datoTil=input$sluttDatoReg, tidsenhet=input$tidsenhetReg, 
                      enhetsUtvalg=as.numeric(input$enhetsNivaa), reshID=reshID)
            , rownames = T, digits=0, spacing="xs"
            )
            # output$tittelFord <- renderUI({
            #       tagList(
            #             h3(UtDataFord$tittel),
            #             h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
            #       )}) #, align='center'
            
            output$undertittelBelegg <- renderUI({
                  t1 <- 'Tabellene viser innleggelser '
                  h4(HTML(undertittel <- switch(input$tidsenhetReg,
                         Mnd = paste0(t1, 'siste 12 måneder før ', input$sluttDatoReg, '<br />'),
                         Aar = paste0(t1, 'siste 5 år før ', input$sluttDatoReg, '<br />'))
                  ))})
      output$tabAntOpphShMnd12 <- renderTable({
            switch(input$tidsenhetReg,
                   Mnd=tabAntOpphShMnd(RegData=HovedSkjema, datoTil=input$sluttDatoReg, antMnd=12), #input$datovalgTab[2])  
                   Aar=tabAntOpphSh5Aar(RegData=HovedSkjema, datoTil=input$sluttDatoReg))
      }, rownames = T, digits=0, spacing="xs" 
      )
     
      
      observe({
            #Antall skjema av alle typer.
            tabTilknSkjema <- tabSkjemaTilknyttetH(Data=AlleTab, datoFra=input$datovalgReg[1], datoTil=input$datovalgReg[2])
            
            output$tabAntTilknyttedeSkjema <- renderTable(
                  tabTilknSkjema$Antall
                  ,rownames = T, digits=0, spacing="xs" )
            
            #Andel (prosent) av registreringsskjemaene som har oppfølgingsskjema.      
            output$tabAndelTilknyttedeSkjema <- renderTable(
                  tabTilknSkjema$Andeler
                  ,rownames = T, digits=0, spacing="xs" )
      })
      
      #Antall skjema av hver type
      output$AntallSkjema <- renderTable(
            #tabAntSkjema(Data=AlleTab, datoFra='2015-01-01', datoTil='2018-11-01'),
            t(tabAntSkjema(Data=AlleTab, datoFra=input$datovalgReg[1], datoTil=input$datovalgReg[2]))
            ,rownames = T, digits=0, spacing="xs" )
            
      observe({   #Fordelingsfigurer og tabeller
            #RegData <- finnRegData(Data = AlleTab, valgtVar <- 'UrinKirInngr')
            RegData <- finnRegData(valgtVar = input$valgtVar, Data = AlleTab)
            RegData <- TilLogiskeVar(RegData)
            output$fordelinger <- renderPlot({
                  NSFigAndeler(RegData=RegData, valgtVar=input$valgtVar, preprosess = 0,
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                               reshID = reshID, 
                               AIS=input$AIS, traume=input$traume, paratetra=as.numeric(input$paratetra),
                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                               erMann=as.numeric(input$erMann), 
                               enhetsUtvalg=as.numeric(input$enhetsUtvalg))
            }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
            )
            
            #RegData må hentes ut fra valgtVar
            UtDataFord <- NSFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                                       reshID = reshID, 
                                       AIS=input$AIS, traume=input$traume, paratetra=as.numeric(input$paratetra),
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                                       erMann=as.numeric(input$erMann), 
                                       enhetsUtvalg=as.numeric(input$enhetsUtvalg))
            tabFord <- lagTabavFigAndeler(UtDataFraFig = UtDataFord)
            output$tittelFord <- renderUI({
                  tagList(
                        h3(UtDataFord$tittel),
                        h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
                  )}) #, align='center'
            output$fordelingTab <- renderTable(
                  tabFord, rownames = T)
      })
      
      observe({ #Sykehusvise gjennomsnitt, figur og tabell
            RegData <- finnRegData(valgtVar = input$valgtVarGjsnGrVar, Data = AlleTab)
            output$gjsnGrVar <- renderPlot(
                  NSFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsnGrVar,
                                 datoFra=input$datovalgGjsnGrVar[1], datoTil=input$datovalgGjsnGrVar[2], 
                                 AIS=input$AISGjsnGrVar, traume=input$traumeGjsnGrVar, paratetra=as.numeric(input$paratetraGjsnGrVar),
                                 minald=as.numeric(input$alderGjsnGrVar[1]), maxald=as.numeric(input$alderGjsnGrVar[2]), 
                                 erMann=as.numeric(input$erMannGjsnGrVar), valgtMaal = input$sentralmaal
                  ),
                  width = 800, height = 600)
            UtDataGjsnGrVar <- NSFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsnGrVar,
                                              datoFra=input$datovalgGjsnGrVar[1], datoTil=input$datovalgGjsnGrVar[2], 
                                              AIS=input$AISGjsnGrVar, traume=input$traumeGjsnGrVar, paratetra=as.numeric(input$paratetraGjsnGrVar),
                                              minald=as.numeric(input$alderGjsnGrVar[1]), maxald=as.numeric(input$alderGjsnGrVar[2]), 
                                              erMann=as.numeric(input$erMannGjsnGrVar), valgtMaal = input$sentralmaal)
            tabGjsnGrVar <- lagTabavFigGjsnGrVar(UtDataFraFig = UtDataGjsnGrVar)
            #tabGjsnGrVar <- lagTabavFig(UtDataFraFig = Data)
            output$tittelGjsnGrVar <- renderUI({
                  tagList(
                        h3(UtDataGjsnGrVar$tittel),
                        h5(HTML(paste0(UtDataGjsnGrVar$utvalgTxt, '<br />')))
                  )}) #, align='center'
            output$gjsnGrVarTab <- renderTable(
                  tabGjsnGrVar, rownames = T)
            
      }) #observe gjsnGrVar
} #server
# Run the application 
shinyApp(ui = ui, server = server)


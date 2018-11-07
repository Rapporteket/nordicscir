#Resultattjeneste for NordicScir
library(nordicscir)
library(shiny)
library(knitr)
library(lubridate)
#ibrary(shinyBS) # Additional Bootstrap Controls


ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
      title = 'NORSK SPINNALSKADEREGISTER',
      tabPanel("Viktigste resultater/Oversiktsside",
               #fluidRow(
               #column(width=5,
               h2("Dæsjbord?  - evt. vise viktigstre variable/resultater", align='center' ),
               h2("Gi tilbakemelding på hva som skal være på sida", align='center' ),
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
                       tags$li("Kun en figur på hver side, eller fint å vise to samtidig som under 'Andeler'? "),
                       tags$li("Hvilke utvalgs/filtreringsmuligheter skal vi ha i de ulike fanene"), 
                       tags$li("Innhold i tabeller som vises i tilknytning til figurer.")
               ),
               br(),
               tags$ul(tags$b('Kommer: '),
                       tags$li("Tabeller, registreringsoversikter"),
                        tags$li("Hjelpefunksjoner; belegg, SorterOgNavngiTidsEnhet. Flere?"),
                        tags$li("Tabeller fra figurene"),
                        tags$li("Laste ned tabeller"),
                        tags$li("Tulledata"),
                        tags$li("Gj.sn. tid?"),
                       tags$li("Alle figurer vil få tilhørende tabell i fane ved siden av som for 'Andeler' ")
               )
      ), #tab
      
      #-----Registreringsoversikter------------
      tabPanel("Registreringsoversikter",
               sidebarPanel(width=3,
                            h3('Utvalg')
               ),
                            
      
               mainPanel(
                     tabsetPanel(id='ark',
                                 tabPanel('Ant. registrerte ryggmargsskader',
                                          h3(""),
                                          p(em("Velg tidsperiode ved å velge sluttdato i menyen til venstre")),
                                          tableOutput("tabBelegg"),
                                          tableOutput("antReg")
                                 ),
                                 tabPanel('Ant. registrerte skjema',
                                          h3("Antall gyldige registreringsskjema per sykehus (+totalt), 
                                             og andel av registreringsskjemaene som har hatt kontroll og/eller oppfølging"),
                                          p(em("Velg tidsperiode ved å velge sluttdato i menyen til venstre"))
                                          #tableOutput("tabAntOpphShMnd12")
                                 ),
                                 tabPanel('Ant. unike pasienter?')
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
                                              'Livskval.: Fornøydhet med livet' = 'LivsGen',
                                              'Livskval.: Fornøydhet med fysisk helse' = 'LivsFys',
                                              'Livskval.: Fornøydhet med psykisk helse' = 'LivsPsyk',
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
                                        choices = c("Alle"='', #'ikke'
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
                                              'Livskval.: Fornøydhet med livet' = 'LivsGen',
                                              'Livskval.: Fornøydhet med fysisk helse' = 'LivsFys',
                                              'Livskval.: Fornøydhet med psykisk helse' = 'LivsPsyk'
                                             )
                            )
               ),
               mainPanel(
                     tabsetPanel(
                           tabPanel(
                                 'Figur',
                                 plotOutput('gjsnGrVar')),
                           tabPanel(
                                 'Tabell',
                                 uiOutput("tittelgjsnGrVar"),
                                 tableOutput('gjsnGrVarTab'))
                     )
               )
               
) #GjsnGrVar 

)

#----- Define server logic required to draw a histogram-------
server <- function(input, output) {

      
      context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
      if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
                  
                  registryName <- "nordicscir"
                  dbType <- "mysql"
                  
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
            
            dato <- 'FormDataContract2018-11-01' #2017-05-24
            sti <- 'A:/NordicScir/'
            HovedSkjema <- read.table(paste0(sti, 'Main',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            Livskvalitet <- read.table(paste0(sti, 'LifeQuality',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            Kontroll <- read.table(paste0(sti, 'Control', dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            Urin <- read.table(paste0(sti, 'UrinaryTractFunction', dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            Tarm <- read.table(paste0(sti, 'BowelFunction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            AktivFunksjon <- read.table(paste0(sti, 'ActivityAndParticipationPerformance',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            AktivTilfredshet <- read.table(paste0(sti, 'ActivityAndParticipationSatisfaction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            
            Livskvalitet$HovedskjemaGUID <- toupper(Livskvalitet$HovedskjemaGUID)
            Kontroll$HovedskjemaGUID <- toupper(Kontroll$HovedskjemaGUID)
            Urin$HovedskjemaGUID <- toupper(Urin$HovedskjemaGUID)
            Tarm$HovedskjemaGUID <- toupper(Tarm$HovedskjemaGUID)
            AktivFunksjon$HovedskjemaGUID <- toupper(AktivFunksjon$HovedskjemaGUID)
            AktivTilfredshet$HovedskjemaGUID <- toupper(AktivTilfredshet$HovedskjemaGUID)
            
            
            KobleMedHoved <- function(HovedSkjema,Skjema2,alleHovedskjema=T) {
                  varBegge <- intersect(names(Skjema2),names(HovedSkjema)) ##Variabelnavn som finnes i begge datasett
                  Skjema2 <- Skjema2[ ,c("HovedskjemaGUID", names(Skjema2)[!(names(Skjema2) %in% varBegge)])]  #"SkjemaGUID",
                  NSdata <- merge(HovedSkjema, Skjema2, suffixes = c('','XX'),
                                  by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = alleHovedskjema, all.y=F)
                  return(NSdata)
            }
            LivskvalitetHoved <- KobleMedHoved(HovedSkjema,Livskvalitet)
            KontrollHoved <- KobleMedHoved(HovedSkjema,Kontroll)
            UrinHoved <- KobleMedHoved(HovedSkjema,Urin)
            TarmHoved <- KobleMedHoved(HovedSkjema,Tarm)
            Aktivitet <- KobleMedHoved(AktivFunksjon,AktivTilfredshet)
            AktivitetHoved <- KobleMedHoved(HovedSkjema,Aktivitet)
            
            
      }
      
      HovedSkjema <- NSPreprosesser(HovedSkjema)
      RegData <- KobleMe
      AlleTab <- list(HovedSkjema, Livskvalitet, Kontroll, Urin, Tarm, AktivFunksjon, AktivTilfredshet)
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
      
      output$tabBelegg <- renderTable({
            tabBelegg(RegData = HovedSkjema, datoTil=Sys.Date(), tidsenhet='Mnd', enhetsUtvalg=0, reshID=reshID)
            })
            
      output$antReg <- renderTable({
            tabAntOpphShMnd(RegData = HovedSkjema, datoTil=Sys.Date(), antMnd=12)
      })
      
      observe({   
            if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
                  RegData <- NSRegDataSQL(valgtVar = input$valgtVar)
                  print(dim(RegData)[1])
            } #hente data på server
            
            output$fordelinger <- renderPlot({
                  NSFigAndeler(RegData=RegData, valgtVar=input$valgtVar, preprosess = 0,
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                               reshID = reshID, 
                               AIS=input$AIS, traume=as.numeric(input$traume), paratetra=as.numeric(input$paratetra),
                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                               erMann=as.numeric(input$erMann), 
                               enhetsUtvalg=as.numeric(input$enhetsUtvalg))
            }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
            )
            
            UtDataFord <- NSFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                                       reshID = reshID, 
                                       AIS=input$AIS, traume=as.numeric(input$traume), paratetra=as.numeric(input$paratetra),
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                                       erMann=as.numeric(input$erMann), 
                                       enhetsUtvalg=as.numeric(input$enhetsUtvalg))
            #SJEKK !!!!!!!!!!:
            #tab <- lagTabavFig(UtDataFraFig = UtDataFord)
            
            output$tittelFord <- renderUI({
                  tagList(
                        h3(UtDataFord$tittel),
                        h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
                  )}) #, align='center'
            output$fordelingTab <- renderTable(
                  tab, rownames = T)
      })
      
      output$gjsnGrVar <- renderPlot(
            NSFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsnGrVar
                           # datoFra=input$datovalgGjsnGrVar[1], datoTil=input$datovalgGjsnGrVar[2], 
                         # AIS=input$AISGjsnGrVar, traume=as.numeric(input$traumeGjsnGrVar), paratetra=as.numeric(input$paratetraGjsnGrVar),
                         # minald=as.numeric(input$alderGjsnGrVar[1]), maxald=as.numeric(input$alderGjsnGrVar[2]), 
                         # erMann=as.numeric(input$erMannGjsnGrVar)
                         ),
            width = 800, height = 600)
      } #server
# Run the application 
shinyApp(ui = ui, server = server)


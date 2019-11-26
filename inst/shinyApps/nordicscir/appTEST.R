#Resultattjeneste for NordicScir
library(nordicscir)
library(shiny)
#library(shinyjs)
library(knitr)
library(lubridate)
library(dplyr)
#ibrary(shinyBS) # Additional Bootstrap Controls
library(kableExtra)
#library(zoo)


startDatoStandard <- paste0(as.numeric(format(Sys.Date()-400, "%Y")), '-01-01') #Sys.Date()-400

# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- context %in% c("DEV", "TEST", "QA", "PRODUCTION") #rapbase::isRapContext()


regTitle = ifelse(paaServer,'Norsk ryggmargsskaderegister',
                  'Norsk ryggmargsskaderegister med FIKTIVE data')

#----Valg

valgAIS <- 0:5
names(valgAIS) <- c("Alle","A","B","C","D","E")
#valgAIS <- as.character(0:5),
#names(valgAIS) <- c('Alle', LETTERS[1:5]),

valgNivaaUt <- c(99,0,1,2,3,9)
names(valgNivaaUt) <- c("Alle", "Paraplegi", "Tetraplegi", "C1-4", "C5-8", "Ukjent")

ui <- tagList(
   shinyjs::useShinyjs(),
   navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
      title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"), regTitle),
      windowTitle = regTitle,
           
      #--------Fordelinger-----------            
      tabPanel("Fordelinger",
               sidebarPanel(width = 3,
                            selectInput(
                                  inputId = "valgtVar", label="Velg variabel",
                                  choices = c('Alder' = 'Alder', 
                                              'Ais ved innleggelse' = 'AAis' ,
                                              'Ais ved utskriving' = 'FAis', 
                                              'Skadeårsak ' = 'SkadeArsak',
                                              'Skadeårsak, ikke-traumatisk' = 'Ntsci',
                                              #'Fjern? Pustehjelp' = 'Pustehjelp[VentAssi]',
                                              'A&D Funksjon: Mobilitet' = 'FunkMob',
                                              'Livskval.: Tilfredshet med fysisk helse' = 'LivsFys',
                                              'Livskval.: Tilfredshet med psykisk helse' = 'LivsPsyk',
                                             'Urin: Kirurgiske inngrep' = 'UrinKirInngr',
                                              'Urin: Legemiddelbruk (fra 2019)' = 'UrinLegemidler',
                                               'Urin: Legemiddelbruk, hvilke' = 'UrinLegemidlerHvilke',
                                              'Urin: Blæretømming, hovedmetode' = 'UrinTomBlareHoved',
                                              'Urin: Blæretømming, tilleggsmetode' = 'UrinTomBlareTillegg',
                                              'Tarm: Avføring, hovedmetode' = 'TarmAvfHoved',
                                             'Tarm: Kirurgiske inngrep, hvilke' = 'TarmKirInngrepHvilke'
                                  )
                            ),
                            dateRangeInput(inputId = 'datovalg', start = startDatoStandard, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            radioButtons(inputId = 'datoUt', 'Bruk utskrivingsdato til datofiltrering?',
                                         choiceNames = c('nei','ja'), choiceValues = 0:1, selected = 0),
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
                            selectInput(inputId = 'AIS', label='AIS-grad ved utreise',
                                        multiple = T, #selected=0,
                                        choices = valgAIS
                             ),
                            selectInput(inputId = 'traume', label='Traume',
                                        choices = c("Alle"=' ', #'ikke'
                                                    "Traume"='ja', 
                                                    "Ikke traume"='nei')
                            ),
                            selectInput(inputId = 'nivaaUt', label='Nivå ved utreise',
                                        choices = valgNivaaUt
                            )
                            #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
                            #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
               ),
               
               mainPanel(width = 6,
                         tabsetPanel( id='fordeling',
                                      tabPanel(
                                         'Figur',
                                         br(),
                                         em('(Høyreklikk på figuren for å laste den ned)'),
                                         br(),
                                         br(),
                                         plotOutput('fordelinger'),
                                         hr()
                                      ) ,
                                      tabPanel(
                                         'Figur, alle sykehus',
                                         plotOutput('fordelingPrSh')),
                                      tabPanel(
                                         'Tabell',
                                         uiOutput("tittelFord"),
                                         br(),
                                         tableOutput('fordelingTab'),
                                         downloadButton(outputId = 'lastNed_tabFord', label='Last ned'),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         #hr(),
                                         tableOutput('fordelingShTab'),
                                         downloadButton(outputId = 'lastNed_tabFordSh', label='Last ned')
                                      )
                         )) #mainPanel
      ) #tab Fordelinger
   
         ) #navbar-page
) #tagList




#----- Define server logic required to draw a histogram-------
server <- function(input, output, session) {
      reshID <- reactive({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 107627)}) 
      rolle <- reactive({ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')})
      
      
      observe({if (rolle() != 'SC') {
            hideTab(inputId = "fordeling", target = "Figur, alle sykehus")
         hideTab(inputId = "fordeling", target = "lastNed_tabFordSh")
         hideTab(inputId = "fordeling", target = "fordelingShTab")
         hideTab(inputId = 'Registeradministrasjon', target ='Registeradministrasjon')
           
      }
      })
      
 #NB: Skal bare forholde oss til oppfølgingsskjema som er tilknyttet et gyldig Hovedskjema
      #paaServer <- (context == "TEST" | context == "QA" | context == "PRODUCTION")
      if (paaServer) {
                  HovedSkjema <- NSRegDataSQL() #datoFra = datoFra, datoTil = datoTil)
                  LivskvalH <- NSRegDataSQL(valgtVar='LivsXX')
                  KontrollH <- NSRegDataSQL(valgtVar='KontXX')
                  UrinH <- NSRegDataSQL(valgtVar='UrinXX')
                  TarmH <- NSRegDataSQL(valgtVar='TarmXX')
                  AktivFunksjonH <- NSRegDataSQL(valgtVar='FunkXX')
                  AktivTilfredshetH <- NSRegDataSQL(valgtVar='TilfXX')
                  
                  # widget
                  output$appUserName <- renderText(rapbase::getUserFullName(session))
                  output$appOrgName <- renderText(rapbase::getUserReshId(session))
      } 

      if (!exists('HovedSkjema')){
            #Tulledata:
            data('NordicScirFIKTIVEdata', package = 'nordicscir') #NB: Ikke koblede data

            Livskval$HovedskjemaGUID <- toupper(Livskval$HovedskjemaGUID)
            Kontroll$HovedskjemaGUID <- toupper(Kontroll$HovedskjemaGUID)
            Urin$HovedskjemaGUID <- toupper(Urin$HovedskjemaGUID)
            Tarm$HovedskjemaGUID <- toupper(Tarm$HovedskjemaGUID)
            AktivFunksjon$HovedskjemaGUID <- toupper(AktivFunksjon$HovedskjemaGUID)
            AktivTilfredshet$HovedskjemaGUID <- toupper(AktivTilfredshet$HovedskjemaGUID)
           
            #HovedSkjema <- NSPreprosesser(HovedSkjema) 
            LivskvalH <- KobleMedHoved(HovedSkjema,Livskval)
            KontrollH <- KobleMedHoved(HovedSkjema,Kontroll)
            UrinH <- KobleMedHoved(HovedSkjema,Urin)
            TarmH <- KobleMedHoved(HovedSkjema,Tarm)
            AktivFunksjonH <- KobleMedHoved(HovedSkjema, AktivFunksjon)
            Aktivitet <- KobleMedHoved(HovedSkjema = AktivFunksjon, Skjema2 = AktivTilfredshet) #[,-which(names(AktivFunksjon)=='HovedskjemaGUID')]
            AktivTilfredshetH <- KobleMedHoved(HovedSkjema, Aktivitet)
      }
      HovedSkjema <- NSPreprosesser(HovedSkjema)
      LivskvalH <- NSPreprosesser(LivskvalH)
      KontrollH <- NSPreprosesser(KontrollH)
      UrinH <- NSPreprosesser(UrinH)
      TarmH <- NSPreprosesser(TarmH)
      AktivFunksjonH <- NSPreprosesser(AktivFunksjonH)
      AktivTilfredshetH <- NSPreprosesser(AktivTilfredshetH)
      
      AlleTab <- list(HovedSkjema=HovedSkjema, 
                      LivskvalH=LivskvalH, 
                      KontrollH=KontrollH, 
                      UrinH=UrinH, 
                      TarmH=TarmH, 
                      AktivFunksjonH = AktivFunksjonH, 
                      AktivTilfredshetH = AktivTilfredshetH)
      
     
#---------Fordelinger------------
            observe({   #Fordelingsfigurer og tabeller
            #RegData <- finnRegData(Data = AlleTab, valgtVar <-'UrinLegemidlerHvilke')
            RegData <- finnRegData(valgtVar = input$valgtVar, Data = AlleTab)
            RegData <- TilLogiskeVar(RegData)
            #print(input$datoUt)
            #NSFigAndeler(RegData=RegData, valgtVar='UrinLegemidlerHvilke', preprosess = 0)
            
            output$fordelinger <- renderPlot({
                  NSFigAndeler(RegData=RegData, valgtVar=input$valgtVar, preprosess = 0,
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                               reshID = reshID(), 
                               AIS=input$AIS, traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                               erMann=as.numeric(input$erMann), 
                               enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                               datoUt=as.numeric(input$datoUt))
            }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
            )
            
            UtDataFord <- NSFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                                       reshID = reshID(), 
                                       AIS=input$AIS, traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                                       erMann=as.numeric(input$erMann), 
                                       enhetsUtvalg=as.numeric(input$enhetsUtvalg))
            
            output$tittelFord <- renderUI({
               tagList(
                  h3(UtDataFord$tittel),
                  h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
               )}) 
            
            tabFord <- lagTabavFigAndeler(UtDataFraFig = UtDataFord)
            
            output$fordelingTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
               antKol <- ncol(tabFord)
               kableExtra::kable(tabFord, format = 'html'
                                 , full_width=F
                                 , digits = c(0,1,0,1)[1:antKol]
               ) %>%
                  add_header_above(c(" "=1, 'Egen enhet/gruppe' = 2, 'Resten' = 2)[1:(antKol/2+1)]) %>%
                  column_spec(column = 1, width_min = '7em') %>%
                  column_spec(column = 2:(ncol(tabFord)+1), width = '7em') %>%
                  row_spec(0, bold = T)
            }
            output$lastNed_tabFord <- downloadHandler(
               filename = function(){paste0(input$valgtVar, '_fordeling.csv')},
               content = function(file, filename){write.csv2(tabFord, file, row.names = F, na = '')
               })
            
            
            output$fordelingPrSh <- renderPlot({
                  NSFigAndelerSh(RegData=RegData, valgtVar=input$valgtVar, preprosess = 0,
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                               AIS=input$AIS, traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                               erMann=as.numeric(input$erMann), 
                               datoUt=as.numeric(input$datoUt))
            }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
            )
            UtDataFordSh <- NSFigAndelerSh(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                                       AIS=input$AIS, traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                                       erMann=as.numeric(input$erMann), 
                                       datoUt=as.numeric(input$datoUt))
            
            #RegData <- NSRegDataSQL()
            #UtDataFordSh <- NSFigAndelerSh(RegData=RegData)
            tabFordSh <- lagTabavFigAndelerSh(UtDataFraFig = UtDataFordSh)
            
            output$fordelingShTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
               antKol <- ncol(tabFordSh)
               tab <- kableExtra::kable(tabFordSh, format = 'html'
                                 , full_width=F
                                 , digits = c(0,0,0,1,1,1)[1:antKol]
               ) %>%
                  add_header_above(tab, header= c(" "=1, 'Antall' = 3, 'Andel' = 3))  %>% #[1:(antKol/2+1)])
                  column_spec(column = 1, width_min = '7em') %>%
                  column_spec(column = 2:(ncol(tabFordSh)+1), width = '7em') %>%
                  row_spec(0, bold = T)
            }
            output$lastNed_tabFordSh <- downloadHandler(
               filename = function(){paste0(input$valgtVar, '_fordelingSh.csv')},
               content = function(file, filename){write.csv2(tabFordSh, file, row.names = F, na = '')
               })
            
      }) #observe Fordeling
      
} #server
# Run the application 
shinyApp(ui = ui, server = server)


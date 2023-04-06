library(shiny)
library(data.table)
library(googleVis)
library(DT)
library(plotly)

shinyUI(fluidPage(
  
  titlePanel("Projekt zaliczeniowy 2021/2022"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # 9. pobieranie danych z sieci 
      actionButton(inputId = "open_page_EUROSTAT", label = "Pobierz dane EUROSTAT"),  
      actionButton(inputId = "open_page_GUS", label = "Pobierz dane GUS"),
      actionButton("getDataFromServer", "Utworz baze RSQLite"),
      
      
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  
                  tabPanel("SQL",
                           textInput("sqlQueryInput",
                                     label = "Zapytanie SQL",
                                     value = "select * from EUROSTAT limit 100"    
                           ),
                           downloadButton("fileOutPath",
                                          label = "Eksport danych"
                           ),
                           
                           DT::dataTableOutput("tbl"),
                           plotlyOutput("firstmap")
                  ),
                  tabPanel("Mapa GUS",
                           
                           fluidRow(column(6,HTML(paste0("Wybierz przedzial czasowy do bezposredniej analizy")),
                                           
                                           textInput("date1bezp",
                                                     label = "Data poczatkowa",
                                                     value = "2020-01-01"    
                                           ),
                                           textInput("date2bezp",
                                                     label = "Data koncowa",
                                                     value = "2021-12-01"    
                                           ),
                                           htmlOutput("mapagusbezp")),
                                    column(6,
                                           HTML(paste0("Wybierz przedzial czasowy do analizy wzgl??dnej")),
                                           textInput("date1wzgl",
                                                     label = "Data poczatkowa",
                                                     value = "2015-01-01"    
                                           ),
                                           textInput("date2wzgl",
                                                     label = "Data koncowa",
                                                     value = "2019-12-31"    
                                           ),
                                           htmlOutput("mapaguswzg")))
                  ),
                  tabPanel("Mapa EU",
                           
                           fluidRow(column(6,HTML(paste0("Wybierz przedzial czasowy do bezposredniej analizy")),
                                           
                                           textInput("date1bezpeu",
                                                     label = "Data poczatkowa",
                                                     value = "2020-01-01"    
                                           ),
                                           textInput("date2bezpeu",
                                                     label = "Data koncowa",
                                                     value = "2021-12-01"    
                                           ),
                                           htmlOutput("mapaeubezp")),
                                    column(6,
                                           HTML(paste0("Wybierz przedzial czasowy do bezposredniej analizy")),
                                           textInput("date1wzgleu",
                                                     label = "Data poczatkowa",
                                                     value = "2015-01-01"    
                                           ),
                                           textInput("date2wzgleu",
                                                     label = "Data koncowa",
                                                     value = "2019-12-31"    
                                           ),
                                           htmlOutput("mapaeuwzg")),
                           )),
                  tabPanel("Szeregi czasowe GUS",
                           
                           fluidRow(column(5,HTML(paste0("Wybierz przedzial czasowy do bezposredniej analizy")),
                                           
                                           textInput("date1bezpsz",
                                                     label = "Data poczatkowa",
                                                     value = "2020-01-01"    
                                           ),
                                           textInput("date2bezpsz",
                                                     label = "Data koncowa",
                                                     value = "2021-12-01"    
                                           ),
                                           plotlyOutput("szeregigusbezp"),
                           ),
                           column(5,
                                  HTML(paste0("Wybierz przedzial czasowy do analizy wzgl??dnej")),
                                  textInput("date1wzglsz",
                                            label = "Data poczatkowa",
                                            value = "2015-01-01"    
                                  ),
                                  textInput("date2wzglsz",
                                            label = "Data koncowa",
                                            value = "2019-12-31"    
                                  ),
                                  plotlyOutput("szeregiguswzgl")),
                           column(2,
                                  selectInput("timegus", "Wymiar czasu:",
                                              c("Tydzie??" = "week",
                                                "Miesi??c" = "month",
                                                "Rok" = "year")),
                                  selectInput("Plec", "Plec:",
                                              c("Ogolem" = "Ogolem",
                                                "Kobiety" = "Kobiety",
                                                "Mezczyzni" = "Mezczyzni")),
                                  selectInput("Grupa_wiekowa", "Kategoria wiekowa:",
                                              c("0 - Inf",  "00 - 04",  "05 - 09",  "10 - 14",  "15 - 19",  "20 - 24",  "25 - 29",  "30 - 34",  "35 - 39",  "40 - 44", 
                                                "45 - 49",  "50 - 54",  "55 - 59",  "60 - 64",  "65 - 69",  "70 - 74",  "75 - 79",  "80 - 84",  "85 - 89",  "90 - Inf")),
                                  selectInput("Region", "Wojewodztwo:",
                                              c("Polska", "Dolno??l??skie","Kujawsko-Pomorskie","Lubelskie",           
                                                "Lubuskie","??odzkie","Ma??opolskie","Mazowieckie"="Makroregion Wojewodztwo Mazowieckie",         
                                                "Opolskie","Podkarpackie","Podlaskie",           
                                                "Pomorskie", "??l??skie", "??wi??tokrzyskie",
                                                "Warmi??sko-Mazurskie", "Wielkopolskie",  "Zachodniopomorskie"))
                           )
                           )),
                  tabPanel("Szeregi czasowe EU",
                           
                           fluidRow(column(5,HTML(paste0("Wybierz przedzial czasowy do bezposredniej analizy")),
                                           
                                           textInput("date1bezpszu",
                                                     label = "Data poczatkowa",
                                                     value = "2020-01-01"    
                                           ),
                                           textInput("date2bezpszu",
                                                     label = "Data koncowa",
                                                     value = "2021-12-01"    
                                           ),
                                           plotlyOutput("szeregieubezp")
                           ),
                           column(5,
                                  HTML(paste0("Wybierz przedzial czasowy do analizy wzgl??dnej")),
                                  textInput("date1wzglu",
                                            label = "Data poczatkowa",
                                            value = "2015-01-01"    
                                  ),
                                  textInput("date2wzglu",
                                            label = "Data koncowa",
                                            value = "2019-12-31"    
                                  ),
                                  plotlyOutput("szeregieeuwzgl")
                           ),
                           column(2,
                                  selectInput("timeu", "Wymiar czasu:",
                                              c("Tydzie??" = "weeku",
                                                "Miesi??c" = "monthu",
                                                "Rok" = "yearu")),
                                  
                                  selectInput("sex", "P??e??:",
                                              c("Ogolem" = "Total",
                                                "Kobiety" = "Females",
                                                "Mezczyzni" = "Males")),
                                  
                                  selectInput("GEO", "Kraj:",
                                              c( "Poland","Belgium", "Bulgaria", "Czechia", "Denmark",                                         
                                                 "Germany"="Germany (until 1990 former territory of the FRG)", "Estonia",
                                                 "Ireland","Greece","Spain","France","Croatia","Italy","Cyprus",  "Latvia",                                          
                                                 "Lithuania","Luxembourg","Hungary","Malta","Netherlands","Austria",                                        
                                                 "Portugal","Romania","Slovenia","Slovakia","Finland",                                         
                                                 "Sweden","Iceland","Liechtenstein","Norway","Switzerland","United Kingdom" ,                                
                                                 "Montenegro","Albania","Serbia","Andorra","Armenia","Georgia" ))
                           )
                           ))
                  
                  
      )
    )
  )
  
))
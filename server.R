library(shiny)
library(DBI)
library(RSQLite)
library(googleVis)
library(ggplot2)
library(data.table)
library(DT)
library(plotly)
library(reshape)
library(forecast)
library(reshape)
library(dplyr)
library(tidyverse)
library(tsibble)
library("readxl")

#--------------------------


shinyServer(function(input, output) {

  sqlVar <- reactiveValues(
    sqlText = NULL
  )

  observeEvent(input$sqlQueryInput,{
    sqlVar$sqlText <- input$sqlQueryInput
  })

  output$plainSQLText <- renderPrint({
    return(cat(paste(sqlVar$sqlText,"\n")))
  })
  
  
  observeEvent(input$open_page_GUS,{
    browseURL("https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/")
  })  
  
  observeEvent(input$open_page_EUROSTAT,{
    browseURL("https://ec.europa.eu/eurostat/web/population-demography/demography-population-stock-balance/database?node_code=demomwk")
  })  
  
  dataDir    <- file.path(getwd(),"data")
  endOfMonth <- function(X){
    X <- as.character(as.Date(paste0(substr(as.character(as.Date(paste0(substr(as.character(X),1,7),"-01")) + 31),1,7),"-01"))-1)
    return(X)
  }
  
  dataINeurostat <- reactive({
    try({
      unzip(file.path(dataDir,"demo_r_mwk_ts.zip"),exdir=file.path(dataDir),setTimes=T)
      d <- read.table(file="data/demo_r_mwk_ts_1_Data.csv",sep=",",dec=",",header=T,stringsAsFactors=F)
      d$Value <- as.integer(gsub(",|:","",d$Value))
      print("Data EUORSTAT cleaned")
      return(d)
    },silent=T)
    return(data.frame())
  }) 
 
  dataINgus <- reactive({
    try({
      unzip(file.path(dataDir,"zgony_wg_tygodni.zip"),exdir=file.path(dataDir),setTimes=T)
      hd <- getwd()
      setwd(file.path(dataDir,"zgony_wg_tygodni"))
      try({
        lapply(dir(),function(f){
          file.rename(
            from=f, 
            to = gsub(" ","_",gsub("\x88","l",f))
          )
        })
      })
      setwd(hd)
      czytajDaneLiczboweZZakladki <- function(f,sheet,plec){
        
        d <- as.data.frame(read_excel(f,sheet=sheet))
        colnames(d)[1:3] <- c("Grupa_wiekowa","Region_id","Region")
        d <- d[-c(1:(grep("^Og",d$Grupa_wiekowa)[1]-1)),]
        
        tygodnie <- 1:(ncol(d)-3)
        tygodnie[nchar(tygodnie)<2] <- paste0("0",tygodnie[nchar(tygodnie)<2])
        colnames(d)[4:ncol(d)] <- tygodnie
        
        d <- reshape::melt(d,id.vars=c("Grupa_wiekowa","Region_id","Region"))
        colnames(d) <- c("Grupa_wiekowa","Region_id","Region","Tydzien","Liczba")
        d$Grupa_wiekowa[grep("Og",d$Grupa_wiekowa)] <- "0 - Inf"
        d$Grupa_wiekowa[grep("wi",d$Grupa_wiekowa)] <- "90 - Inf"
        #d$Liczba[is.na(d$Liczba)] <- 0 
        d <- cbind("Plec"=plec,d)
        
        return(d)
        
      }
      #------------------------------------------------------------------------------
      
      # iterujemy po plikach; wczytujemy; obrabiamy  
      hd <- getwd()
      setwd(file.path(dataDir,"zgony_wg_tygodni"))
      
      try({
        mainRet <- do.call("rbind",lapply(dir(),function(f){
          print(f)
          
          ogolem <- czytajDaneLiczboweZZakladki(f,1,"Ogolem")
          mezczyzni <- czytajDaneLiczboweZZakladki(f,2,"Mezczyzni")
          kobiety <- czytajDaneLiczboweZZakladki(f,3,"Kobiety")
          print("Dane GUS wczytane")
          
          dane <- rbind(ogolem,mezczyzni,kobiety)
          
          # tygodnie 
          tygodnie <- as.data.frame(read_excel(f,sheet=grep("tyg",tolower(excel_sheets(f)))))
          tygodnie <- do.call("rbind",lapply(split(tygodnie,tygodnie[,2]),function(x){
            return(data.frame(Tydzien=unique(x[,2]),Od=min(x[,1]),Do=max(x[,1])))
          }))
          tygodnie$Tydzien <- gsub("T|W","",unlist(lapply(strsplit(tygodnie$Tydzien,"-"),function(x){x[2]})))
          rownames(tygodnie) <- NULL
          
          dane <- merge(x=dane,y=tygodnie,by="Tydzien",all=T)
          dane <- dane[,-which(colnames(dane)=="Tydzien")]
          
          dane <- dane[c("Od","Do","Plec","Grupa_wiekowa","Region_id","Region","Liczba")]
          dane$Liczba <- as.integer(dane$Liczba)
          
          dane$Grupa_wiekowa[dane$Grupa_wiekowa=="0 - 4"] <- "00 - 04"
          dane$Grupa_wiekowa[dane$Grupa_wiekowa=="5 - 9"] <- "05 - 09"
          #dane$Od <- as.Date(as.POSIXct(dane$Od/1000, origin="1970-01-01"))
          #dane$Do <- as.Date(as.POSIXct(dane$Do/1000, origin="1970-01-01"))

          return(dane)
        }))
        
        #write.table(mainRet,file="../GUS_dane_przetworzone_pelne.csv",sep=";",dec=",",row.names=F)
      })
      
      
      setwd(hd)
    },silent=T)
    print(mainRet$Od)
    return(as.data.frame(mainRet))
  })
  
  dbName <- file.path(dataDir,"database.db")
  con <- dbConnect(
    dbDriver("SQLite"),
    dbname = dbName)
  
  observeEvent(input$getDataFromServer,{
    dbWriteTable(con, "EUROSTAT", dataINeurostat(), overwrite = TRUE, row.names = FALSE)
    print("Table EUROSTAT made")
    #d <- read.table(file=file.path(dataDir,"GUS_dane_przetworzone_pelne.csv"),sep=";",dec=",",header=T)
    dbWriteTable(con, "GUS", dataINgus(), overwrite = TRUE, row.names = FALSE)
    print("Table GUS made")
    
    })
  
  datagus <- reactive({
    dataDir    <- file.path(getwd(),"data")
    
    dbName <- file.path(dataDir,"database.db")
    con <- dbConnect(
      dbDriver("SQLite"),
      dbname = dbName)
    data <- dbGetQuery(con, "select * from GUS")
    return(data.frame())})
  
  dataeurostat <- reactive({
    data <- dbGetQuery(con, "select * from EUROSTAT")
    return(data.frame())})
    

  zak2 <- reactive({
    data <- dbGetQuery(con, paste(sqlVar$sqlText))
    return(data)})
  output$tbl <-  DT::renderDataTable({DT::datatable(
                                  zak2(),
                                   rownames = FALSE,
                                   options = list(
                                       scrollX = TRUE,
                                       pageLength = 16,
                                       lengthMenu = seq(from=10,by=10,to=100)
                                     ))})
  output$firstmap <- renderPlotly({
    
    
    if (all(c("timestamp", "value", "variable") %in% colnames(zak2()))){
      jpeg("sqlplot.jpg")
      xxx <- zak2()
      xxx$timestamp <- as.Date(xxx$timestamp, "%Y-%m-%d")
      xxx <- na.omit(xxx)
      img <- ggplotly(ggplot(xxx, aes(x=timestamp,y=value , col=variable)) + geom_line())
      dev.off()
      silent=T
    }
    return(img)
  })

  output$fileOutPath <- downloadHandler(
    
    filename = function() { 
      return(paste0(gsub("-","_",as.character(Sys.Date())),"_out.csv")) 
    },
    content = function(file) {
      write.csv(
        xxx(),
        file)
    }
    
  )
  
  sqlQuery <- function(
    sqlQueryText="select * from sqlite_master",
    dataDir = file.path(getwd(),"data"),
    dbName = file.path(dataDir,"database.db")
  ){
    con <- dbConnect(
      dbDriver("SQLite"),
      dbname = dbName
    )
    ret <- data.frame()
    try({
      ret <- dbGetQuery(con, sqlQueryText)
    })
    dbDisconnect(con)
    return(ret)
  }
  
  output$mapagusbezp <-renderGvis({
    jpeg("gusbezpplot.jpg")
    dt <- sqlQuery(paste0("select Region, Liczba from GUS where Region_id in (\"PL9\",\"PL42\",\"PL41\",\"PL72\",\"PL22\",\"PL52\",\"PL21\",\"PL71\",\"PL43\",\"PL81\",\"PL61\",\"PL51\",\"PL82\",\"PL84\",\"PL63\",\"PL62\") and Od>='",input$date1bezp,"' and Do<='", input$date2bezp,"' group by Region"))
    dt <- data.frame(lapply(dt, function(x) {
                       gsub("Makroregion Województwo Mazowieckie", "Mazowieckie", x)
                   }))
   
    gvisGeoChart(dt, "Region", "Liczba",
                                           options=list(region="PL",
                                                        displayMode="regions",
                                                        resolution="provinces",
                                                        width=500, height=500, as.is=TRUE))
    dev.off()})
  output$mapaguswzg <-renderGvis({
    dt <- as.data.frame(sqlQuery(paste0("select Region, Liczba from GUS where Region_id in (\"PL9\",\"PL42\",\"PL41\",\"PL72\",\"PL22\",\"PL52\",\"PL21\",\"PL71\",\"PL43\",\"PL81\",\"PL61\",\"PL51\",\"PL82\",\"PL84\",\"PL63\",\"PL62\") and Od>='",input$date1bezp,"' and Do<='", input$date2bezp,"' group by Region")))
    dt <- data.frame(lapply(dt, function(x) {
      gsub("Makroregion WojewĂłdztwo Mazowieckie", "Mazowieckie", x)
    }))
    dtwzg <- as.data.frame(sqlQuery(paste0("select Region, Liczba from GUS where Region_id in (\"PL9\",\"PL42\",\"PL41\",\"PL72\",\"PL22\",\"PL52\",\"PL21\",\"PL71\",\"PL43\",\"PL81\",\"PL61\",\"PL51\",\"PL82\",\"PL84\",\"PL63\",\"PL62\") and Od>='",input$date1wzgl,"' and Do<='", input$date2wzgl,"' group by Region")))
    dtwzg <- data.frame(lapply(dtwzg, function(x) {
      gsub("Makroregion WojewĂłdztwo Mazowieckie", "Mazowieckie", x)
    }))
    
    f <- left_join(dt,dtwzg, by=c("Region"))
    f$Liczba.x <- as.numeric(f$Liczba.x)
    f$Liczba.y <- as.numeric(f$Liczba.y)
    f$diff <- f$Liczba.x/f$Liczba.y
    f <- na.omit(f)
    
    gvisGeoChart(f, "Region", "diff",
                 options=list(region="PL",
                              displayMode="regions",
                              resolution="provinces",
                              width=500, height=500, as.is=TRUE))})
  
  
  
  output$mapaeubezp <-renderGvis({
    d1 <- gsub(" ", "", yearweek(input$date1bezpeu))
    d2 <- gsub(" ", "", yearweek(input$date2bezpeu))
    eu <- sqlQuery(paste0("select Value, GEO from EUROSTAT where SEX='Total' and TIME>='",d1,"' and TIME<='",d2,"' group by GEO "))
    
    
    eu$GEO <-  c("AL", "AD", "AM", "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "GE", "DE", "GR", "HU", "IS", "IE", "IT", "LV", "LI", "LT", "LU", "MT", "ME", "NL", "NO", "PL", "PT", "RO", "RS", "SK", "SI", "ES", "SE", "CH", "GB")
    eu<- na.omit(eu)
    gvisGeoChart(eu, "GEO", "Value",
                options=list(region="150",width=500, height=500, as.is=TRUE))})
  
  output$mapaeuwzg <-renderGvis({
    d1 <- gsub(" ", "", yearweek(input$date1bezpeu))
    d2 <- gsub(" ", "", yearweek(input$date2bezpeu))
    dw1 <- gsub(" ", "", yearweek(input$date1wzgleu))
    dw2 <- gsub(" ", "", yearweek(input$date2wzgleu))
    eu1 <- sqlQuery(paste0("select Value, GEO from EUROSTAT where SEX='Total' and TIME>='",d1,"' and TIME<='",d2,"' group by GEO "))
    eu2 <- sqlQuery(paste0("select Value, GEO from EUROSTAT where SEX='Total' and TIME>='",dw1,"' and TIME<='",dw2,"' group by GEO "))
    
    eu <- left_join(eu1,eu2, by=c("GEO"))
    eu$Value.x <- as.numeric(eu$Value.x)
    eu$Value.y <- as.numeric(eu$Value.y)
    eu$diff <- eu$Value.x/eu$Value.y
    eu$GEO <-  c("AL", "AD", "AM", "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "GE", "DE", "GR", "HU", "IS", "IE", "IT", "LV", "LI", "LT", "LU", "MT", "ME", "NL", "NO", "PL", "PT", "RO", "RS", "SK", "SI", "ES", "SE", "CH", "GB")
    eu <- na.omit(eu)
    
    gvisGeoChart(eu, "GEO", "diff",
                 options=list(region="150",width=500, height=500, as.is=TRUE))})
  
  output$szeregigusbezp <- renderPlotly({
    
    ret <- sqlQuery(paste0("select Od, Do, Liczba from GUS where Region='",input$Region ,"' and Grupa_wiekowa='",input$Grupa_wiekowa,"' and Plec='",input$Plec,"' and Od>='", input$date1bezpsz,"' and Do<= '",input$date2bezpsz,"' order by Od"))
    ret <- na.omit(ret)
    
    if(input$timegus=="month"){
     
      ret$Od <- as.Date(ret$Od)
      ret$Do <- as.Date(ret$Do)
      ret <- na.omit(ret)
      
      dailyData <- do.call("rbind",lapply(split(ret,ret$Od),function(x){
        tmpSeq <- seq(from=x$Od,to=x$Do,by="1 day")
        return(data.frame(Data=tmpSeq,Liczba=x$Liczba/length(tmpSeq)))
      }))
      dailyData$RokMiesiac <- as.Date(endOfMonth(dailyData$Data))
      monthlyData <- aggregate(Liczba~RokMiesiac,data=dailyData,FUN=mean,na.rm=T)
      colnames(monthlyData)[1] <- "Data"
      monthlyData <- monthlyData[order(monthlyData$Data),]
      monthlyData <- monthlyData[-c(1,nrow(monthlyData)),]
      img_3 <- (
        ggplot(data=monthlyData,aes(x=Data,y=Liczba))
        + 
          geom_line()+ggtitle("Wizualizacja bezpośrednia")
      
     )
      
      
    } else if(input$timegus=="year"){
      ret$Od <- as.Date(ret$Od, format="%Y")
      ret <- na.omit(ret)
      
      ret <- aggregate(Liczba ~ Od, data = ret, FUN = mean, na.rm=T)
      plot <- ggplot(ret, aes(Od, Liczba)) + geom_line(size = 0.5)+ggtitle("Wizualizacja bezpośrednia")
      
    } else{
      
      ret$Od <- as.Date(ret$Od, format="%Y-%m-%d")
      #ret <- aggregate(Liczba ~ Od, data = ret, FUN = sum, na.rm=T)
      ret <- na.omit(ret)
      
      
      #df1 <- melt(ret, id.vars = "Od", measure.vars = "Liczba")
      plot <- ggplot(ret, aes(Od, Liczba)) + geom_line(size = 0.5)+ggtitle("Wizualizacja bezpośrednia")
      }
  })
  
  output$szeregiguswzgl <- renderPlotly({
    
    ret <- sqlQuery(paste0("select * from GUS where Region='",input$Region ,"' and Grupa_wiekowa='",input$Grupa_wiekowa,"' and Plec='",input$Plec,"' and Od>='", input$date1bezpsz,"' and Do<= '",input$date2bezpsz,"' order by Od"))
    wzg <- sqlQuery(paste0("select * from GUS where Region='",input$Region ,"' and Grupa_wiekowa='",input$Grupa_wiekowa,"' and Plec='",input$Plec,"' and Od>='", input$date1wzglsz,"' and Do<= '",input$date2wzglsz,"' order by Od"))
    
    ret <- na.omit(ret)
    wzg <- na.omit(wzg)
    if(input$timegus=="month"){
      
      ret$Od <- as.Date(ret$Od)
      ret$Do <- as.Date(ret$Do)
      wzg$Od <- as.Date(wzg$Od)
      wzg$Do <- as.Date(wzg$Do)

      dailyDataret <- do.call("rbind",lapply(split(ret,ret$Od),function(x){
        tmpSeq <- seq(from=x$Od,to=x$Do,by="1 day")
        return(data.frame(Data=tmpSeq,Liczba=x$Liczba/length(tmpSeq)))
      }))
      dailyDatawzg <- do.call("rbind",lapply(split(wzg,wzg$Od),function(x){
        tmpSeq <- seq(from=x$Od,to=x$Do,by="1 day")
        return(data.frame(Data=tmpSeq,Liczba=x$Liczba/length(tmpSeq)))
      }))
      dailyDataret$RokMiesiac <- as.Date(endOfMonth(dailyDataret$Data))
      dailyDatawzg$RokMiesiac <- as.Date(endOfMonth(dailyDatawzg$Data))
      
      monthlyDataret <- aggregate(Liczba~RokMiesiac,data=dailyDataret,FUN=mean,na.rm=T)
      monthlyDatawzg <- aggregate(Liczba~RokMiesiac,data=dailyDatawzg,FUN=mean,na.rm=T)
      
      avg <- mean(monthlyDatawzg$Liczba)
      monthlyDataret$diff <- monthlyDataret$Liczba/avg
      
      colnames(monthlyDataret)[1] <- "Data"
      monthlyDataret <- monthlyDataret[order(monthlyDataret$Data),]
      monthlyDataret <- monthlyDataret[-c(1,nrow(monthlyDataret)),]
     
      
      img_3 <- (
        ggplot(data=monthlyDataret,aes(x=Data,y=diff))
        + 
          geom_line()+ggtitle("Wizualizacja względna")
        
      )
      
      
    } else if(input$timegus=="year"){
      ret$Od <- as.Date(ret$Od, format="%Y")
      ret <- na.omit(ret)
      wzg <- aggregate(Liczba ~ Od, data = ret, FUN = mean, na.rm=T)
      ret <- aggregate(Liczba ~ Od, data = ret, FUN = mean, na.rm=T)
      avg <- mean(wzg$Liczba)
      ret$diff <- ret$Liczba/avg
      
      plot <- ggplot(ret, aes(Od, diff)) + geom_line(size = 0.5)+ggtitle("Wizualizacja względna")
      
    } else{
      
      ret$Od <- as.Date(ret$Od, format="%Y-%m-%d")
      ret <- aggregate(Liczba ~ Od, data = ret, FUN = mean, na.rm=T)
      ret <- na.omit(ret)
      wzg <- aggregate(Liczba ~ Od, data = ret, FUN = mean, na.rm=T)
      avg <- mean(wzg$Liczba)
      ret$diff <- ret$Liczba/avg
      #df1 <- melt(ret, id.vars = "Od", measure.vars = "Liczba")
      plot <- ggplot(ret, aes(Od, diff)) + geom_line(size = 0.5)+ggtitle("Wizualizacja względna")
    }
  })
  
  output$szeregieubezp <- renderPlotly({
    
    ret <- sqlQuery(paste0("select TIME, Value from EUROSTAT where GEO='",input$GEO ,"' and SEX='",input$sex,"' and TIME>='", gsub(" ", "", yearweek(input$date1bezpszu)),"' and TIME<= '",gsub(" ", "", yearweek(input$date2bezpszu)),"' order by TIME"))
    func1 <- function(x) {
      z<-paste0(strsplit(x,"W")[[1]][1], "-W", strsplit(x,"W")[[1]][2],"-1")
      z<- paste0(ISOweek::ISOweek2date(z))
      return(z)
    }
    func2 <- function(x) {
      z<-paste0(strsplit(x,"W")[[1]][1], "-W", strsplit(x,"W")[[1]][2],"-7")
      z<- paste0(ISOweek::ISOweek2date(z))
      return(z)
    }
    ret$Od <- lapply(ret$TIME, function(x) sapply(x, func1))
    ret$Do <- lapply(ret$TIME, function(x) sapply(x, func2))
    
    ret$Od <- as.Date(as.character(ret$Od), format="%Y-%m-%d")
    ret$Do <- as.Date(as.character(ret$Do), format="%Y-%m-%d")
    ret <- na.omit(ret)
    
    
    if(input$timeu=="monthu"){
      
      dailyData <- do.call("rbind",lapply(split(ret,ret$Od),function(x){
        tmpSeq <- seq(from=x$Od,to=x$Do,by="1 day")
        return(data.frame(Data=tmpSeq,Value=x$Value/length(tmpSeq)))
      }))
      dailyData$RokMiesiac <- as.Date(endOfMonth(dailyData$Data))
      monthlyData <- aggregate(Value~RokMiesiac,data=dailyData,FUN=mean,na.rm=T)
      colnames(monthlyData)[1] <- "Data"
      monthlyData <- monthlyData[order(monthlyData$Data),]
      monthlyData <- monthlyData[-c(1,nrow(monthlyData)),]
      img_3 <- (
        ggplot(data=monthlyData,aes(x=Data,y=Value))
        + 
          geom_line()+ggtitle("Wizualizacja bezpośrednia")
        
      )
      
      
    } else if(input$timeu=="yearu"){
      ret$Od <- format(as.Date(ret$Od, format="%Y-%m-%d"),"%Y")
      ret <- na.omit(ret)
      
      ret <- aggregate(Value ~ Od, data = ret, FUN = mean, na.rm=T)
      plot <- ggplot(ret, aes(Od, Value, group=1)) + geom_line(size = 0.5)+ggtitle("Wizualizacja bezpośrednia")
      
    } else{
      
      ret$Od <- as.Date(ret$Od, format="%Y-%m-%d")
      ret <- aggregate(Value ~ Od, data = ret, FUN = mean, na.rm=T)
      ret <- na.omit(ret)
      
      
      #df1 <- melt(ret, id.vars = "Od", measure.vars = "Liczba")
      plot <- ggplot(ret, aes(Od, Value)) + geom_line(size = 0.5)+ggtitle("Wizualizacja bezpośrednia")
    }
  })
  
  output$szeregieeuwzgl <- renderPlotly({
    
    ret <- sqlQuery(paste0("select TIME, Value from EUROSTAT where GEO='",input$GEO ,"' and SEX='",input$sex,"' and TIME>='", gsub(" ", "", yearweek(input$date1bezpszu)),"' and TIME<= '",gsub(" ", "", yearweek(input$date2bezpszu)),"' order by TIME"))
    wzg <- sqlQuery(paste0("select TIME, Value from EUROSTAT where GEO='",input$GEO ,"' and SEX='",input$sex,"' and TIME>='", gsub(" ", "", yearweek(input$date1wzglu)),"' and TIME<= '",gsub(" ", "", yearweek(input$date2wzglu)),"' order by TIME"))
    func1 <- function(x) {
      z<-paste0(strsplit(x,"W")[[1]][1], "-W", strsplit(x,"W")[[1]][2],"-1")
      z<- paste0(ISOweek::ISOweek2date(z))
      return(z)
    }
    func2 <- function(x) {
      z<-paste0(strsplit(x,"W")[[1]][1], "-W", strsplit(x,"W")[[1]][2],"-7")
      z<- paste0(ISOweek::ISOweek2date(z))
      return(z)
    }
    ret$Od <- lapply(ret$TIME, function(x) sapply(x, func1))
    ret$Do <- lapply(ret$TIME, function(x) sapply(x, func2))
    
    ret$Od <- as.Date(as.character(ret$Od), format="%Y-%m-%d")
    ret$Do <- as.Date(as.character(ret$Do), format="%Y-%m-%d")
    wzg$Od <- lapply(wzg$TIME, function(x) sapply(x, func1))
    wzg$Do <- lapply(wzg$TIME, function(x) sapply(x, func2))
    
    wzg$Od <- as.Date(as.character(wzg$Od), format="%Y-%m-%d")
    wzg$Do <- as.Date(as.character(wzg$Do), format="%Y-%m-%d")
    
    
    if(input$timeu=="monthu"){
      
      
      dailyDataret <- do.call("rbind",lapply(split(ret,ret$Od),function(x){
        tmpSeq <- seq(from=x$Od,to=x$Do,by="1 day")
        return(data.frame(Data=tmpSeq,Value=x$Value/length(tmpSeq)))
      }))
      dailyDatawzg <- do.call("rbind",lapply(split(wzg,wzg$Od),function(x){
        tmpSeq <- seq(from=x$Od,to=x$Do,by="1 day")
        return(data.frame(Data=tmpSeq,Value=x$Value/length(tmpSeq)))
      }))
      dailyDataret$RokMiesiac <- as.Date(endOfMonth(dailyDataret$Data))
      dailyDatawzg$RokMiesiac <- as.Date(endOfMonth(dailyDatawzg$Data))
      
      monthlyDataret <- aggregate(Value~RokMiesiac,data=dailyDataret,FUN=mean,na.rm=T)
      monthlyDatawzg <- aggregate(Value~RokMiesiac,data=dailyDatawzg,FUN=mean,na.rm=T)
      
      avg <- mean(monthlyDatawzg$Value)
      monthlyDataret$diff <- monthlyDataret$Value/avg
      
      colnames(monthlyDataret)[1] <- "Data"
      monthlyDataret <- monthlyDataret[order(monthlyDataret$Data),]
      monthlyDataret <- monthlyDataret[-c(1,nrow(monthlyDataret)),]
      
      
      img_3 <- (
        ggplot(data=monthlyDataret,aes(x=Data,y=diff))
        + 
          geom_line()+ggtitle("Wizualizacja względna")
        
      )
      
      
    } else if(input$timeu=="yearu"){
      ret$Od <- format(ret$Od,"%Y")
      ret <- na.omit(ret)
      wzg <- aggregate(Value ~ Od, data = ret, FUN = mean, na.rm=T)
      ret <- aggregate(Value ~ Od, data = ret, FUN = mean, na.rm=T)
      avg <- mean(wzg$Value)
      ret$diff <- ret$Value/avg
      
      plot <- ggplot(ret, aes(Od, diff, group=1)) + geom_line(size = 0.5)+ggtitle("Wizualizacja względna")
      
    } else{
      
      ret$Od <- as.Date(ret$Od, format="%Y-%m-%d")
      ret <- aggregate(Value ~ Od, data = ret, FUN = mean, na.rm=T)
      ret <- na.omit(ret)
      wzg <- aggregate(Value ~ Od, data = ret, FUN = mean, na.rm=T)
      avg <- mean(wzg$Value)
      ret$diff <- ret$Value/avg
      #df1 <- melt(ret, id.vars = "Od", measure.vars = "Liczba")
      plot <- ggplot(ret, aes(Od, diff)) + geom_line(size = 0.5) + ggtitle("Wizualizacja względna")
    }
  })
  
  
  
})



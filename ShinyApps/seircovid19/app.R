############################################
######SEIR COVID-19 Shiny app   #######
######Diego Marcondes           #######
######dmarcondes@ime.usp.br     #######
#######################################

#Pacotes
library(shiny)
library(shinythemes)
library(shinyjs)
library(rgdal)
library(leaflet)
library(knitr)
library(kableExtra)
library(shinycssloaders)
library(leaflet.extras)
library(shinyWidgets)
library(htmlwidgets)
library(tidyverse)
library(lubridate)
library(xtable)
library(rhandsontable)
options(java.parameters = "-Xss2560k")

#####Dados#####
cases_city <- readRDS("./www/cases_city.rds")
#cases_DRS <- readRDS("./www/cases_DRS.rds")
deaths_city <- readRDS("./www/deaths_city.rds")
deaths_DRS <- readRDS("./www/deaths_DRS.rds")
peak_city <- readRDS("./www/peak_city.rds")
peak_DRS <- readRDS("./www/peak_DRS.rds")
asymp <- readRDS("./www/assymptomatics.rds")
#pos <- readRDS("./www/pos.rds")
#nmodels <- readRDS("./www/nmodels.rds")
dmin <- min(ymd(cases_city$Date))
dmax <- dmin+30

######UI#####
ui <- shinyUI(fluidPage(
  useShinyjs(),
  uiOutput("title"),
  uiOutput("app")
))

# Server
server <- function(input, output) {
  #shinyURL.server()
  
  output$app = renderUI(
    navbarPage(theme = shinytheme("cyborg"),"Previsão COVID-19 em São Paulo",windowTitle = "Previsão COVID-19 - SP",
               tabPanel("Estado de São paulo",fluidPage(
               tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"),tags$style("#tbl {white-space: nowrap;}"),
                         tags$style("#state {font-size:20px;}"),
                         tags$style("#date {font-size:20px;}")),
                 sidebarLayout(
                   sidebarPanel(
                     HTML('<center><img src="logoblack.png" height="100" width="300"></center>'),
                     width = 3),
                   mainPanel(h2("Resultados para o Estado de São Paulo",align = "center"),
                             fluidRow(column(7,sliderInput("date",
                                                          h4("Data"),
                                                          min = as.Date(dmin,"%d/%m/%y"),
                                                          max = as.Date(dmax,"%d/%m/%y"),
                                                          value=as.Date(dmax),
                                                          timeFormat="%d/%m/%y")),
                                      column(5,downloadButton("report", "Gerar Relatório Executivo para a data escolhida")),
                                      tags$style(type='text/css', ".irs-grid-text { font-size: 14pt; }"),
                                      tags$style(type='text/css', "#report { width:100%; margin-top: 25px;}")))),
                   fluidRow(column(8,HTML(paste('<center><img src="SP_EPcurve_predicted_',pos,'.png" height="625" width="937"></center>',sep = ""))),
                            column(4,uiOutput("resEstado")),
                            tags$style("#resEstado{color: white;font-size: 20px;font-style: bold;
                                 }"
                            )),
                       br(),
                             br(),
                             br(),
                             br(),
               tags$p("Aplicação desenvolvida por ", 
                      tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
                      tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
                      tags$a(href="mailto:dmarcondes@ime.usp.br","dmarcondes@ime.usp.br."),align="center"),
               br())),
    
    tabPanel("Resultados por DRS",fluidPage(
      tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"),tags$style("#tbl {white-space: nowrap;}"),
                tags$style("#state {font-size:20px;}"),
                tags$style("#date {font-size:20px;}")),
      sidebarLayout(
        sidebarPanel(
          HTML('<center><img src="logoblack.png" height="100" width="300"></center>'),
          width = 3),
        mainPanel(
        br(),
        br(),
        br(),
        br(),
        tags$p("Aplicação desenvolvida por ", 
               tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
               tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
               tags$a(href="mailto:dmarcondes@ime.usp.br","dmarcondes@ime.usp.br."),align="center"),
        br())))),
    
    tabPanel("Resultados por Município",fluidPage(
      tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"),tags$style("#tbl {white-space: nowrap;}"),
                tags$style("#state {font-size:20px;}"),
                tags$style("#date {font-size:20px;}")),
      sidebarLayout(
        sidebarPanel(
          HTML('<center><img src="logoblack.png" height="100" width="300"></center>'),
          width = 3),
        mainPanel(
        br(),
        br(),
        br(),
        br(),
        tags$p("Aplicação desenvolvida por ", 
               tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
               tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
               tags$a(href="mailto:dmarcondes@ime.usp.br","dmarcondes@ime.usp.br."),align="center"),
        br()))))))
  
  output$report <- downloadHandler(
     filename <- paste("Relatório Previsão SEIR IME USP_",input$date,".pdf",sep = ""),
     content <- function(file) {
       withProgress(message = 'Baixando', value = 0,{
         incProgress(1/3)
         tempReport <- file.path(tempdir(), "report.Rmd")
         file.copy("./www/report.Rmd", tempReport, overwrite = TRUE)
         templogo <- file.path(tempdir(), "logoblack.png")
         file.copy("./www/logoblack.png", templogo, overwrite = TRUE)
         templogoUSP <- file.path(tempdir(), "logo_USP.png")
         file.copy("./www/logo_USP.png", templogoUSP, overwrite = TRUE)
         temppico <- file.path(tempdir(),paste("risk_peak_",pos,".png",sep = ""))
         file.copy(paste("./www/risk_peak_",pos,".png",sep = ""), temppico, overwrite = TRUE)
         params <- list(input = input,data = format(dmin,"%d/%m/%Y"),cases_city = cases_city,#cases_DRS = cases_DRS,
                        deaths_city = deaths_city,deaths_DRS = deaths_DRS,peak_DRS = peak_DRS,peak_city = peak_city,
                        pos = pos,nmodels = nmodels)
         incProgress(1/3)
         rmarkdown::render(tempReport, output_file = file,
                           params = params,
                           envir = new.env(parent = globalenv())
         )
         
         incProgress(1/3)})
     }
   )
  
  output$resEstado <- renderUI({
    tmpC <- cases_city %>% filter(Date == ymd(input$date))
    tmpM <- deaths_city %>% filter(Date == ymd(input$date))
    t1 <- paste("<h3 align='center'> ",format(input$date,"%d/%m/%Y")," <br> Estado de São Paulo <br><br> <span style='color: yellow;'>",
                format(round(sum(tmpC$Mediana)),decimal.mark = ",",big.mark = "."),
                "<br>[",format(round(sum(tmpC$Infimo)),decimal.mark = ",",big.mark = "."),",",
                format(round(sum(tmpC$Sup)),decimal.mark = ",",big.mark = "."),"] <br> Casos Confirmados</span> <br><br> <span style='color: red;'>",
                format(round(sum(tmpM$Mediana)),decimal.mark = ",",big.mark = "."),"<br>[",
                format(round(sum(tmpM$Infimo)),decimal.mark = ",",big.mark = "."),",",
                format(round(sum(tmpM$Sup)),decimal.mark = ",",big.mark = "."),"] <br> Mortes </h3>",sep = "")
    HTML(t1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


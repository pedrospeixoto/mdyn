#######################################
######SEIR COVID-19 Shiny app   #######
######Diego Marcondes           #######
######dmarcondes@ime.usp.br     #######
#######################################

#Pacotes
library(shiny)
library(shinythemes)
library(shinyjs)
library(knitr)
library(kableExtra)
library(shinyWidgets)
library(htmlwidgets)
library(tidyverse)
library(rmarkdown)
library(stringi)
library(lubridate)
library(xtable)
library(rhandsontable)
options(java.parameters = "-Xss2560k")

#####Dados#####
cases_city <- readRDS("./www/cases_city.rds")
cases_DRS <- readRDS("./www/cases_DRS.rds")
deaths_city <- readRDS("./www/deaths_city.rds")
deaths_DRS <- readRDS("./www/deaths_DRS.rds")
peak_city <- readRDS("./www/peak_city.rds")
peak_DRS <- readRDS("./www/peak_DRS.rds")
Rt <- readRDS("./www/Rt.rds")
Rt$Municipio <- factor(stri_trans_totitle(tolower(as.character(Rt$Municipio))))
param <- readRDS("./www/param.rds")
pos <- readRDS("./www/pos.rds")
nmodels <- readRDS("./www/nmodels.rds")
drs <- readRDS("./www/drs.rds")
dmin <- min(ymd(cases_city$Date))
dmax <- dmin+20
l_drs <- levels(cases_DRS$DRS)
l_city <- as.character(unique(cases_city$Municipio[as.numeric(cases_city$c_1000) == 1]))
l_drs2 <- c(l_drs,l_city)
names(l_drs2) <- c(paste("DRS",l_drs),paste("Cidade de",stri_trans_totitle(tolower(as.character(l_city)))))
l_drs2 <- l_drs2[order(names(l_drs2))]
l_drs2 <- c("SP",l_drs2)
names(l_drs2)[1] <- "Estado de São Paulo"

myDownloadButton <- function(outputId, label = "Download"){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, NULL, label)
}

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
    navbarPage(theme = shinytheme("spacelab"),"CoronaMoove",windowTitle = "Previsão COVID-19 - SP",
               tabPanel("Estado de São Paulo",fluidPage(
               tags$head(tags$link(rel = "shortcut icon", href = "favicon.jpg"),tags$style("#tbl {white-space: nowrap;}"),
                         tags$style(type='text/css', "* { font-family: 'lato light' !important; }" ),
                         tags$style("#state {font-size:20px;}"),
                         tags$style("#date {font-size:20px;}")),
                 sidebarLayout(
                   sidebarPanel(
                     HTML('<center><a href="https://www.ime.usp.br/"><img src="logo.jpg" height="200" width="150"></center></a>'),
                     width = 2),
                   mainPanel(HTML("<h1 align='center'><span style='font-weight: bold;'>Previsão COVID-19 para o Estado de São Paulo </span></h1>"),
                             br(),
                             fluidRow(column(8,sliderInput("date",
                                                          h3("Escolha a data para previsão"),
                                                          min = as.Date(dmin,"%d/%m/%y"),
                                                          max = as.Date(dmax,"%d/%m/%y"),
                                                          value=as.Date(dmax),
                                                          timeFormat="%d/%m/%y",width = '90%'),align = "center"),
                                      column(4,div(style="display:inline-block;text-align:center;float:left;",
                                                   myDownloadButton("report", "Gerar Relatório Executivo para a data escolhida")),align = "center"),
                                      tags$style(type='text/css', ".irs-grid-text { font-size: 10pt; }"),
                                      tags$style(type='text/css', 
                                                 "#report {width:130%; margin-top: 60px; margin-left: 75px; align:center}")))),
                   fluidRow(column(8,HTML(paste('<center><img src="SP_EPcurve_predicted_',pos,
                                                '.png" height="625" width="937"></center>',sep = ""))),
                            column(4,uiOutput("resEstado"),style = "background-color:#F5F5F5;")),
               p("*A linha e banda vermelha dizem respeito ao número de indivíduos que estão infectados simultâneamente na data. As linhas tracejadas são as estimativas (mediana, pior caso e melhor caso) para o pico de mortes pela doença (dia com mais mortes)."),
               fluidRow(column(6,HTML("<h2 align='center'><span style='font-weight: bold; color: #1E90FF;'>Previsão Mortes por Departamento Regional de Saúde </span> </h2>")),
                        column(6,HTML("<h2 align='center'><span style='font-weight: bold; color: #1E90FF;'>Previsão Casos por Departamento Regional de Saúde </span> </h2>"))),
               fluidRow(column(6,rHandsontableOutput("mortes_DRS")),
                        column(6,rHandsontableOutput("casos_DRS")),
                        tags$style(type="text/css", "#tabela th {font-weight:bold;}")),
               br(),
               fluidRow(column(6,HTML("<h2 align='center'><span style='font-weight: bold; color: #1E90FF;'>Previsão Mortes por Município </span> </h2>")),
                        column(6,HTML("<h2 align='center'><span style='font-weight: bold; color: #1E90FF;'>Previsão Casos por Município </span> </h2>"))),
               fluidRow(column(6,rHandsontableOutput("mortes_city")),
                        column(6,rHandsontableOutput("casos_city")),
                        tags$style(type="text/css", "#tabela th {font-weight:bold;}")),
               p("*Os valores nas tabelas dizem respeito à data escolhida. O modelo projeta casos e mortes apenas para os DRSs e Municípios com mais de 1,000 casos confirmados ou 100 mortes confirmadas quando o modelo foi ajustado. Municípios e DRSs que não satisfazem essa condição foram omitidos."),
                             br(),
               fluidRow(column(3,myDownloadButton("videoMortesSP","Baixar animação com a previsão de mortes")),
                        column(3,myDownloadButton("videoCasosSP","Baixar animação com a previsão de casos"))),
               br(),
               HTML("<p> <span style='color: red'><strong>*As animações acima APENAS ilustram o comportamento do espalhamento espaço-temporal da COVID-19, sendo que os valores exatos de casos e mortes confirmadas projetados NÃO DEVEM SER CONSIDERADOS.</strong></span></p>"),
               br(),
               tags$p("Aplicação desenvolvida por ", 
                      tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
                      tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
                      tags$a(href="mailto:dmarcondes@ime.usp.br","dmarcondes@ime.usp.br."),align="center"),
               br())),
    
               tabPanel("Departamento Regional de Saúde",fluidPage(
                 tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"),tags$style("#tbl {white-space: nowrap;}"),
                           tags$style(type='text/css', "* { font-family: 'lato light' !important; }" )),
                 sidebarLayout(
                   sidebarPanel(
                     HTML('<center><a href="https://www.ime.usp.br/"><img src="logo.jpg" height="200" width="150"></center></a>'),
                     width = 2),
                   mainPanel(uiOutput("titleDRS"),
                             br(),
                             fluidRow(column(3,selectizeInput(inputId = "DRS",label = h3("DRS"),
                                                             choices = l_drs,multiple = F,selected = "Grande São Paulo"),align = "center"),
                                      column(6,sliderInput("dateDRS",
                                                           h3("Escolha a data para previsão"),
                                                           min = as.Date(dmin,"%d/%m/%y"),
                                                           max = as.Date(dmax,"%d/%m/%y"),
                                                           value=as.Date(dmax),
                                                           timeFormat="%d/%m/%y",width = '90%'),align = "center"),
                                      column(3,div(style="display:inline-block;text-align:center;float:left;",
                                                   myDownloadButton("reportDRS", 
                                                                    "Gerar Relatório Executivo para a data e DRS escolhido"))),
                                      tags$style(type='text/css', ".irs-grid-text { font-size: 10pt; }"),
                                      tags$style(type='text/css',
                                                 "#reportDRS {width:110%; margin-top: 60px; margin-left: 30px; align:center}")
                                      ))),
                 fluidRow(column(8,uiOutput("EPDRS")),
                          column(4,uiOutput("resDRS"),style = "background-color:#F5F5F5;")),
                 p("*A linha e banda vermelha dizem respeito ao número de indivíduos que estão infectados simultâneamente na data. As linhas tracejadas são as estimativas (mediana, pior caso e melhor caso) para o pico de mortes pela doença (dia com mais mortes)."),
                 fluidRow(column(6,HTML("<h2 align='center'><span style='font-weight: bold; color: #1E90FF;'>Previsão Mortes por Município </span> </h2>")),
                          column(6,HTML("<h2 align='center'><span style='font-weight: bold; color: #1E90FF;'>Previsão Casos por Município </span> </h2>"))),
                 fluidRow(column(6,rHandsontableOutput("mortes_city_DRS")),
                          column(6,rHandsontableOutput("casos_city_DRS")),
                          tags$style(type="text/css", "#tabela th {font-weight:bold;}")),
                 p("*Os valores nas tabelas dizem respeito à data escolhida. O modelo projeta casos e mortes apenas para os DRSs e Municípios com mais de 1,000 casos confirmados ou 100 mortes confirmadas quando o modelo foi ajustado. Municípios e DRSs que não satisfazem essa condição foram omitidos."),
                 br(),
                 fluidRow(column(3,myDownloadButton("videoMortesDRS","Baixar animação com a previsão de mortes")),
                          column(3,myDownloadButton("videoCasosDRS","Baixar animação com a previsão de casos"))),
                 br(),
                 HTML("<p> <span style='color: red'><strong>*As animações acima APENAS ilustram o comportamento do espalhamento espaço-temporal da COVID-19, sendo que os valores exatos de casos e mortes confirmadas projetados NÃO DEVEM SER CONSIDERADOS.</strong></span></p>"),
                 br(),
                 tags$p("Aplicação desenvolvida por ", 
                        tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
                        tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
                        tags$a(href="mailto:dmarcondes@ime.usp.br","dmarcondes@ime.usp.br."),align="center"),
                 br())),
               
               tabPanel("Município",fluidPage(
                 tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"),tags$style("#tbl {white-space: nowrap;}"),
                           tags$style(type='text/css', "* { font-family: 'lato light' !important; }" )),
                 sidebarLayout(
                   sidebarPanel(
                     HTML('<center><a href="https://www.ime.usp.br/"><img src="logo.jpg" height="200" width="150"></center></a>'),
                     width = 2),
                   mainPanel(uiOutput("titleCity"),
                             br(),
                             fluidRow(column(6,selectizeInput(inputId = "city",label = h3("Município"),
                                                              choices = l_city,multiple = F,selected = "SÃO PAULO"),align = "center"),
                                      column(6,sliderInput("dateCity",
                                                           h3("Escolha a data para previsão"),
                                                           min = as.Date(dmin,"%d/%m/%y"),
                                                           max = as.Date(dmax,"%d/%m/%y"),
                                                           value=as.Date(dmax),
                                                           timeFormat="%d/%m/%y",width = '90%'),align = "center"),
                                      tags$style(type='text/css', ".irs-grid-text { font-size: 10pt; }")))),
                 fluidRow(column(8,uiOutput("EPCity")),
                          column(4,uiOutput("resCity"),style = "background-color:#F5F5F5;")),
                 p("*A linha e banda vermelha dizem respeito ao número de indivíduos que estão infectados simultâneamente na data. As linhas tracejadas são as estimativas (mediana, pior caso e melhor caso) para o pico de mortes pela doença (dia com mais mortes). O modelo projeta casos e mortes para todas os DRSs e apenas para Municípios com mais de 100 casos confirmados quando o modelo foi ajustado."),
                 br(),
                 tags$p("Aplicação desenvolvida por ", 
                        tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
                        tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
                        tags$a(href="mailto:dmarcondes@ime.usp.br","dmarcondes@ime.usp.br."),align="center"),
                 br())),
               
               tabPanel("R Efetivo",fluidPage(
                 tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"),tags$style("#tbl {white-space: nowrap;}"),
                           tags$style(type='text/css', "* { font-family: 'lato light' !important; }" )),
                 sidebarLayout(
                   sidebarPanel(
                     HTML('<center><a href="https://www.ime.usp.br/"><img src="logo.jpg" height="200" width="150"></center></a>'),
                     width = 2),
                   mainPanel(HTML("<h1 align='center'><span style='font-weight: bold;'>Número Reprodutivo Efetivo</span></h1>"),
                             br(),
                             fluidRow(column(12,HTML(paste("<h3 align='center'> O <span style='color: #1E90FF'> Número Reprodutivo Efetivo </span> representa o número médio de casos secundários causados por um indivíduo infectado residente no Município. Os valores se referem à semana terminando em",
                                                           format(dmin,"%d/%m/%Y"),"</h3>")),slign = "center")))),
                 fluidRow(column(6,img(src=paste('SP_Rt_',pos,'.png',sep = ""),height = 468,width = 702,align = "center")),
                          column(6,rHandsontableOutput("tableRt"))),
                 br(),
                 p(paste("*O Número Reprodutivo Efetivo foi calculado apenas para os Municípios com mais de 1,000 casos ou 100 mortes na semana terminando em ",
                         format(dmin,"%d/%m/%Y"))),
                 br(),
                 tags$p("Aplicação desenvolvida por ", 
                        tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
                        tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
                        tags$a(href="mailto:dmarcondes@ime.usp.br","dmarcondes@ime.usp.br."),align="center"),
                 br())),
               
               tabPanel("Aderência",fluidPage(
                 tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"),tags$style("#tbl {white-space: nowrap;}"),
                           tags$style(type='text/css', "* { font-family: 'lato light' !important; }" )),
                 sidebarLayout(
                   sidebarPanel(
                     HTML('<center><a href="https://www.ime.usp.br/"><img src="logo.jpg" height="200" width="150"></center></a>'),
                     width = 2),
                   mainPanel(uiOutput("titleAD"),
                             fluidRow(column(12,selectizeInput(inputId = "DRSad",label = h3("DRS ou Município"),
                                                              choices = l_drs2,multiple = F,selected = "Estado de São Paulo"),align = "center")),
                             fluidRow(column(12,uiOutput("textGOF"),slign = "center")))),
                 fluidRow(column(6,uiOutput("aderiMortes")),column(6,uiOutput("aderiCasos"))),
                 br(),
                 p(paste("*A aderência foi testada apenas para os DRSs e Municípios com mais de 1,000 casos ou 100 mortes na semana terminando em ",
                         format(dmin,"%d/%m/%Y"))),
                 br(),
                 tags$p("Aplicação desenvolvida por ", 
                        tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
                        tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
                        tags$a(href="mailto:dmarcondes@ime.usp.br","dmarcondes@ime.usp.br."),align="center"),
                 br())),
               
               tabPanel("Sobre",fluidPage(
                 tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"),tags$style("#tbl {white-space: nowrap;}"),
                           tags$style(type='text/css', "* { font-family: 'lato light' !important; }" )),
                 fluidRow(column(12, HTML('<center><a href="https://www.ime.usp.br/"><img src="logo_horizontal.jpg" height="100" width="300"></center></a>'),
                                 align = "center")),
                 br(),
                 fluidRow(column(2),column(8,withMathJax(includeHTML("./www/sobre.html")),style = "background-color:#F5F5F5;")),
                 br(),
                 br(),
                 tags$p("Aplicação desenvolvida por ", 
                        tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
                        tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
                        tags$a(href="mailto:dmarcondes@ime.usp.br","dmarcondes@ime.usp.br."),align="center"),
                 br()))))
  
  output$report <- downloadHandler(
     filename <- paste("Relatório Previsão SEIR IME USP_SP_",input$date,".pdf",sep = ""),
     content <- function(file) {
       withProgress(message = 'Baixando', value = 0,{
         incProgress(1/3)
         tempReport <- file.path(tempdir(), "report_SP.Rmd")
         file.copy("./www/report_SP.Rmd", tempReport, overwrite = TRUE)
         templogo <- file.path(tempdir(), "logoblack.jpg")
         file.copy("./www/logoblack.jpg", templogo, overwrite = TRUE)
         templogoUSP <- file.path(tempdir(), "logo_USP.jpg")
         file.copy("./www/logo_USP.jpg", templogoUSP, overwrite = TRUE)
         temppico <- file.path(tempdir(),paste("risk_peak_",pos,".png",sep = ""))
         file.copy(paste("./www/risk_peak_",pos,".png",sep = ""), temppico, overwrite = TRUE)
         tempEP <- file.path(tempdir(),paste("SP_EPcurve_predicted_",pos,".png",sep = ""))
         file.copy(paste("./www/SP_EPcurve_predicted_",pos,".png",sep = ""), tempEP, overwrite = TRUE)
         temp_mortes <- file.path(tempdir(),paste("mortes.png",sep = ""))
         file.copy(paste("./www/mortes/",sprintf("%03d", as.numeric(ymd(input$date) - ymd(dmin)) + 1),".png",sep = ""),
                   temp_mortes,overwrite = TRUE)
         temp_casos <- file.path(tempdir(),paste("casos.png",sep = ""))
         file.copy(paste("./www/casos/",sprintf("%03d", as.numeric(ymd(input$date) - ymd(dmin)) + 1),".png",sep = ""),
                   temp_casos,overwrite = TRUE)
         params <- list(input = input,data = format(dmin,"%d/%m/%Y"),cases_city = cases_city,cases_DRS = cases_DRS,
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
  
  output$videoMortesSP <- downloadHandler(
    filename <- "mortes_Estado.mp4",
    content <- function(file) {
      file.copy("./www/mortes_Estado.mp4", file)
    },
    contentType = "video"
  )
  
  observe({
    if(!is.null(input$DRS)){
      output$videoMortesDRS <- downloadHandler(
      filename <- function(){paste("mortes_",gsub(" ","",input$DRS),".mp4",sep = "")},
      content <- function(file) {
        file.copy(paste("./www/mortes_",gsub(" ","",input$DRS),".mp4",sep = ""), file)
      },
      contentType = "video"
      )
  
    output$videoCasosSP <- downloadHandler(
     filename <- "casos_Estado.mp4",
      content <- function(file) {
       file.copy("./www/casos_Estado.mp4", file)
      },
      contentType = "video"
    )
    
  
    output$videoCasosDRS <- downloadHandler(
      filename <- function(){paste("casos_",gsub(" ","",input$DRS),".mp4",sep = "")},
      content <- function(file) {
        file.copy(paste("./www/casos_",gsub(" ","",input$DRS),".mp4",sep = ""), file)
      },
      contentType = "video"
    )
    }
  })
  
  output$resEstado <- renderUI({
    if(!is.null(input$date)){
     tmpC <- cases_city %>% filter(Date == ymd(input$date))
      tmpM <- deaths_city %>% filter(Date == ymd(input$date))
      t1 <- paste("<h1 align='center'> <span style='color: #1E90FF;'><strong> Pevisão para ",format(input$date,"%d/%m/%Y"),
                  " </strong></span></h1> <h2 align='center',face = 'bold'> <br> <span style='color: orange;'><strong>Casos Confirmados: ",
                  format(round(sum(tmpC$Mediana)),decimal.mark = ",",big.mark = "."),
                  "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Melhor Cenário: ",
                  format(round(sum(tmpC$Infimo)),decimal.mark = ",",big.mark = "."),
                  "<br> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Pior Cenário: ",
                  format(round(sum(tmpC$Sup)),decimal.mark = ",",big.mark = "."),
                  "</strong></span> <br><br> <span style='color: red;'> <strong> Mortes Confirmadas: ",
                  format(round(sum(tmpM$Mediana)),decimal.mark = ",",big.mark = "."),"&nbsp;&nbsp;&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Melhor Cenário: ",
                  format(round(sum(tmpM$Infimo)),decimal.mark = ",",big.mark = "."),
                  "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Pior Cenário: ",
                  format(round(sum(tmpM$Sup)),decimal.mark = ",",big.mark = "."),
                  "</strong></span> </h2> <br> <h4 align='center'> As previsões são realizadas através de Modelos SEIR Metapopulação com Mobilidade supondo que a evolução da doença se manterá da mesma forma que na semana terminando em ",
                  format(dmin,"%d/%m/%Y"),
                  "</h4> </p> <p align = 'center'> <span style='color: red;'> <strong> DISCLAIMER: Apenas as previsões pontuais realizadas até o dia ",
                  format(dmax,"%d/%m/%Y"),
                  " e a projeção do pico da doença devem ser considerados, e somente para os DRSs e Municípios com mais de 1,000 casos ou 100 mortes em ",
                  format(dmin,"%d/%m/%Y")," (os demais DRSs e Municípios foram omitidos). Embora apresentadas nos gráficos, as previsões pontuais após ",
                  format(dmax,"%d/%m/%Y"),
                  " NÃO DEVEM SER CONSIDERADAS. </strong> </span> </p> <p align='center'> Desenvolvido por pesquisadores do <a href='https://www.ime.usp.br/'>IME-USP</a> </p> <p align='center'> Atualizado em ",format(dmin,"%d/%m/%Y"),"</p>",sep = "")
      HTML(t1)
    }
  })
  
  output$resDRS <- renderUI({
    tmpC <- cases_DRS %>% filter(Date == ymd(input$dateDRS) & DRS == input$DRS)
    tmpM <- deaths_DRS %>% filter(Date == ymd(input$dateDRS) & DRS == input$DRS)
    if(input$DRS == "Grande São Paulo")
      t1 <- paste("<h1 align='center'> <span style='color: #1E90FF;'><strong>  Previsão para ",format(input$dateDRS,"%d/%m/%Y"),
                  "</strong></span></h1> <p align='center'> <span style='color: #1E90FF;'><strong> (Excluindo a Cidade de São Paulo) </strong></span></p> <h2 align='center',face = 'bold'> <br> <span style='color: orange;'> <strong> Casos Confirmados: ",
                  format(round(sum(tmpC$Mediana)),decimal.mark = ",",big.mark = "."),
                  "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Melhor Cenário: ",
                  format(round(sum(tmpC$Infimo)),decimal.mark = ",",big.mark = "."),
                  "<br> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Pior Cenário: ",
                  format(round(sum(tmpC$Sup)),decimal.mark = ",",big.mark = "."),"</span> <br><br> <span style='color: red;'> Mortes Confirmadas: ",
                  format(round(sum(tmpM$Mediana)),decimal.mark = ",",big.mark = "."),
                  "&nbsp;&nbsp;&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Melhor Cenário: ",
                  format(round(sum(tmpM$Infimo)),decimal.mark = ",",big.mark = "."),
                  "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Pior Cenário: ",
                  format(round(sum(tmpM$Sup)),decimal.mark = ",",big.mark = "."),
                  "</strong></span> </h2> <h4 align='center'> As previsões são realizadas através de Modelos SEIR Metapopulação com Mobilidade supondo que a evolução da doença se manterá da mesma forma que na semana terminando em ",
                  format(dmin,"%d/%m/%Y"),"</h4> </p> <p align = 'center'> <span style='color: red;'> <strong> DISCLAIMER: Apenas as previsões pontuais realizadas até o dia ",format(dmax,"%d/%m/%Y"),
                  " e a projeção do pico da doença devem ser considerados, e somente para os DRSs e Municípios com mais de 1,000 casos ou 100 mortes em ",
                  format(dmin,"%d/%m/%Y")," (os demais DRSs e Municípios foram omitidos). Embora apresentadas nos gráficos, as previsões pontuais após ",
                  format(dmax,"%d/%m/%Y"),
                  " NÃO DEVEM SER CONSIDERADAS. </strong> </span> </p> <p align='center'> Desenvolvido por pesquisadores do <a href='https://www.ime.usp.br/'>IME-USP</a> </p> <p align='center'> Atualizado em ",format(dmin,"%d/%m/%Y"),"</p>",sep = "")
    else
      t1 <- paste("<h1 align='center'> <span style='color: #1E90FF;'><strong> Previsão para ",format(input$dateDRS,"%d/%m/%Y"),
                  "</strong></span></h1> <h2 align='center',face = 'bold'> <br> <span style='color: orange;'> <strong> Casos Confirmados: ",
                  format(round(sum(tmpC$Mediana)),decimal.mark = ",",big.mark = "."),
                  "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Melhor Cenário: ",
                  format(round(sum(tmpC$Infimo)),decimal.mark = ",",big.mark = "."),
                  "<br> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Pior Cenário: ",
                  format(round(sum(tmpC$Sup)),decimal.mark = ",",big.mark = "."),"</span> <br><br> <span style='color: red;'> Mortes Confirmadas: ",
                  format(round(sum(tmpM$Mediana)),decimal.mark = ",",big.mark = "."),
                  "&nbsp;&nbsp;&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Melhor Cenário: ",
                  format(round(sum(tmpM$Infimo)),decimal.mark = ",",big.mark = "."),
                  "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Pior Cenário: ",
                  format(round(sum(tmpM$Sup)),decimal.mark = ",",big.mark = "."),
                  "</strong></span> </h2> <h4 align='center'> As previsões são realizadas através de Modelos SEIR Metapopulação com Mobilidade supondo que a evolução da doença se manterá da mesma forma que na semana terminando em ",
                  format(dmin,"%d/%m/%Y"),"</h4> </p> <p align = 'center'> <span style='color: red;'> <strong> DISCLAIMER: Apenas as previsões pontuais realizadas até o dia ",format(dmax,"%d/%m/%Y"),
                  " e a projeção do pico da doença devem ser considerados, e somente para os DRSs e Municípios com mais de 1,000 casos ou 100 mortes em ",
                  format(dmin,"%d/%m/%Y")," (os demais DRSs e Municípios foram omitidos). Embora apresentadas nos gráficos, as previsões pontuais após ",
                  format(dmax,"%d/%m/%Y"),
                  " NÃO DEVEM SER CONSIDERADAS. </strong> </span> </p> <p align='center'> Desenvolvido por pesquisadores do <a href='https://www.ime.usp.br/'>IME-USP</a> </p> <p align='center'> Atualizado em ",format(dmin,"%d/%m/%Y"),"</p>",sep = "")
    HTML(t1)
  })
  
  output$resCity <- renderUI({
    tmpC <- cases_city %>% filter(Date == ymd(input$dateCity) & Municipio == input$city)
    tmpM <- deaths_city %>% filter(Date == ymd(input$dateCity) & Municipio == input$city)
    t1 <- paste("<h1 align='center'> <span style='color: #1E90FF;'><strong> Previsão para ",format(input$dateDRS,"%d/%m/%Y"),
                "</strong></span></h1> <h2 align='center',face = 'bold'> <br> <span style='color: orange;'> <strong> Casos Confirmados: ",
                format(round(tmpC$Mediana),decimal.mark = ",",big.mark = "."),
                "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Melhor Cenário: ",
                format(round(tmpC$Infimo),decimal.mark = ",",big.mark = "."),
                "<br> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Pior Cenário: ",
                format(round(tmpC$Sup),decimal.mark = ",",big.mark = "."),"</span> <br><br> <span style='color: red;'> Mortes Confirmadas: ",
                format(round(tmpM$Mediana),decimal.mark = ",",big.mark = "."),
                "&nbsp;&nbsp;&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Melhor Cenário: ",
                format(round(tmpM$Infimo),decimal.mark = ",",big.mark = "."),
                "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Pior Cenário: ",
                format(round(tmpM$Sup),decimal.mark = ",",big.mark = "."),
                "</strong></span> </h2> <br> <h4 align='center'> As previsões são realizadas através de Modelos SEIR Metapopulação com Mobilidade supondo que a evolução da doença se manterá da mesma forma que na semana terminando em ",
                format(dmin,"%d/%m/%Y"),"</h4> </p> <p align = 'center'> <span style='color: red;'> <strong> DISCLAIMER: Apenas as previsões pontuais realizadas até o dia ",format(dmax,"%d/%m/%Y"),
                " e a projeção do pico da doença devem ser considerados, e somente para os DRSs e Municípios com mais de 1,000 casos ou 100 mortes em ",
                format(dmin,"%d/%m/%Y")," (os demais DRSs e Municípios foram omitidos). Embora apresentadas nos gráficos, as previsões pontuais após ",
                format(dmax,"%d/%m/%Y"),
                " NÃO DEVEM SER CONSIDERADAS. </strong> </span> </p> <p align='center'> Desenvolvido por pesquisadores do <a href='https://www.ime.usp.br/'>IME-USP</a> </p> <p align='center'> Atualizado em ",format(dmin,"%d/%m/%Y"),"</p>",sep = "")
    HTML(t1)
  })
  
  observe({
    if(!is.null(input$date)){
      tmpC_DRS <- cases_DRS %>% filter(Date == input$date)
      tmpM_DRS <- deaths_DRS %>% filter(Date == input$date)
      tmpC_city <- cases_city %>% filter(Date == input$date & c_1000) %>% droplevels()
      tmpM_city <- deaths_city %>% filter(Date == input$date & c_1000) %>% droplevels()
      
      tmpM_DRS <- tmpM_DRS[order(tmpM_DRS$Mediana,decreasing = T),c(2:5)]
      names(tmpM_DRS) <- c("DRS","Mínimo","Mediana","Máximo")
      tmpM_DRS[,2:4] <- sapply(tmpM_DRS[,2:4], FUN=function(x) prettyNum(round(x),decimal.mark = ",",big.mark="."))
      
      tmpC_DRS <- tmpC_DRS[order(tmpC_DRS$Mediana,decreasing = T),c(2:5)]
      names(tmpC_DRS) <- c("DRS","Mínimo","Mediana","Máximo")
      tmpC_DRS[,2:4] <- sapply(tmpC_DRS[,2:4], FUN=function(x) prettyNum(round(x),decimal.mark = ",",big.mark="."))
      
      tmpM_city <- tmpM_city[order(tmpM_city$Mediana,decreasing = T),-c(1,6)]
      names(tmpM_city) <- c("Município","Mínimo","Mediana","Máximo")
      tmpM_city[,2:4] <- sapply(tmpM_city[,2:4], FUN=function(x) prettyNum(round(x),decimal.mark = ",",big.mark="."))
      tmpM_city[,1] <- factor(stri_trans_totitle(tolower(as.character(tmpM_city[,1]))))
      
      tmpC_city <- tmpC_city[order(tmpC_city$Mediana,decreasing = T),-c(1,6)]
      names(tmpC_city) <- c("Município","Mínimo","Mediana","Máximo")
      tmpC_city[,2:4] <- sapply(tmpC_city[,2:4], FUN=function(x) prettyNum(round(x),decimal.mark = ",",big.mark="."))
      tmpC_city[,1] <- factor(stri_trans_totitle(tolower(as.character(tmpC_city[,1]))))
      
      output$mortes_DRS <- renderRHandsontable({
        rhandsontable(data = tmpM_DRS,stretchH = "all",rowHeaders = NULL,digits = 0) %>%
          hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = '#F5F5F5';
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   td.style.color = 'black';}") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_col(col = c(2,3,4),valign = "htCenter")
        
      })
      
      output$casos_DRS <- renderRHandsontable({
        rhandsontable(data = tmpC_DRS,stretchH = "all",rowHeaders = NULL,digits = 0) %>%
          hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = '#F5F5F5';
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   td.style.color = 'black';}") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_col(col = c(2,3,4),valign = "htCenter")
      })
      
      output$mortes_city <- renderRHandsontable({
        rhandsontable(data = tmpM_city,stretchH = "all",rowHeaders = NULL,digits = 0) %>%
          hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = '#F5F5F5';
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   td.style.color = 'black';}") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_col(col = c(2,3,4),valign = "htCenter")
        
      })
      
      output$casos_city <- renderRHandsontable({
        rhandsontable(data = tmpC_city,stretchH = "all",rowHeaders = NULL,digits = 0) %>%
          hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = '#F5F5F5';
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   td.style.color = 'black';}") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_col(col = c(2,3,4),valign = "htCenter")
      })
    }
  })
  
  output$titleDRS <- renderUI({
    HTML(paste("<h1 align='center'><span style='font-weight: bold;'> Previsão COVID-19 para o DRS",input$DRS,"</span></h1>"))
  })
  
  output$EPDRS <- renderUI({
    #HTML(paste('<center><img src="DRS_',gsub(" ","",input$DRS),'_EPCurve_',pos,'".png" height="625" width="937"></center>',sep = ""))
    img(src=paste('DRS_',gsub(" ","",input$DRS),'_EPCurve_',pos,'.png',sep = ""),height = 625,width = 937,align = "center")
  })
  
  observe({
    if(!is.null(input$dateDRS)){
      tmpC_city <- cases_city %>% filter(Date == input$dateDRS & Municipio %in% drs$Municipio[drs$Regiao == input$DRS] & c_1000)
      tmpM_city <- deaths_city %>% filter(Date == input$dateDRS & Municipio %in% drs$Municipio[drs$Regiao == input$DRS] & c_1000)
      
      tmpM_city <- tmpM_city[order(tmpM_city$Mediana,decreasing = T),-c(1,6)]
      names(tmpM_city) <- c("Município","Mínimo","Mediana","Máximo")
      tmpM_city[,2:4] <- sapply(tmpM_city[,2:4], FUN=function(x) prettyNum(round(x),decimal.mark = ",",big.mark="."))
      tmpM_city[,1] <- factor(stri_trans_totitle(tolower(as.character(tmpM_city[,1]))))
      
      tmpC_city <- tmpC_city[order(tmpC_city$Mediana,decreasing = T),-c(1,6)]
      names(tmpC_city) <- c("Município","Mínimo","Mediana","Máximo")
      tmpC_city[,2:4] <- sapply(tmpC_city[,2:4], FUN=function(x) prettyNum(round(x),decimal.mark = ",",big.mark="."))
      tmpC_city[,1] <- factor(stri_trans_totitle(tolower(as.character(tmpC_city[,1]))))
      
      output$mortes_city_DRS <- renderRHandsontable({
        rhandsontable(data = tmpM_city,stretchH = "all",rowHeaders = NULL,digits = 0) %>%
          hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = '#F5F5F5';
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   td.style.color = 'black';}") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_col(col = c(2,3,4),valign = "htCenter")
        
      })
      
      output$casos_city_DRS <- renderRHandsontable({
        rhandsontable(data = tmpC_city,stretchH = "all",rowHeaders = NULL,digits = 0) %>%
          hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = '#F5F5F5';
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   td.style.color = 'black';}") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_col(col = c(2,3,4),valign = "htCenter")
      })
    }
  })
  
  output$reportDRS <- downloadHandler(
    filename <- function(){paste("Relatório Previsão SEIR IME USP_",input$DRS,"_",input$dateDRS,".pdf",sep = "")},
    content <- function(file) {
      withProgress(message = 'Baixando', value = 0,{
        incProgress(1/3)
        tempReport <- file.path(tempdir(), "report_DRS.Rmd")
        file.copy("./www/report_DRS.Rmd", tempReport, overwrite = TRUE)
        templogo <- file.path(tempdir(), "logoblack.jpg")
        file.copy("./www/logoblack.jpg", templogo, overwrite = TRUE)
        templogoUSP <- file.path(tempdir(), "logo_USP.jpg")
        file.copy("./www/logo_USP.jpg", templogoUSP, overwrite = TRUE)
        tempEP <- file.path(tempdir(),"EPcurve.png")
        file.copy(paste('./www/DRS_',gsub(" ","",input$DRS),'_EPCurve_',pos,'.png',sep = ""), tempEP,
                  overwrite = TRUE)
        
        params <- list(drs = drs,DRS = input$DRS,input = input,data = format(dmin,"%d/%m/%Y"),cases_city = cases_city,
                       cases_DRS = cases_DRS,
                       deaths_city = deaths_city,deaths_DRS = deaths_DRS,peak_DRS = peak_DRS,peak_city = peak_city,
                       pos = pos,nmodels = nmodels,dmin = dmin)
        incProgress(1/3)
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        
        incProgress(1/3)})
    }
  )
  
  output$titleCity <- renderUI({
    HTML(paste("<h1 align='center'><span style='font-weight: bold;'> Previsão COVID-19 para",input$city,"-SP </span></h1>"))
  })
  
  output$EPCity <- renderUI({
    img(src=paste(gsub(" ","",input$city),'_EPCurve_',pos,'.png',sep = ""),height = 625,width = 937,align = "center")
  })
  
  output$titleAD <- renderUI({
    if(input$DRSad == "SP")
      HTML(paste("<h1 align='center'><span style='font-weight: bold;'> Aderência para o Estado de São Paulo </span></h1>"))
    else{
      if(input$DRSad %in% l_drs)
        HTML(paste("<h1 align='center'><span style='font-weight: bold;'> Aderência para o",names(l_drs2)[l_drs2 == input$DRSad],"</span></h1>"))
      else
        HTML(paste("<h1 align='center'><span style='font-weight: bold;'> Aderência para a",names(l_drs2)[l_drs2 == input$DRSad],"</span></h1>"))
    }
  })
  
  output$textGOF <- renderUI({
    if(input$DRSad == "SP")
      HTML(paste("<h4 align='center'> As figuras abaixo mostram como os modelos (linhas tracejadas e bandas de confiança) se aderiram aos dados observados (linha sólida) no Estado de São Paulo na semana terminando em",format(dmin,"%d/%m/%Y"),"</h4>"))
    else{
      if(input$DRSad %in% l_drs)
        HTML(paste("<h4 align='center'> As figuras abaixo mostram como os modelos (linhas tracejadas e bandas de confiança) se aderiram aos dados observados (linha sólida) no",names(l_drs2)[l_drs2 == input$DRSad],"na semana terminando em",format(dmin,"%d/%m/%Y"),"</h4>"))
      else
        HTML(paste("<h4 align='center'> As figuras abaixo mostram como os modelos (linhas tracejadas e bandas de confiança) se aderiram aos dados observados (linha sólida) na",names(l_drs2)[l_drs2 == input$DRSad],"na semana terminando em",format(dmin,"%d/%m/%Y"),"</h4>"))
    }
  })
  
  output$aderiCasos <- renderUI({
    if(input$DRSad == "SP")
      img(src=paste(gsub(" ","",input$DRSad),'_casos_',pos,'.png',sep = ""),height = 468,width = 702,align = "center")
    else{
      if(input$DRSad %in% l_drs)
        img(src=paste("DRS_",gsub(" ","",input$DRSad),'_casos.png',sep = ""),height = 468,width = 702,align = "center")
      else
        img(src=paste(gsub(" ","",input$DRSad),'_casos.png',sep = ""),height = 468,width = 702,align = "center")
    } 
  })
  
  output$aderiMortes <- renderUI({
    if(input$DRSad == "SP")
      img(src=paste(gsub(" ","",input$DRSad),'_mortes_',pos,'.png',sep = ""),height = 468,width = 702,align = "center")
    else{
      if(input$DRSad %in% l_drs)
        img(src=paste("DRS_",gsub(" ","",input$DRSad),'_mortes.png',sep = ""),height = 468,width = 702,align = "center")
      else
        img(src=paste(gsub(" ","",input$DRSad),'_mortes.png',sep = ""),height = 468,width = 702,align = "center")
    } 
  })
  
  output$tableRt <- renderRHandsontable({
    rhandsontable(data = Rt,stretchH = "all",rowHeaders = NULL,digits = 2) %>%
      hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = '#F5F5F5';
                 Handsontable.renderers.TextRenderer.apply(this, arguments);
                 td.style.color = 'black';}") %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_col(col = c(3,4,5),valign = "htCenter")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


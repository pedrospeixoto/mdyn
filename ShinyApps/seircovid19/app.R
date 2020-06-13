############################################
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
asymp <- readRDS("./www/assymptomatics.rds")
pos <- readRDS("./www/pos.rds")
nmodels <- readRDS("./www/nmodels.rds")
drs <- readRDS("./www/drs.rds")
dmin <- min(ymd(cases_city$Date))
dmax <- dmin+30
l_drs <- levels(drs$Regiao)
l_drs <- l_drs[l_drs != "Cidade de São Paulo"]
l_city <- levels(cases_city$Municipio)
tmp1 <- deaths_DRS %>% filter(Date == dmin & Mediana > 50)
tmp2 <- cases_DRS %>% filter(Date == dmin & Mediana > 500)
l_drs2 <- c("SP",levels(drs$Regiao)[levels(drs$Regiao) %in% tmp1$DRS & levels(drs$Regiao) %in% tmp2$DRS])
names(l_drs2) <- c("Estado de São Paulo",l_drs2[-1])


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
    navbarPage(theme = shinytheme("cyborg"),"Previsão COVID-19 em São Paulo",windowTitle = "Previsão COVID-19 - SP",
               tabPanel("Estado de São Paulo",fluidPage(
               tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"),tags$style("#tbl {white-space: nowrap;}"),
                         tags$style(type='text/css', "* { font-family: 'lato light' !important; }" ),
                         tags$style("#state {font-size:20px;}"),
                         tags$style("#date {font-size:20px;}")),
                 sidebarLayout(
                   sidebarPanel(
                     HTML('<center><a href="https://www.ime.usp.br/"><img src="logoblack.png" height="100" width="300"></center></a>'),
                     width = 3),
                   mainPanel(h2("Previsão COVID-19 para o Estado de São Paulo",align = "center"),
                             fluidRow(column(7,sliderInput("date",
                                                          h4("Escolha a data para previsão"),
                                                          min = as.Date(dmin,"%d/%m/%y"),
                                                          max = as.Date(dmax,"%d/%m/%y"),
                                                          value=as.Date(dmax),
                                                          timeFormat="%d/%m/%y",width = '90%')),
                                      column(5,div(style="display:inline-block;text-align:center;float:right;",
                                                   myDownloadButton("report", "Gerar Relatório Executivo para a data escolhida"))),
                                      tags$style(type='text/css', ".irs-grid-text { font-size: 10pt; }"),
                                      tags$style(type='text/css', "#report { width:130%; margin-top: 60px; align:center}")))),
                   fluidRow(column(8,HTML(paste('<center><img src="SP_EPcurve_predicted_',pos,
                                                '.png" height="625" width="937"></center>',sep = ""))),
                            column(4,uiOutput("resEstado"),style = "background-color:#1a1a1a;")),
               br(),
               fluidRow(column(6,h3("Previsão de Mortes por DRS",align = "center")),
                        column(6,h3("Previsão de Casos por DRS",align = "center"))),
               fluidRow(column(6,rHandsontableOutput("mortes_DRS")),
                        column(6,rHandsontableOutput("casos_DRS")),
                        tags$style(type="text/css", "#tabela th {font-weight:bold;}")),
               br(),
               fluidRow(column(6,h3("Previsão de Mortes por Município",align = "center")),
                        column(6,h3("Previsão de Casos por Município",align = "center"))),
               fluidRow(column(6,rHandsontableOutput("mortes_city")),
                        column(6,rHandsontableOutput("casos_city")),
                        tags$style(type="text/css", "#tabela th {font-weight:bold;}")),
                       br(),
               p("*Os valores nas tabelas dizem respeito à data escolhida. O modelo projeta casos e mortes para todas as DRSs e apenas para Municípios com mais de 100 casos confirmados quando o modelo foi ajustado."),
                             br(),
               fluidRow(column(3,myDownloadButton("videoMortesSP","Baixar animação com a previsão de mortes")),
                        column(3,myDownloadButton("videoCasosSP","Baixar animação com a previsão de casos"))),
               br(),
               tags$p("Aplicação desenvolvida por ", 
                      tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
                      tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
                      tags$a(href="mailto:dmarcondes@ime.usp.br","dmarcondes@ime.usp.br."),align="center"),
               br())),
    
               tabPanel("Diretoria Regional de Saúde",fluidPage(
                 tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"),tags$style("#tbl {white-space: nowrap;}"),
                           tags$style(type='text/css', "* { font-family: 'lato light' !important; }" )),
                 sidebarLayout(
                   sidebarPanel(
                     HTML('<center><a href="https://www.ime.usp.br/"><img src="logoblack.png" height="100" width="300"></center></a>'),
                     width = 3),
                   mainPanel(uiOutput("titleDRS"),
                             fluidRow(column(3,selectizeInput(inputId = "DRS",label = h4("DRS"),
                                                             choices = l_drs,multiple = F,selected = "Grande São Paulo")),
                                      column(5,sliderInput("dateDRS",
                                                           h4("Escolha a data para previsão"),
                                                           min = as.Date(dmin,"%d/%m/%y"),
                                                           max = as.Date(dmax,"%d/%m/%y"),
                                                           value=as.Date(dmax),
                                                           timeFormat="%d/%m/%y",width = '90%')),
                                      column(4,div(style="display:inline-block;text-align:center;float:right;",
                                                   myDownloadButton("reportDRS", 
                                                                    "Gerar Relatório Executivo para a data e DRS escolhida"))),
                                      tags$style(type='text/css', ".irs-grid-text { font-size: 10pt; }"),
                                      tags$style(type='text/css', "#reportDRS { width:130%; margin-top: 60px;}")
                                      ))),
                 fluidRow(column(8,uiOutput("EPDRS")),
                          column(4,uiOutput("resDRS"),style = "background-color:#1a1a1a;")),
                 br(),
                 fluidRow(column(6,h3("Previsão de Mortes por Município",align = "center")),
                          column(6,h3("Previsão de Casos por Município",align = "center"))),
                 fluidRow(column(6,rHandsontableOutput("mortes_city_DRS")),
                          column(6,rHandsontableOutput("casos_city_DRS")),
                          tags$style(type="text/css", "#tabela th {font-weight:bold;}")),
                 br(),
                 p("*Os valores nas tabelas dizem respeito à data escolhida. O modelo projeta casos e mortes para todas as DRSs e apenas para Municípios com mais de 100 casos confirmados quando o modelo foi ajustado."),
                 br(),
                 fluidRow(column(3,myDownloadButton("videoMortesDRS","Baixar animação com a previsão de mortes")),
                          column(3,myDownloadButton("videoCasosDRS","Baixar animação com a previsão de casos"))),
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
                     HTML('<center><a href="https://www.ime.usp.br/"><img src="logoblack.png" height="100" width="300"></center></a>'),
                     width = 3),
                   mainPanel(uiOutput("titleCity"),
                             fluidRow(column(5,selectizeInput(inputId = "city",label = h4("Município"),
                                                              choices = l_city,multiple = F,selected = "SÃO PAULO")),
                                      column(7,sliderInput("dateCity",
                                                           h4("Escolha a data para previsão"),
                                                           min = as.Date(dmin,"%d/%m/%y"),
                                                           max = as.Date(dmax,"%d/%m/%y"),
                                                           value=as.Date(dmax),
                                                           timeFormat="%d/%m/%y",width = '90%')),
                                      tags$style(type='text/css', ".irs-grid-text { font-size: 10pt; }")))),
                 fluidRow(column(8,uiOutput("EPCity")),
                          column(4,uiOutput("resCity"),style = "background-color:#1a1a1a;")),
                 br(),
                 p("*O modelo projeta casos e mortes para todas as DRSs e apenas para Municípios com mais de 100 casos confirmados quando o modelo foi ajustado."),
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
                     HTML('<center><a href="https://www.ime.usp.br/"><img src="logoblack.png" height="100" width="300"></center></a>'),
                     width = 3),
                   mainPanel(uiOutput("titleAD"),
                             fluidRow(column(3,selectizeInput(inputId = "DRSad",label = h4("DRS"),
                                                              choices = l_drs2,multiple = F,selected = "Grande São Paulo")),
                                      column(9,uiOutput("textGOF"))))),
                 fluidRow(column(6,uiOutput("aderiMortes")),column(6,uiOutput("aderiCasos"))),
                 br(),
                 p(paste("*A aderência foi testada apenas para as DRSs com mais de 500 casos e 50 mortes na semaa terminando em ",
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
                 sidebarLayout(
                   sidebarPanel(
                     HTML('<center><a href="https://www.ime.usp.br/"><img src="logoblack.png" height="100" width="300"></center></a>'),
                     HTML(paste("<h5 align='justify'> As projeções foram realizadas através da simulação de",nmodels,"Modelos SEIR Metapopulacionais, em que a mobilidade entre as cidades do Estado, mensurada por dados anônimos de geolocalização de celulares, é acoplada à um modelo epidemiológico de compartimentos. Os modelos foram escolhidos dentre dezenas de milhares de modelos candidatos, pois se aderiram bem aos dados observados. A aderência foi mensurada pela diferença entre o número de mortes e o número de casos confirmados nas DRSs com mais de 50 mortes e 500 casos, respectivamente, durante o período de uma semana terminando em",format(dmin,"%d/%m/%Y"),"</h5>")),
                     HTML(paste("<h5 align='justify'> As projeções desses modelos, que leva em consideração a transmissão do vírus entre as cidades, só pode ser realizada com maior confiança pelo período de um mês e supõe que a evolução da doença se manterá da mesma forma que na semana terminando em ",format(dmin,"%d/%m/%Y"),". Além disso, também podemos estimar através deles quando será o pico de mortes pela doença (dia com mais mortes). As bandas superior e inferior das estimativas dizem respeito ao pior e melhor caso simulados pelos modelos escolhidos. </h5>",sep = "")),
                     br(),
                     HTML("<h5 align='left'> Diego Marcondes (<a href='mailto:dmarcondes@ime.usp.br'>dmarcondes@ime.usp.br</a>)</h5>"),
                     HTML("<h5 align='left'> Claúdia Peixoto (<a href='mailto:claudiap@ime.usp.br'>claudiap@ime.usp.br</a>)</h5>"),
                     HTML("<h5 align='left'> Sergio Oliva (<a href='mailto:smo@ime.usp.br'>smo@ime.usp.br</a>)</h5>"),
                     HTML("<h5 align='left'> Pedro Peixoto (<a href='mailto:ppeixoto@usp.br'>ppeixoto@usp.br</a>)</h5>"),
                     h5("(A ADICIONAR MAIS INFORMAÇÕES QUANDO O RELATÓRIO ESTIVER PRONTO)"),
                     br(),
                     HTML("Mais informações em  <a href='https://www.ime.usp.br/~pedrosp/covid19'>www.ime.usp.br/~pedrosp/covid19</a>."),
                     width = 12),
                   mainPanel()),
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
         templogo <- file.path(tempdir(), "logoblack.png")
         file.copy("./www/logoblack.png", templogo, overwrite = TRUE)
         templogoUSP <- file.path(tempdir(), "logo_USP.png")
         file.copy("./www/logo_USP.png", templogoUSP, overwrite = TRUE)
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
      t1 <- paste("<h3 align='center',face='bold'> PREVISÃO PARA ",format(input$date,"%d/%m/%Y"),
                  " </h3> <h4 align='center'> <br> <span style='color: yellow;'>Casos Confirmados: ",
                  format(round(sum(tmpC$Mediana)),decimal.mark = ",",big.mark = "."),
                  "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Melhor Cenário: ",
                  format(round(sum(tmpC$Infimo)),decimal.mark = ",",big.mark = "."),
                  "<br> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Pior Cenário: ",
                  format(round(sum(tmpC$Sup)),decimal.mark = ",",big.mark = "."),"</span> <br><br> <span style='color: red;'> Mortes Confirmadas: ",
                  format(round(sum(tmpM$Mediana)),decimal.mark = ",",big.mark = "."),"&nbsp;&nbsp;&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Melhor Cenário: ",
                  format(round(sum(tmpM$Infimo)),decimal.mark = ",",big.mark = "."),
                  "<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Pior Cenário: ",
                  format(round(sum(tmpM$Sup)),decimal.mark = ",",big.mark = "."),
                  "</span> </h4> <br> <h5 align='center'> As previsões são realizadas através de Modelos SEIR Metapopulação com Mobilidade supondo que a evolução da doença se manterá da mesma forma que na semana terminando em ",
                  format(dmin,"%d/%m/%Y"),"</h5> <p align='center'> Desenvolvido por pesquisadores do IME-USP </p> <p align='center'> Atualizado em ",
                  format(dmin,"%d/%m/%Y")," </p>",sep = "")
      HTML(t1)
    }
  })
  
  observe({
    if(!is.null(input$date)){
      tmpC_DRS <- cases_DRS %>% filter(Date == input$date)
      tmpM_DRS <- deaths_DRS %>% filter(Date == input$date)
      tmpC_city <- cases_city %>% filter(Date == input$date)
      tmpM_city <- deaths_city %>% filter(Date == input$date)
      
      tmpM_DRS <- tmpM_DRS[order(tmpM_DRS$Mediana,decreasing = T),c(2:5)]
      names(tmpM_DRS) <- c("DRS","Mínimo","Mediana","Máximo")
      tmpM_DRS[,2:4] <- sapply(tmpM_DRS[,2:4], FUN=function(x) prettyNum(round(x),decimal.mark = ",",big.mark="."))
      
      tmpC_DRS <- tmpC_DRS[order(tmpC_DRS$Mediana,decreasing = T),c(2:5)]
      names(tmpC_DRS) <- c("DRS","Mínimo","Mediana","Máximo")
      tmpC_DRS[,2:4] <- sapply(tmpC_DRS[,2:4], FUN=function(x) prettyNum(round(x),decimal.mark = ",",big.mark="."))
      
      tmpM_city <- tmpM_city[order(tmpM_city$Mediana,decreasing = T),-1]
      names(tmpM_city) <- c("Município","Mínimo","Mediana","Máximo")
      tmpM_city[,2:4] <- sapply(tmpM_city[,2:4], FUN=function(x) prettyNum(round(x),decimal.mark = ",",big.mark="."))
      
      tmpC_city <- tmpC_city[order(tmpC_city$Mediana,decreasing = T),-1]
      names(tmpC_city) <- c("Município","Mínimo","Mediana","Máximo")
      tmpC_city[,2:4] <- sapply(tmpC_city[,2:4], FUN=function(x) prettyNum(round(x),decimal.mark = ",",big.mark="."))
      
      output$mortes_DRS <- renderRHandsontable({
        rhandsontable(data = tmpM_DRS,stretchH = "all",rowHeaders = NULL,digits = 0) %>%
          hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = 'black';
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   td.style.color = 'white';}") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_col(col = c(2,3,4),valign = "htCenter")
        
      })
      
      output$casos_DRS <- renderRHandsontable({
        rhandsontable(data = tmpC_DRS,stretchH = "all",rowHeaders = NULL,digits = 0) %>%
          hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = 'black';
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   td.style.color = 'white';}") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_col(col = c(2,3,4),valign = "htCenter")
      })
      
      output$mortes_city <- renderRHandsontable({
        rhandsontable(data = tmpM_city,stretchH = "all",rowHeaders = NULL,digits = 0) %>%
          hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = 'black';
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   td.style.color = 'white';}") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_col(col = c(2,3,4),valign = "htCenter")
        
      })
      
      output$casos_city <- renderRHandsontable({
        rhandsontable(data = tmpC_city,stretchH = "all",rowHeaders = NULL,digits = 0) %>%
          hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = 'black';
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   td.style.color = 'white';}") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_col(col = c(2,3,4),valign = "htCenter")
      })
    }
  })
  
  output$titleDRS <- renderUI({
    HTML(paste("<h2 align='center'> Previsão COVID-19 para a DRS",input$DRS,"</h2>"))
  })
  
  output$EPDRS <- renderUI({
    #HTML(paste('<center><img src="DRS_',gsub(" ","",input$DRS),'_EPCurve_',pos,'".png" height="625" width="937"></center>',sep = ""))
    img(src=paste('DRS_',gsub(" ","",input$DRS),'_EPCurve_',pos,'.png',sep = ""),height = 625,width = 937,align = "center")
  })
  
  output$resDRS <- renderUI({
    tmpC <- cases_DRS %>% filter(Date == ymd(input$dateDRS) & DRS == input$DRS)
    tmpM <- deaths_DRS %>% filter(Date == ymd(input$dateDRS) & DRS == input$DRS)
    if(input$DRS == "Grande São Paulo")
      t1 <- paste("<h3 align='center',face='bold'> PREVISÃO PARA ",format(input$dateDRS,"%d/%m/%Y"),
                  " <br> DRS ",input$DRS,
                  "</h3> <p align='center'> (Excluindo a Cidade de São Paulo) </p> <h4 align='center'> <br> <span style='color: yellow;'>Casos Confirmados: ",
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
                  "</span> </h4> <br> <h5 align='center'> As previsões são realizadas através de Modelos SEIR Metapopulação com Mobilidade supondo que a evolução da doença se manterá da mesma forma que na semana terminando em ",
                 format(dmin,"%d/%m/%Y"),"</h5> <p align='center'> Desenvolvido por pesquisadores do IME-USP </p> <p align='center'> Atualizado em ",
                  format(dmin,"%d/%m/%Y")," </p>",sep = "")
    else
      t1 <- paste("<h3 align='center',face='bold'> PREVISÃO PARA ",format(input$dateDRS,"%d/%m/%Y"),
                  " <br> DRS ",input$DRS,"</h3> <h4 align='center'> <br> <span style='color: yellow;'>Casos Confirmados: ",
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
                  "</span> </h4> <br> <h5 align='center'> As previsões são realizadas através de Modelos SEIR Metapopulação com Mobilidade supondo que a evolução da doença se manterá da mesma forma que na semana terminando em ",
                  format(dmin,"%d/%m/%Y"),"</h5> <p align='center'> Desenvolvido por pesquisadores do IME-USP </p> <p align='center'> Atualizado em ",
                  format(dmin,"%d/%m/%Y")," </p>",sep = "")
    HTML(t1)
  })
  
  observe({
    if(!is.null(input$dateDRS)){
      tmpC_city <- cases_city %>% filter(Date == input$dateDRS & Municipio %in% drs$Municipio[drs$Regiao == input$DRS])
      tmpM_city <- deaths_city %>% filter(Date == input$dateDRS & Municipio %in% drs$Municipio[drs$Regiao == input$DRS])
      
      tmpM_city <- tmpM_city[order(tmpM_city$Mediana,decreasing = T),-1]
      names(tmpM_city) <- c("Município","Mínimo","Mediana","Máximo")
      tmpM_city[,2:4] <- sapply(tmpM_city[,2:4], FUN=function(x) prettyNum(round(x),decimal.mark = ",",big.mark="."))
      
      tmpC_city <- tmpC_city[order(tmpC_city$Mediana,decreasing = T),-1]
      names(tmpC_city) <- c("Município","Mínimo","Mediana","Máximo")
      tmpC_city[,2:4] <- sapply(tmpC_city[,2:4], FUN=function(x) prettyNum(round(x),decimal.mark = ",",big.mark="."))
      
      output$mortes_city_DRS <- renderRHandsontable({
        rhandsontable(data = tmpM_city,stretchH = "all",rowHeaders = NULL,digits = 0) %>%
          hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = 'black';
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   td.style.color = 'white';}") %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
          hot_col(col = c(2,3,4),valign = "htCenter")
        
      })
      
      output$casos_city_DRS <- renderRHandsontable({
        rhandsontable(data = tmpC_city,stretchH = "all",rowHeaders = NULL,digits = 0) %>%
          hot_cols(columnSorting = TRUE,renderer = "function(instance, td) {td.style.background = 'black';
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   td.style.color = 'white';}") %>%
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
        templogo <- file.path(tempdir(), "logoblack.png")
        file.copy("./www/logoblack.png", templogo, overwrite = TRUE)
        templogoUSP <- file.path(tempdir(), "logo_USP.png")
        file.copy("./www/logo_USP.png", templogoUSP, overwrite = TRUE)
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
    HTML(paste("<h2 align='center'> Previsão COVID-19 para",input$city,"-SP </h2>"))
  })
  
  output$EPCity <- renderUI({
    img(src=paste(gsub(" ","",input$city),'_EPCurve_',pos,'.png',sep = ""),height = 625,width = 937,align = "center")
  })
  
  output$resCity <- renderUI({
    tmpC <- cases_city %>% filter(Date == ymd(input$dateCity) & Municipio == input$city)
    tmpM <- deaths_city %>% filter(Date == ymd(input$dateCity) & Municipio == input$city)
    t1 <- paste("<h3 align='center',face='bold'> PREVISÃO PARA ",format(input$dateCity,"%d/%m/%Y"),
                " <br> ",input$city," - SP</h3> <h4 align='center'> <br> <span style='color: yellow;'>Casos Confirmados: ",
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
                "</span> </h4> <br> <h5 align='center'> As previsões são realizadas através de Modelos SEIR Metapopulação com Mobilidade supondo que a evolução da doença se manterá da mesma forma que na semana terminando em ",
                format(dmin,"%d/%m/%Y"),"</h5> <p align='center'> Desenvolvido por pesquisadores do IME-USP </p> <p align='center'> Atualizado em ",
                format(dmin,"%d/%m/%Y")," </p>",sep = "")
    HTML(t1)
  })
  
  output$titleAD <- renderUI({
    if(input$DRSad != "SP")
      HTML(paste("<h2 align='center'> Aderência para a DRS",input$DRSad,"</h2>"))
    else
      HTML(paste("<h2 align='center'> Aderência para o Estado de São Paulo </h2>"))
  })
  
  output$textGOF <- renderUI({
    if(input$DRSad != "SP")
      HTML(paste("<h5 align='center'> As figuras abaixo mostram como os modelos (linhas tracejadas e bandas de confiança) se aderiram aos dados observados (linha sólida) na DRS",input$DRSad,"na semana terminando em",format(dmin,"%d/%m/%Y"),"</h5>"))
    else
      HTML(paste("<h5 align='center'> As figuras abaixo mostram como os modelos (linhas tracejadas e bandas de confiança) se aderiram aos dados observados (linha sólida) no Estado de São Paulo na semana terminando em",format(dmin,"%d/%m/%Y"),"</h5>"))
  })
  
  output$aderiCasos <- renderUI({
    if(input$DRSad != "SP")
      img(src=paste(gsub(" ","",input$DRSad),'_casos.png',sep = ""),height = 468,width = 702,align = "center")
    else
      img(src=paste(gsub(" ","",input$DRSad),'_casos_',pos,'.png',sep = ""),height = 468,width = 702,align = "center")
  })
  
  output$aderiMortes <- renderUI({
    if(input$DRSad != "SP")
      img(src=paste(gsub(" ","",input$DRSad),'_mortes.png',sep = ""),height = 468,width = 702,align = "center")
    else
      img(src=paste(gsub(" ","",input$DRSad),'_mortes_',pos,'.png',sep = ""),height = 468,width = 702,align = "center")
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


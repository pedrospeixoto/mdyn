#######################################
######Notificação COVID-19 Shiny#######
######Diego Marcondes           #######
######dmarcondes@ime.usp.br     #######
#######################################

#Pacotes
library(shiny)
library(shinythemes)
library(shinyjs)
library(rgdal)
library(leaflet)
library(shinycssloaders)
library(leaflet.extras)
library(shinyWidgets)
library(htmlwidgets)
library(tidyverse)
library(lubridate)
library(rhandsontable)
options(java.parameters = "-Xss2560k")

#####Dados#####
shp <- readRDS(file = "./www/shp_brazil.rds")
shp_estados <- readRDS(file = "./www/shape_estados.rds")
file <- gzcon(url("https://data.brasil.io/dataset/covid19/caso_full.csv.gz"))
txt <- readLines(file)
obs <- read.csv(textConnection(txt)) #Get data
obs$date <- ymd(obs$date)
dataD <- max(obs$date)
dataD <- paste(day(dataD),"/",sprintf("%02d", month(dataD)),"/",year(dataD),sep = "")
obs <- obs %>% filter(date == max(date) & city != "" & city != "Importados/Indefinidos") %>% 
  select(city,state,city_ibge_code,last_available_confirmed,last_available_deaths)


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
    navbarPage(theme = shinytheme("yeti"),"Propensão Relativa à Subnotificação COVID-19",windowTitle = "Propensão Subnotificação COVID-19",
               tabPanel("Página Inicial",fluidPage(
               tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"),tags$style("#tbl {white-space: nowrap;}"),
                         tags$style("#state {font-size:20px;}"),
                         tags$style("#date {font-size:20px;}")),
                 sidebarLayout(
                   sidebarPanel(
                     HTML('<center><img src="logo.png" height="100" width="300"></center>'),
                     width = 3),
                   mainPanel(
                     #titlePanel(h2("Escolha o estado")),
                     fluidRow(column(4,selectizeInput(inputId = "state",label = h3("Selecione o Estado"),
                                                          choices = list('Acre' = 'AC','Alagoas' = 'AL','Amapá' = 'AP',
                                                                         'Amazonas' = 'AM',
                                                                         'Bahia' = 'BA','Ceará' = 'CE','Distrito Federal' = 'DF',
                                                                         'Espírito Santo' = 'ES','Goiás' = 'GO','Maranhão' = 'MA',
                                                                         'Mato Grosso' = 'MT','Mato Grosso do Sul' = 'MS',
                                                                         'Minas Gerais' = 'MG','Pará' = 'PA','Paraíba' = 'PB','Paraná' = 'PR',
                                                                         'Pernambuco' = 'PE','Piauí' = 'PI','Rio de Janeiro' = 'RJ',
                                                                         'Rio Grande do Norte' = 'RN','Rio Grande do Sul' = 'RS','Rondônia' = 'RO',
                                                                         'Roraima' = 'RR','Santa Catarina' = 'SC','São Paulo' = 'SP',
                                                                         'Sergipe' = 'SE','Tocantins' = 'TO'),multiple = F,selected = 'SP'))),
                     fluidRow(column(4,column(4,actionButton("do", "Processar"))))))),
                     fluidRow(addSpinner(leafletOutput(outputId = "map",height=650),spin = "circle", color = "#E41A1C")),
                     br(),
                     br(),
               fluidRow(column(12,align = "center",h2("Municípios Relativamente Propensos à Subnotificação COVID-19"))),
               fluidRow(column(4,h5("")),
                        column(4,rHandsontableOutput("tabela")),
                        tags$style(type="text/css", "#tabela th {font-weight:bold;}")),
               br(),
               br(),
               fluidRow(
                 column(2,h3("Nome do Arquivo")),
                 column(6,uiOutput("filenames"))),
               fluidRow(
                 column(1,align = "center","  ",downloadButton('download', 'Download'))),
               br(),
               br(),
               withMathJax(HTML("<h4><strong>Propensão Relativa à Subnotificação COVID-19</strong> <br> <br>  <p align='justify'> A propensão relativa à subnotificação de um Município é definida como $$PR = \\frac{\\text{Taxa de morte por COVID-19 no Município}}{\\text{Taxa de morte por COVID-19 no Estado do Município}} - 1$$ em que as taxa de morte são definidas como a proporção entre o número de mortes confirmadas por COVID-19 e o número de casos confirmados de COVID-19 no Município e no Estado, respectivamente. No mapa acima a propensão só é calculada para Municípios com pelo menos duas mortes confirmadas por COVID-19. A propensão representa o <i> Lift </i> menos um entre as variáveis óbito por COVID-19 e Município quando calculado considerando apenas os indivíduos infectados por COVID-19 contabilizados nas estatísticas oficiais. Para mais informações sobre o <i> Lift</i> ver <a href='https://arxiv.org/abs/1901.10012'>[1]</a> e <a href='https://www.mdpi.com/1099-4300/20/2/97'>[2]</a>.</p>")),
               withMathJax(HTML("<h4><strong>Como interpretar</strong> <br> <br> <p align='justify'> Se a <strong>PR</strong> for maior do que zero, significa que a taxa de morte no Município está acima da taxa de morte no Estado, e concluímos que esse Município é mais propenso a ter mortes por COVID-19 relativo ao seu Estado. Essa propensão se da majoritariamente pelos seguintes motivos:</p>")),
               HTML("<p align='justify'> <strong>(i)</strong> A população do Município possui características distintas da população do Estado como um todo, no sentido de ter uma parcela maior no grupo de risco em relação à doença, o que faz com que a taxa de mortalidade do Município seja maior.</p>"),
               HTML("<p align='justify'> <strong>(ii)</strong> O sitema de saúde do Município entrou em colapso e não está mais prestando os devidos cuidados aos pacientes com COVID-19, o que causa uma taxa de morte maior.</p>"),
               HTML("<p align='justify'> <strong>(iii)</strong> Os casos no Município estão com uma subnotificação acima da média do Estado, o que faz com que menos casos sejam contabilizados para o cálculo da taxa de morte, fazendo com que ela seja superestimada.</p>"),
               HTML("Tendo em vista que os sistemas de saúde ainda não entraram em colapso, e atentando ás particularidades sociodemográficas de cada Município, <strong>podemos considerar a propensão relativa à subnotificação como uma medida da subnotificação de casos no Município em relação à subnotifcação no Estado</strong>. Se a <strong>PR</strong> do Município for maior do que um, temos evidências de que a subnotificação de casos nele é maior do que no seu Estado e portanto ele está, relativamente ao seu Estado, mais propenso à subnotificação, entendida aqui como uma maior prevalência de casos do que representada pelos dados oficiais."),
               br(),
               br(),
               HTML("Os dados utilizados aqui foram retirados do site <a href='https://brasil.io/dataset/covid19/caso/'>Brasil.io</a>."),
               br(),
               br(),
               tags$p("Aplicação desenvolvida por ", 
                      tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
                      tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
                      tags$a(href="mailto:dmarcondes@ime.usp.br","dmarcondes@ime.usp.br."),align="center"),
               br())))
  
  #Map variables
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    left: 0%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 24px;
  }"))
  
  ime <- tags$div(
    HTML('<a href="https://www.ime.usp.br/"> <img border="0" alt="ImageTitle" src="IME_simplificado.png" width="60" height="100"> </a>')
  )
  usp <- tags$div(
    HTML('<a href="https://www.usp.br/"> <img border="0" alt="ImageTitle" src="logo_USP.jpeg" width="60" height="35"> </a>')
  )
  fapesp <- tags$div(
    HTML('<a href="http://www.fapesp.br/en/"> <img border="0" alt="ImageTitle" src="FAPESP.png" width="60" height="35"> </a>')
  )
  
  title <- tags$div(tag.map.title, HTML(paste("Propensão Relativa à Subnotificação COVID-19 IME - USP",dataD)))
  
  #Filenames
  output$filenames <- renderUI({
    if(is.null(input$state))
      textInput("filename","",value = "propensao_risco")
    else
      textInput("filename","",value = paste("propensao_risco_",input$state,sep = ""))
  })
  
  #Map
  output$map <- renderLeaflet({
     leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
       addTiles(urlTemplate = "", attribution = "©IME - USP. Design: Diego Marcondes.") %>%
       addControl(title, position = "topright", className="map-title") %>%
       addEasyButton(easyButton(
         icon="fa-crosshairs", title="Locate Me",
         onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
       addControl(ime, position = "bottomleft") %>%
       addControl(usp, position = "bottomleft") %>%
       addControl(fapesp, position = "bottomleft") %>%
       addPolygons(data = shp_estados,weight = 3,fill = F,color = "black")
   })
  
  #Input map
  observeEvent(input$do,{
    if(!is.null(input$state)){
      tmp <- obs %>% filter(state == input$state) %>% droplevels()
      tmp$rate <- ifelse(tmp$last_available_confirmed > 0,tmp$last_available_deaths/tmp$last_available_confirmed,0)
      tmp$rate[tmp$last_available_deaths < 2] <- NA
      rate <- sum(tmp$last_available_deaths)/sum(tmp$last_available_confirmed)
      tmp$lift <- tmp$rate/rate
      tab <- tmp %>% filter(lift > 1) %>% select(city,lift)
      tab$lift <- tab$lift - 1
      names(tab) <- c("Município","Propensão Relativa")
      tab <- tab[order(tab[,2],decreasing = T),]
      tmp$lift[tmp$lift < 1] <- 1 
      tmp$lift[tmp$lift > 2] <- 2 
      
      output$tabela <- renderRHandsontable({
        rhandsontable(data = tab,stretchH = "all",rowHeaders = NULL) %>%
          hot_cols(columnSorting = TRUE)
      })
    
      #Shapes
      shp_tmp <- shp[shp$UF == input$state,]
      shp_tmp <- merge(shp_tmp,tmp,by.x = "CD_GEOCMU",by.y = "city_ibge_code",all = T)
      for(j in 1:ncol(shp_tmp))
        if(is.numeric(shp_tmp[[names(shp_tmp)[j]]]))
          shp_tmp[is.na(shp_tmp[[names(shp_tmp)[j]]]),j] <- 0
      nomes <- vector()
      nomes[shp_tmp$lift == 1] <- "Propensão < 0"
      nomes[shp_tmp$last_available_deaths < 2] <- "Sem dados suficientes"
      nomes[shp_tmp$lift == 2] <- "Propensão > 1"
      nomes[is.na(nomes)] <- paste("Propensão:",round(shp_tmp$lift[is.na(nomes)]-1,2))
      shp_tmp$lift <- shp_tmp$lift - 1
      shp_tmp$lift[shp_tmp$last_available_deaths < 2] <- -0.01
  
      # Make vector of colors for values smaller than 0 (20 colors)
      rc <- c("white",colorRampPalette(colors = c("orange","red"), space = "Lab")(10))
       
      #Palete
      mypal <- colorNumeric(palette = rc, domain = c(shp_tmp$lift,1.05))
      cor <- vector()
      cor <- ifelse(nomes != "Sem dados suficientes","black","gray")
      w <- ifelse(nomes != "Sem dados suficientes",1,0.5)
      
       
      output$map <- renderLeaflet({
         leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
           addTiles(urlTemplate = "", attribution = "©IME - USP. Design: Diego Marcondes.") %>%
           addControl(title, position = "topright", className="map-title") %>%
           addEasyButton(easyButton(
            icon="fa-crosshairs", title="Locate Me",
            onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
          addControl(ime, position = "bottomleft") %>%
         addControl(usp, position = "bottomleft") %>%
         addControl(fapesp, position = "bottomleft") %>%
           addPolygons(data = shp_tmp,weight = w,fillColor = mypal(shp_tmp$lift),fillOpacity = 0.75,color = cor,
                     label = shp_tmp$Nome_Munic,
                     labelOptions = labelOptions(textsize = "15px"),
                     popup = nomes) %>%
         addLegend(position = "bottomright", pal = mypal,values = c(shp_tmp$lift,1.05),
                   title = "Propensão",opacity = 0.8)    
       })
    }
  })
   
   #Download
   output$download <- downloadHandler(
     filename = function() paste(input$filename,".csv",sep = ""),
     content = function(file) {
       withProgress(message = 'Salvando dados...',value = 0,{
         tmp <- obs %>% filter(state == input$state) %>% droplevels()
         tmp$rate <- ifelse(tmp$last_available_confirmed > 0,tmp$last_available_deaths/tmp$last_available_confirmed,0)
         tmp$rate[tmp$last_available_deaths < 2] <- NA
         rate <- sum(tmp$last_available_deaths)/sum(tmp$last_available_confirmed)
         tmp$lift <- tmp$rate/rate
         tab <- tmp %>% select(city,lift,last_available_deaths)
         tab$lift <- round(tab$lift - 1,2)
         names(tab) <- c("Município","Propensão Relativa","Número de Mortes")
         tab <- tab[order(tab[,2],decreasing = T),]
         tab <- na.omit(tab)
       incProgress(1/2, detail = "Só mais um instante...")
       write.csv(tab,file = file,row.names = F)
       incProgress(1, detail = "Feito!")
       })
     })   
}

# Run the application 
shinyApp(ui = ui, server = server)


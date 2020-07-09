##################################
######Mobility Shiny app   #######
######Diego Marcondes      #######
######dmarcondes@ime.usp.br#######
##################################

#Pacotes
library(shiny)
library(shinythemes)
library(shinyjs)
#library(shinyURL)
library(rgdal)
library(leaflet)
library(shinycssloaders)
library(leaflet.extras)
library(shinyWidgets)
library(htmlwidgets)
library(tidyverse)
options(java.parameters = "-Xss2560k")

#####OPEN DATE#####
shp <- readRDS(file = "./www/shp_brazil.rds")
shp_estados <- readRDS(file = "./www/shape_estados.rds")
dates <- readRDS(file = "./www/dates.rds")

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
    navbarPage(theme = shinytheme("yeti"),"Mobilidade Municípios - BR",
               tabPanel("Mobilidade",fluidPage(
               tags$head(tags$link(rel = "shortcut icon", href = "favicon.png"),tags$style("#tbl {white-space: nowrap;}"),
                         tags$style("#state {font-size:20px;}"),
                         tags$style("#date {font-size:20px;}")),
                 sidebarLayout(
                   sidebarPanel(
                     HTML('<center><img src="logo.png" height="100" width="300"></center>'),
                     width = 3),
                   mainPanel(
                     titlePanel(h2("Escolhas os parâmetros para construir o mapa")),
                     fluidRow(column(4,selectizeInput(inputId = "state",label = h3("Estados"),
                                                          choices = list('Todos' = 'all','Acre' = 'AC','Alagoas' = 'AL','Amapá' = 'AP',
                                                                         'Amazonas' = 'AM',
                                                                         'Bahia' = 'BA','Ceará' = 'CE','Distrito Federal' = 'DF',
                                                                         'Espírito Santo' = 'ES','Goiás' = 'GO','Maranhão' = 'MA',
                                                                         'Mato Grosso' = 'MT','Mato Grosso do Sul' = 'MS',
                                                                         'Minas Gerais' = 'MG','Pará' = 'PA','Paraíba' = 'PB','Paraná' = 'PR',
                                                                         'Pernambuco' = 'PE','Piauí' = 'PI','Rio de Janeiro' = 'RJ',
                                                                         'Rio Grande do Norte' = 'RN','Rio Grande do Sul' = 'RS','Rondônia' = 'RO',
                                                                         'Roraima' = 'RR','Santa Catarina' = 'SC','São Paulo' = 'SP',
                                                                         'Sergipe' = 'SE','Tocantins' = 'TO'),multiple = T)),
                              column(4,dateInput("date", h3("Data"), value = dates$max,language = "pt-BR",
                                                 min = dates$min,max = dates$max)),
                              column(4,radioButtons("type", label = h3("Tipo"),
                                                    choices = list("Com entrada e saída de todas as cidades" = F, 
                                                                   "Escolher cidades com entrada e saída" = T), 
                                                    selected = F))),
                     fluidRow(column(4,uiOutput("cidades")),
                              column(4,uiOutput("cidades_sai")),
                              column(4,actionButton("process", "Processar"))),
                     fluidRow(column(12,p("Escolha os parâmetros e clique em Processar. Dependendo da sua escolha, o processamento pode demorar",
                                          style = "color:red")))))),
                     fluidRow(addSpinner(leafletOutput(outputId = "map",height=650),spin = "circle", color = "#E41A1C")),
                     br(),
                     br(),
                     fluidRow(
                       column(2,h3("Nome do Arquivo")),
                       column(6,textInput("filename","",value = "Mobilidade"))),
                     fluidRow(
                       column(1,align = "center","  ",downloadButton('download', 'Download'))),
               br(),
               br(),
               HTML("<h4><strong>Sobre os dados</strong> <br> <br>  <p align='justify'> Os dados apresentados nestas análises são obtidos de forma anonimizada pela empresa colaboradora In Loco com base em usos de certos aplicativos de celulares que capturam a geolocalização de usuários. Os dados são constituidos de pares de posições consecutivas dos usuários (origem-destino), indicando multiplas viagens realizadas por milhões de pessoas presentes na base de dados. É importante ressaltar que estes dados não representam uma totalidade da população, mas servem para obter um indicador do padrão de mobilidade da população.</p>"),
               HTML("<h4><strong>Sobre as análises</strong> <br> <br> <p align='justify'> Para cada par de municípios do país indicamos com caminhos orientados (retas) as viagens registradas saindo de um, e entrando no outro município. A intensidade do fluxo diário de pessoas (número de viagens) é indicado pela graduação da cor e a espessura da linha conectando dois munícipios. Para obter mais informações sobre uma reta, basta clicar nela.</p>"),
               HTML("<h4><strong>Por que isso é importante?</strong>"),
               br(),
               br(),
               HTML("<p align='justify'> <strong>(i)</strong> Diversas ações vêm sendo tomadas pelo governo no âmbito municipal, estadual e federal no sentido de reduzir a mobilidade social. O indice de isolamento social (veja em <a href='https://www.ime.usp.br/~pedrosp/covid19'>www.ime.usp.br/~pedrosp/covid19</a> o índice para mais de 4mil municípios do país) mede a mobilidade dentro de um município, mas não capta a movimentação entre cidades. Com essas análises esperamos que fique mais claro o efeito de medidas de restrição de mobilidade na mobilidade regional.</p>"),
               HTML("<p align='justify'> <strong>(ii)</strong> Um outro ponto importante dessas análises é a indicação da força de conectividade entre os municípios para auxiliar na tomada de decisões de forma coordenada entre os diferentes municípios. Municípios altamente conectados devem implementar ações restritivas ou de relaxamento feitas de forma conjunta, evitando, por exemplo, que uma reabertura de um munícipio afete um município altamente conectado com ele ainda em fase crítica do avanço da doença.</p>"),
               br(),
               HTML("Mais informações em  <a href='https://www.ime.usp.br/~pedrosp/covid19'>www.ime.usp.br/~pedrosp/covid19</a>."),
               HTML("Mais informações sobre esse tipo de dado e análise no <a href='https://www.medrxiv.org/content/10.1101/2020.04.07.20056739v1'>artigo.</a>")),
               br(),
               tags$p("Aplicação desenvolvida por ", 
                      tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
                      tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
                      tags$a(href="mailto:ppeixoto@usp.br","ppeixoto@usp.br."),align="center"),
               br()
                 ))
  
  #Buttons for cities
  observe({
    if(!is.null(input$type) & !is.null(input$state)){
      if("all" %in% unlist(input$state)){
        cities <- c("all",shp$CD_GEOCMU)
        names(cities) <- c("Todos",paste(shp$Nome_Munic,"-",shp$UF))
      }
      else{
        cities <- c("all",shp$CD_GEOCMU[shp$UF %in% unlist(input$state)])
        names(cities) <- c("Todos",paste(shp$Nome_Munic[shp$UF %in% unlist(input$state)],"-",shp$UF[shp$UF %in% unlist(input$state)]))
      }
      
      if(input$type){
        output$cidades <- renderUI({selectizeInput(inputId = "cidades_ent",label = h3("Municípios Entrada"),
                                             choices = cities,multiple = T,options = list(maxOptions = 646))})
        output$cidades_sai <- renderUI({selectizeInput(inputId = "cidades_sai",label = h3("Municípios Saída"),
                                             choices = cities,multiple = T,options = list(maxOptions = 646))})
      }
      else{
        output$cidades_sai <- NULL
        output$cidades <- renderUI({selectizeInput(inputId = "cidades",label = h3("Municípios"),
                                             choices = cities,multiple = T,options = list(maxOptions = 646))})
      }
    }
  })
  
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
  
  title <- tags$div(tag.map.title, HTML("Mobilidade Municípios IME - USP"))
  
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
  
  #Control break app
  autoInvalidate <- reactiveTimer(1000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  #Input map
  observeEvent(input$process,{
     if(!is.null(input$date) & !is.null(input$state) & !is.null(input$type) & (!is.null(input$cidades)) | (!is.null(input$cidades_ent) & !is.null(!is.null(input$cidades_sai)))){
       withProgress(message = 'Gerando mapa...',value = 0,{
       file <- paste("./www/graph_",input$date,".rds",sep = "")
       tmp <- readRDS(file)
       
       #Title
       title <- tags$div(tag.map.title, HTML(paste("Mobilidade Municípios IME - USP <br>",input$date)))
       
       #Jitter end
       tmp$lng[tmp$side == "end"] <- tmp$lng[tmp$side == "end"] + 0.025
       tmp$lat[tmp$side == "end"] <- tmp$lat[tmp$side == "end"] - 0.01
        
       #Find which cities and edges should be plotted
       if(input$type){
         if('all' %in% unlist(input$cidades_sai)){
           if(!('all' %in% unlist(input$state)))
            tmp <- tmp %>% filter(tmp$begin %in% shp$CD_GEOCMU[shp$UF %in% unlist(input$state)])
         }
         else{
           tmp <- tmp %>% filter(tmp$begin %in% unlist(input$cidades_sai))
         }
         
         if('all' %in% unlist(input$cidades_ent)){
           if(!('all' %in% unlist(input$state)))
             tmp <- tmp %>% filter(tmp$end %in% shp$CD_GEOCMU[shp$UF %in% unlist(input$state)])
         }
         else{
           tmp <- tmp %>% filter(tmp$end %in% unlist(input$cidades_ent))
         }
       }
       else{
         if('all' %in% unlist(input$cidades)){
           if(!('all' %in% unlist(input$state))){
             tmp <- tmp %>% filter(tmp$begin %in% shp$CD_GEOCMU[shp$UF %in% unlist(input$state)])
             tmp <- tmp %>% filter(tmp$end %in% shp$CD_GEOCMU[shp$UF %in% unlist(input$state)])
           }
         }
         else{
           tmp <- tmp %>% filter(tmp$begin %in% unlist(input$cidades)) 
           tmp <- tmp %>% filter(tmp$end %in% unlist(input$cidades))
         }
       }
       incProgress(1/3, detail = "Dados lidos com sucesso, lendo shapefile...")
       
       #Size of line
       tmp$size <- 0.5 + 4.5 * (log(tmp$w,2)/max(log(tmp$w,2)))
       #Alpha of line
       tmp$size <- 0.5 + 0.5 * (tmp$w/max(tmp$w))
       
       #Shapes
       shp_tmp <- shp[shp$CD_GEOCMU %in% unique(c(tmp$begin,tmp$end)),]
       if("all" %in% unlist(input$state))
         shp_estados_tmp <- shp_estados
       else
         shp_estados_tmp <- shp_estados[shp_estados$UF %in% unlist(input$state),]
       incProgress(2/3, detail = "Shapefiles abertas com sucesso,gerando mapa...")
       
       # Make vector of colors for values smaller than 0 (20 colors)
       rc <- colorRampPalette(colors = c("yellow","orange","red"), space = "Lab")(200)
       
       #Palete
       mypal <- colorNumeric(palette = rc, domain = log(1+tmp$w,2))
       
       output$map <- renderLeaflet({
         map <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
           addTiles(urlTemplate = "", attribution = "©IME - USP. Design: Diego Marcondes.") %>%
           addControl(title, position = "topright", className="map-title") %>%
           addEasyButton(easyButton(
             icon="fa-crosshairs", title="Locate Me",
             onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
           addControl(ime, position = "bottomleft") %>%
           addControl(usp, position = "bottomleft") %>%
           addControl(fapesp, position = "bottomleft") %>%
           addPolygons(data = shp_estados_tmp,weight = 3,fill = F,color = "black") %>%
           addPolygons(data = shp_tmp,weight = 0.1,fillOpacity = 0.01,color = "black",
                       label = paste(shp_tmp$Nome_Munic,"-",shp_tmp$UF),
                       labelOptions = labelOptions(textsize = "15px")) #%>%
         for(i in unique(tmp$key))
           map <- map %>% addGeodesicPolylines(data = tmp[tmp$key == i,],lng = ~lng,lat = ~lat,group = ~key,
                                               color = ~mypal(log(1+w,2)),weight = 3*unique(tmp$size[tmp$key == i]),
                                               label = i,
                                               popup = paste("<p> ",i,"</p>",sep = ""),
                                               options = list(clickable = T),opacity = unique(tmp$size[tmp$key == i])/3)
         map <- map %>% addLegend(position = "bottomright", pal = mypal,values = log(1+tmp$w,2),
                                  labFormat = labelFormat(transform = function(x) 2^x),
                                  title = "Viagens",opacity = 0.8)    
       })
       incProgress(1, detail = "Feito!")})
     }
    })
   
   #Download
   output$download <- downloadHandler(
     filename = function() paste(input$filename,".html",sep = ""),
     content = function(file) {
       withProgress(message = 'Salvando mapa...',value = 0,{
       f <- paste("./www/graph_",input$date,".rds",sep = "")
       tmp <- readRDS(f)
       
       #Title
       title <- tags$div(tag.map.title, HTML(paste("Mobilidade Municípios IME - USP <br>",input$date)))
       
       #Jitter end
       tmp$lng[tmp$side == "end"] <- tmp$lng[tmp$side == "end"] + 0.025
       tmp$lat[tmp$side == "end"] <- tmp$lat[tmp$side == "end"] - 0.01
       
       #Find which cities and edges should be plotted
       if(input$type){
         if('all' %in% unlist(input$cidades_sai)){
           if(!('all' %in% unlist(input$state))){
             tmp <- tmp %>% filter(tmp$begin %in% shp$CD_GEOCMU[shp$UF %in% unlist(input$state)])
             begin <- shp$CD_GEOCMU[shp$UF %in% unlist(input$state)]
           }
           else
             begin <- shp$CD_GEOCMU
         }
         else{
           tmp <- tmp %>% filter(tmp$begin %in% unlist(input$cidades_sai))
           begin <- unlist(input$cidades_sai)
         }
         
         if('all' %in% unlist(input$cidades_ent)){
           if(!('all' %in% unlist(input$state))){
             tmp <- tmp %>% filter(tmp$end %in% shp$CD_GEOCMU[shp$UF %in% unlist(input$state)])
             end <- shp$CD_GEOCMU[shp$UF %in% unlist(input$state)]
           }
           else
             end <- shp$CD_GEOCMU
         }
         else{
           tmp <- tmp %>% filter(tmp$end %in% unlist(input$cidades_ent))
           end <- unlist(input$cidades_ent)
         }
       }
       else{
         if('all' %in% unlist(input$cidades)){
           if(!('all' %in% unlist(input$state))){
             tmp <- tmp %>% filter(tmp$begin %in% shp$CD_GEOCMU[shp$UF %in% unlist(input$state)])
             tmp <- tmp %>% filter(tmp$end %in% shp$CD_GEOCMU[shp$UF %in% unlist(input$state)])
             begin <- shp$CD_GEOCMU[shp$UF %in% unlist(input$state)]
             end <- shp$CD_GEOCMU[shp$UF %in% unlist(input$state)]
           }
           else{
             begin <- shp$CD_GEOCMU
             end <- shp$CD_GEOCMU
           }
         }
         else{
           tmp <- tmp %>% filter(tmp$begin %in% unlist(input$cidades)) 
           tmp <- tmp %>% filter(tmp$end %in% unlist(input$cidades))
           begin <- unlist(input$cidades)
           end <- unlist(input$cidades)
         }
       }
       
       #Size of line
       tmp$size <- 0.5 + 4.5 * (log(tmp$w,2)/max(log(tmp$w,2)))
       #Alpha of line
       tmp$size <- 0.5 + 0.5 * (tmp$w/max(tmp$w))
       
       #Shapes
       shp_tmp <- shp[shp$CD_GEOCMU %in% unique(c(begin,end)),]
       if("all" %in% unlist(input$state))
         shp_estados_tmp <- shp_estados
       else
         shp_estados_tmp <- shp_estados[shp_estados$UF %in% unlist(input$state),]
       
       # Make vector of colors for values smaller than 0 (20 colors)
       rc <- colorRampPalette(colors = c("yellow","orange","red"), space = "Lab")(200)
       
       #Palete
       mypal <- colorNumeric(palette = rc, domain = log(1+tmp$w,2))
       
       map <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
         addTiles(urlTemplate = "", attribution = "©IME - USP. Design: Diego Marcondes.") %>%
         addControl(title, position = "topright", className="map-title") %>%
         addEasyButton(easyButton(
           icon="fa-crosshairs", title="Locate Me",
           onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
         addPolygons(data = shp_estados_tmp,weight = 3,fill = F,color = "black") %>%
         addPolygons(data = shp_tmp,weight = 0.1,fillOpacity = 0.01,color = "black",
                     label = paste(shp_tmp$Nome_Munic,"-",shp_tmp$UF),
                     labelOptions = labelOptions(textsize = "15px")) #%>%
       for(i in unique(tmp$key))
         map <- map %>% addGeodesicPolylines(data = tmp[tmp$key == i,],lng = ~lng,lat = ~lat,group = ~key,
                                             color = ~mypal(log(1+w,2)),weight = 3*unique(tmp$size[tmp$key == i]),
                                             label = i,
                                             popup = paste("<p> ",i,"</p>",sep = ""),
                                             options = list(clickable = T),opacity = unique(tmp$size[tmp$key == i])/3)
       map <- map %>% addLegend(position = "bottomright", pal = mypal,values = log(1+tmp$w,2),
                                labFormat = labelFormat(transform = function(x) 2^x),
                                title = "Viagens",opacity = 0.8)   
       incProgress(1/2, detail = "Só mais um instante...")
       saveWidget(map,file = file)
       incProgress(1, detail = "Feito!")
       })
     })   
}

# Run the application 
shinyApp(ui = ui, server = server)


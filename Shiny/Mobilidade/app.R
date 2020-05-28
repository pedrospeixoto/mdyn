##################################
######Mobility Shiny app   #######
######Diego Marcondes      #######
######dmarcondes@ime.usp.br#######
##################################

#Pacotes
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyURL)
library(rgdal)
library(leaflet)
library(leaflet.extras)
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
  shinyURL.server()
  
  output$app = renderUI(
    navbarPage(theme = shinytheme("yeti"),
               "Mobilidade Municípios - BR",
               tabPanel("Mobilidade",fluidPage(
               tags$head(tags$style("#tbl {white-space: nowrap;}"),tags$style("#state {font-size:20px;}"),
                         tags$style("#date {font-size:20px;}")),
                 sidebarLayout(
                   sidebarPanel(
                     HTML('<center><img src="logo.png" height="100" width="300"></center>'),
                     br(),
                     p("DESCRIÇÃO A FAZER",
                       style="font-size:10pt;text-align:justify;"),
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
                     fluidRow(column(6,uiOutput("cidades")),
                              column(6,uiOutput("cidades_sai"))),
                     fluidRow(column(12,p("Se escolher mais do que 1000 cidades, o processamento pode demorar",style = "color:red"))),
                     leafletOutput(outputId = "map",height=750)))),
                 br(),
                 br(),
                 br(),
                 tags$p("Aplicação desenvolvida por ", 
                        tags$a(href="https://www.linkedin.com/in/diego-marcondes-87a1218b/","Diego Marcondes ")," com ",
                        tags$a(href="https://shiny.rstudio.com/","Shiny."),"Para qualquer tipo de suporte contactar ",
                        tags$a(href="mailto:ppeixoto@usp.br","ppeixoto@usp.br."),align="center"),
               br()
                 )))
  
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
        output$cidades <- renderUI({selectizeInput(inputId = "cidades",label = h3("Municípios"),
                                             choices = cities,multiple = T,options = list(maxOptions = 646))})
      }
    }
  })
  
  #Input map
   observe({
     if(!is.null(input$date) & !is.null(input$state) & !is.null(input$type) & !is.null(input$cidades)){
       output$alert <- renderText({return("")})
       file <- paste("./www/graph_",input$date,".rds",sep = "")
       tmp <- readRDS(file)
        
       #Find which cities and edges should be plotted
       if(input$type){
         if('all' %in% unlist(input$cidades)){
           if(!('all' %in% unlist(input$state))){
            tmp <- tmp %>% filter(tmp$begin %in% shp$CD_GEOCMU[shp$UF %in% unlist(input$state)])
            begin <- shp$CD_GEOCMU[shp$UF %in% unlist(input$state)]
           }
           else
             begin <- shp$CD_GEOCMU
         }
         else{
           tmp <- tmp %>% filter(tmp$begin %in% unlist(input$cidades))
           begin <- unlist(input$cidades)
         }
         
         if('all' %in% unlist(input$cidades_sai)){
           if(!('all' %in% unlist(input$state))){
             tmp <- tmp %>% filter(tmp$end %in% shp$CD_GEOCMU[shp$UF %in% unlist(input$state)])
             end <- shp$CD_GEOCMU[shp$UF %in% unlist(input$state)]
           }
           else
             end <- shp$CD_GEOCMU
         }
         else{
           tmp <- tmp %>% filter(tmp$end %in% unlist(input$cidades_sai))
           end <- unlist(input$cidades_sai)
         }
       }
       else{
         if('all' %in% unlist(input$cidades)){
           if(!('all' %in% unlist(input$state))){
             tmp <- tmp %>% 
               filter(tmp$begin %in% shp$CD_GEOCMU[shp$UF %in% unlist(input$state)] & tmp$end %in% shp$CD_GEOCMU[shp$UF %in% unlist(input$state)])
             begin <- shp$CD_GEOCMU[shp$UF %in% unlist(input$state)]
             end <- shp$CD_GEOCMU[shp$UF %in% unlist(input$state)]
           }
           else{
             begin <- shp$CD_GEOCMU
             end <- shp$CD_GEOCMU
           }
         }
         else{
           tmp <- tmp %>% filter(tmp$begin %in% unlist(input$cidades) && tmp$end %in% unlist(input$cidades))
           begin <- unlist(input$cidades)
           end <- unlist(input$cidades)
         }
       }
       shp_tmp <- shp[shp$CD_GEOCMU %in% unique(c(begin,end)),]
       if("all" %in% unlist(input$state))
         shp_estados_tmp <- shp_estados
       else
         shp_estados_tmp <- shp_estados[shp_estados$UF %in% unlist(input$state),]
       
       
       
       # Make vector of colors for values smaller than 0 (20 colors)
       rc <- colorRampPalette(colors = c("white","orange","red"), space = "Lab")(200)
       
       #Palete
       mypal <- colorNumeric(palette = rc, domain = log(tmp$w,2))
       
       output$map <- renderLeaflet({
         leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% 
           addPolygons(data = shp_estados_tmp,weight = 3,fill = F,color = "white") %>%
           addPolygons(data = shp_tmp,weight = 0.5,fillOpacity = 0.01,color = "white",
                       label = paste(shp_tmp$Nome_Munic,"-",shp_tmp$UF),
                       labelOptions = labelOptions(textsize = "15px")) %>%
           addGeodesicPolylines(lng = tmp$lng,lat = tmp$lat,group = tmp$key,weight = log(log(tmp$w,2),2),
                                color = mypal(log(tmp$w,2)))
           #addPolygons(data = tmp,weight = 1,fillColor = mypal(100*tmp$iso),color = "grey",
          #             ,opacity = 0.5,fillOpacity = 0.5) %>%
          # addLegend(position = "bottomright", pal = mypal, values = 100*tmp$iso,na.label = "S/D",
          #           title = "Isolamento (%)",opacity = 0.8)    
       })}
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


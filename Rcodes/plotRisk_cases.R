#' @import tidyr
#' @import ggplot2
#' @export
#' @title Plot Infected x Risk of Infection
#'
#' @description Build various plots combining the number of infected by COVID-19 in a city of a state in Brazil and the risk of infection
#' estimated by Peixoto et. al. (2020).
#'
#' @details This function download data from <https://brasil.io/dataset/covid19/caso? about the number of confirmed infected individuals,
#' and confirmed deaths, by COVID-19 in each city of Brazil and build plots comparing these quantities with the risk of infection of each city
#' estimated by Peixoto et. al. (2020). It may consider data for a given day or a range of days during the pandemic.
#'
#' @param states A vector with the states to build plots for. It should contain the two letters representing the name of the states. To plot all
#' available states enter "all", which is default.
#' @param day The date which to plot data from. The default is today. Must be represented as YYYY-MM-DD.
#' @param day.init To build a sequence of plots to an interval of days. Should represent the initial day of the range. This surpasses the
#' \emph{day} argument.
#' @param day.final The final day of the range.
#' @param cities Which cities to plot information for. It should either be "populated" for cities with the greatest populations or
#' a named list with the city names. The names must be the two letters representing the states. The city names must be written as they are in the
#' \emph{risk} dataset.
#' @param pos_name A string to put at the end of the saved plots. If you want to run with distinct cities for a same day, use this string
#' to not overwrite the plots.
#' @return A list with all the generated plots, which were also saved on pdf files in high resolution
#'
#' @example
#' #Generate plot for all states today
#' p <- plotRisk_cases() #may work only in the evening, as data about confirmed cases is stored only in the afternoon
#'
#' #Generate plot for all states from March 1st to April 9th
#' #p <- plotRisk_cases(day.init = "2020-03-01",day.final = "2020-04-09") #may take some time
#'
#' #Generate only for the cities in São Paulo with more than 500,000 inhabitants in April 9th
#' c <- risk[["sp"]]$City[risk[["sp"]]$populacao_estimada > 500000]
#' p <- plotRisk_cases(states = "sp",day = "2020-04-09",cities = list("sp" = c),pos_name = "500k")
#'
#' @references Peixoto, et. al. Modeling future spread of infections via mobile geolocation data and population dynamics.
#' An application to COVID-19 in Brazil. 2020. Available at <https://www.ime.usp.br/~pedrosp/covid-19/>.

plotRisk_cases <- function(states = "all",day = "today",day.init = NULL,day.final = NULL,cities = "populated",pos_name = "pop"){
  #Themes for ggplot
  titles <- theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12,color = "black"),
                  axis.title = element_text(size = 14), legend.text = element_text(size = 14),
                  legend.title = element_text(size = 14),
                  panel.border = element_blank(),
                  panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                  legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                  legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
  themes <- list(theme_linedraw())
  n <- list("sp" = "São Paulo","rj" = "Rio de Janeiro")

  #Risk data
  risk_data <- mdyn::risk

  #Get the days to build plots
  if(is.null(day.init) || is.null(day.final)){
    if(day == "today")
      day <-Sys.Date()
    else if(!is.valid.date(day))
      stop("Date invalid. Please check if day is a valid date.")
  }
  else{
    if(is.valid.date(day.init) && is.valid.date(day.final))
      day <- as.character(seq.Date(from = as.Date(day.init),to = as.Date(day.final),by = 1))
    else
      stop("Date invalid. Please check day.init and day.final are valid dates.")
  }

  #Get the states to build plot for
  if(states == "all")
    states <- c("sp","rj")
  else{
    states <- tolower(states)
    tmp <- states[!(states %in% c("sp","rj"))]
    if(length(tmp) > 0)
      stop(paste("Sorry, but states",paste(tmp,collapse = ","),"are not available yet! Please, come back later."))
  }

  #Dowload number of infected data
  cat("---------------------------------****---------------------------------\n")
  cat("Reading data about infected individuals\n")
  file <- gzcon(url("https://data.brasil.io/dataset/covid19/caso_full.csv.gz"))
  txt <- readLines(file)
  data <- read.csv(textConnection(txt))
  data <- data[tolower(data$state) %in% states,]
  data <- data[data$date %in% day,]
  data <- data[,c(1,3,8,9,11,12,15)]
  data$city <- as.character(toupper(data$city))
  data <- data[data$city != "Importados/Indefinidos",]
  if(nrow(data) == 0)
    stop("Sorry, but is not data about infected individuals for the chosen day range. Maybe try get data from yesterday.")
  cat("---------------------------------DONE---------------------------------\n")

  #Merging datasets
  for(s in states){
    risk_data[[s]]$City <- as.character(risk_data[[s]]$City)
    risk_data[[s]] <- merge(x = risk_data[[s]],y = data[data$state == toupper(s),],by.x = "City",by.y = "city",
                            all.x = T)
  }
  rm(data)

  #Choose only given cities
  if(cities == "populated"){
    risk_data[["sp"]] <- risk_data[["sp"]][risk_data[["sp"]]$populacao_estimada > 100000,]
    risk_data[["rj"]] <- risk_data[["rj"]][risk_data[["rj"]]$populacao_estimada > 75000,]
  }
  else{
    names(cities) <- tolower(names(cities))
    for(s in states)
      risk_data[[s]] <- risk_data[[s]][risk_data[[s]]$City %in% cities[[s]],]
  }

  cat("---------------------------------****---------------------------------\n")
  cat("Building and saving plots\n")
  plots <- list()
  #Plots for each day and state
  for(s in states){
    cat(paste("Started state of",n[[s]],"\n"))
    plots[[s]] <- list()
    for(d in day){
      tmp <- risk_data[[s]]
      #tmp$date[is.na(tmp$date)] <- d
      tmp <- tmp[tmp$date == d,]
      tmp_invert <- tidyr::gather(tmp,"s","rank",-City,-State,-risk_lesser,-risk_greater,-risk,-dist,-populacao_estimada,
                                          -date,-last_available_confirmed,-last_available_confirmed_per_100k_inhabitants,
                                          -last_available_death_rate,-last_available_deaths,-state)
      tmp_invert$City <- factor(tmp_invert$City,tmp_invert$City[order(tapply(tmp_invert$rank,tmp_invert$City,mean))])
      tmp_invert$last_available_confirmed[is.na(tmp_invert$last_available_confirmed)] <- 0
      tmp_invert$last_available_confirmed_per_100k_inhabitants[is.na(tmp_invert$last_available_confirmed_per_100k_inhabitants)] <- 0
      tmp_invert$last_available_death_rate[is.na(tmp_invert$last_available_death_rate)] <- 0
      tmp_invert$last_available_deaths[is.na(tmp_invert$last_available_deaths)] <- 0

      #Risk vs distance to capital city
      p <- ggplot(tmp_invert,aes(x = City,y = rank,colour = risk)) +
        ggtitle(paste("Risk x Distance to capital city\nState of",n[[s]],"\nGenerated from mdyn package on R (https://github.com/pedrospeixoto/mdyn)\nPeixoto, et. al. Modeling future spread of infections via mobile geolocation data and population dynamics. An application to COVID-19 in Brazil.")) +
        geom_point(aes(y = max(tmp_invert$rank,na.rm = T)-rank)) +
        geom_smooth(data = unique(tmp_invert[c(1,6,15)]),se = F,aes(x = City,y = (dist/max(tmp_invert$dist,na.rm = T)) * max(tmp_invert$rank,na.rm = T),
                                                            group = 1),color = "gray") +
        geom_point(data = unique(tmp_invert[c(1,5,6,15)]),aes(x = City,y = (dist/max(tmp_invert$dist,na.rm = T)) * max(tmp_invert$rank,na.rm = T),
                                                            colour = risk),pch = 2) +
        themes + titles + theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank()) + ylab("Rank of Infection") +
        scale_y_continuous(sec.axis = sec_axis(~.*(max(tmp_invert$dist,na.rm = T)/max(tmp_invert$rank,na.rm = T)),
                                               name = "Distance to capital city (km)")) +
        scale_colour_manual("Risk",values = c("red","orange"))
      pdf(file = paste("plot_risk_distance_",s,"_",pos_name,".pdf",sep = ""),width = 15,height = 10)
      suppressWarnings(suppressMessages(print(p)))
      dev.off()
      plots[[s]][["distance_capital"]] <- p

      #Risk vs number of confirmed cases
      p <- ggplot(tmp_invert,aes(x = City,y = rank,colour = risk)) +
        ggtitle(paste("Risk x Number of confirmed cases on",d,"\nState of",n[[s]],"\nGenerated from mdyn package on R (https://github.com/pedrospeixoto/mdyn)\nPeixoto, et. al. Modeling future spread of infections via mobile geolocation data and population dynamics. An application to COVID-19 in Brazil.")) +
        geom_point(aes(y = max(tmp_invert$rank,na.rm = T)-rank)) +
        geom_smooth(data = unique(tmp_invert[c(1,9,15)]),se = F,
                    aes(x = City,y = (mod_log(last_available_confirmed)/mod_log(max(tmp_invert$last_available_confirmed,na.rm = T))) * max(tmp_invert$rank,na.rm = T),
                                                                    group = 1),color = "gray") +
        geom_point(data = unique(tmp_invert[c(1,5,9,15)]),
                   aes(x = City,y = (mod_log(last_available_confirmed)/mod_log(max(tmp_invert$last_available_confirmed,na.rm = T))) * max(tmp_invert$rank,na.rm = T),
                                                              colour = risk),pch = 2) +
        themes + titles + theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank()) + ylab("Rank of Infection") +
        scale_y_continuous(sec.axis = sec_axis(~.*(mod_log(max(tmp_invert$last_available_confirmed,na.rm = T))/max(tmp_invert$rank,na.rm = T)),
                                               name = "Number of confirmed cases",labels = function(x) round(exp(x)))) +
        scale_colour_manual("Risk",values = c("red","orange"))
      pdf(file = paste("plot_risk_confirmed_",s,"_",d,"_",pos_name,".pdf",sep = ""),width = 15,height = 10)
      suppressWarnings(suppressMessages(print(p)))
      dev.off()
      plots[[s]][["confirmed"]] <- p

      #Risk vs number of confirmed cases per 100k
      p <- ggplot(tmp_invert,aes(x = City,y = rank,colour = risk)) +
        ggtitle(paste("Risk x Number of confirmed cases per 100k inhabitants on",d,"\nState of",n[[s]],"\nGenerated from mdyn package on R (https://github.com/pedrospeixoto/mdyn)\nPeixoto, et. al. Modeling future spread of infections via mobile geolocation data and population dynamics. An application to COVID-19 in Brazil.")) +
        geom_point(aes(y = max(tmp_invert$rank,na.rm = T)-rank)) +
        geom_smooth(data = unique(tmp_invert[c(1,10,15)]),se = F,
                    aes(x = City,y = (mod_log(last_available_confirmed_per_100k_inhabitants)/mod_log(max(tmp_invert$last_available_confirmed_per_100k_inhabitants,na.rm = T))) * max(tmp_invert$rank,na.rm = T),
                        group = 1),color = "gray") +
        geom_point(data = unique(tmp_invert[c(1,5,10,15)]),
                   aes(x = City,y = (mod_log(last_available_confirmed_per_100k_inhabitants)/mod_log(max(tmp_invert$last_available_confirmed_per_100k_inhabitants,na.rm = T))) * max(tmp_invert$rank,na.rm = T),
                       colour = risk),pch = 2) +
        themes + titles + theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank()) + ylab("Rank of Infection") +
        scale_y_continuous(sec.axis = sec_axis(~.*(mod_log(max(tmp_invert$last_available_confirmed_per_100k_inhabitants,na.rm = T))/max(tmp_invert$rank,na.rm = T)),
                                               name = "Number of confirmed cases per 100k inhabitants",labels = function(x) round(exp(x)))) +
        scale_colour_manual("Risk",values = c("red","orange"))
      pdf(file = paste("plot_risk_confirmed100k_",s,"_",d,"_",pos_name,".pdf",sep = ""),width = 15,height = 10)
      suppressWarnings(suppressMessages(print(p)))
      dev.off()
      plots[[s]][["confirmed100k"]] <- p

      #Risk vs number of death
      p <- ggplot(tmp_invert,aes(x = City,y = rank,colour = risk)) +
        ggtitle(paste("Risk x Number of deaths on",d,"\nState of",n[[s]],"\nGenerated from mdyn package on R (https://github.com/pedrospeixoto/mdyn)\nPeixoto, et. al. Modeling future spread of infections via mobile geolocation data and population dynamics. An application to COVID-19 in Brazil.")) +
        geom_point(aes(y = max(tmp_invert$rank,na.rm = T)-rank)) +
        geom_smooth(data = unique(tmp_invert[c(1,12,15)]),se = F,
                    aes(x = City,y = (mod_log(last_available_deaths)/mod_log(max(tmp_invert$last_available_deaths,na.rm = T))) * max(tmp_invert$rank,na.rm = T),
                        group = 1),color = "gray") +
        geom_point(data = unique(tmp_invert[c(1,5,12,15)]),
                   aes(x = City,y = (mod_log(last_available_deaths)/mod_log(max(tmp_invert$last_available_deaths,na.rm = T))) * max(tmp_invert$rank,na.rm = T),
                       colour = risk),pch = 2) +
        themes + titles + theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank()) + ylab("Rank of Infection") +
        scale_y_continuous(sec.axis = sec_axis(~.*(mod_log(max(tmp_invert$last_available_deaths,na.rm = T))/max(tmp_invert$rank,na.rm = T)),
                                               name = "Number of deaths",labels = function(x) round(exp(x)))) +
        scale_colour_manual("Risk",values = c("red","orange"))
      pdf(file = paste("plot_risk_deaths_",s,"_",d,"_",pos_name,".pdf",sep = ""),width = 15,height = 10)
      suppressWarnings(suppressMessages(print(p)))
      dev.off()
      plots[[s]][["deaths"]] <- p

      #Risk vs death rate
      p <- ggplot(tmp_invert,aes(x = City,y = rank,colour = risk)) +
        ggtitle(paste("Risk x Death rate on",d,"\nState of",n[[s]],"\nGenerated from mdyn package on R (https://github.com/pedrospeixoto/mdyn)\nPeixoto, et. al. Modeling future spread of infections via mobile geolocation data and population dynamics. An application to COVID-19 in Brazil.")) +
        geom_point(aes(y = max(tmp_invert$rank,na.rm = T)-rank)) +
        geom_smooth(data = unique(tmp_invert[c(1,11,15)]),se = F,
                    aes(x = City,y = ((last_available_death_rate)/(max(tmp_invert$last_available_death_rate,na.rm = T))) * max(tmp_invert$rank,na.rm = T),
                        group = 1),color = "gray") +
        geom_point(data = unique(tmp_invert[c(1,5,11,15)]),
                   aes(x = City,y = ((last_available_death_rate)/(max(tmp_invert$last_available_death_rate,na.rm = T))) * max(tmp_invert$rank,na.rm = T),
                       colour = risk),pch = 2) +
        themes + titles + theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank()) + ylab("Rank of Infection") +
        scale_y_continuous(sec.axis = sec_axis(~.*((max(tmp_invert$last_available_death_rate,na.rm = T))/max(tmp_invert$rank,na.rm = T)),
                                               name = "Death rate")) +
        scale_colour_manual("Risk",values = c("red","orange"))
      pdf(file = paste("plot_risk_death_rate_",s,"_",d,"_",pos_name,".pdf",sep = ""),width = 15,height = 10)
      suppressWarnings(suppressMessages(print(p)))
      dev.off()
      plots[[s]][["death_rate"]] <- p
    }
    cat(paste("Ended state of",n[[s]],"\n"))
  }
  return(p)
}

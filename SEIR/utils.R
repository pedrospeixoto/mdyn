#Util functions for fitting and simulating the SEIR metapopulation model for COVID-19

#Plot themes
titles <- theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12,color = "black"),
                axis.title = element_text(size = 14), legend.text = element_text(size = 14),
                legend.title = element_text(size = 14),
                panel.border = element_blank(),
                panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
titles_Map <- theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12,color = "black"),
                    axis.title = element_text(size = 14), legend.text = element_text(size = 14),
                    legend.title = element_text(size = 14,face = "bold"),plot.title = element_text(size = 16,face = "bold",hjust = 0.5),
                    panel.border = element_blank(),legend.key.width=unit(4,"cm"),
                    panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                    legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                    legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))

#Get legend from ggplot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#Function to remove accents
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

#SEIR solver
solve_seir <- function(y,times,derivatives,parms){
  mod <- data.frame("t" = 1,rbind(y))
  if(length(names(y)) > 0)
    names(mod)[-1] <- names(y)
  for(t in times[-1]){
    y <- y + unlist(derivatives(t = t-1,Y = y,parK = parms))
    mod[t,] <- c(t,y)
  }
  return(mod)
}

#Get drs data
# drs <- read.csv("/home/dmarcondes/mdyn/SEIR/dados/DRS.csv",sep = ";") #Read drs table
# drs$Municipio <- gsub("'","",drs$Municipio) #Correct names 
# drs <- drs[match(par$names,drs$Municipio),] #Order cities
# tmp <- data.frame("Municipio" = par$names,"pop" = par$pop) #Get population of each city
# drs <- data.table(merge(drs,tmp)) #Merge to get population
# drs <- drs[,N := sum(pop),by = DRS] #Population by DRS
# drs <- drs %>% select(DRS,Regiao,Municipio,N) %>% data.frame() #Clean
# drs$DRS <- as.character(drs$DRS) #Character DRS
# drs$DRS[drs$Municipio == "SÃO PAULO"] <- "0" #Set city of SP as DRS
# drs$Regiao <- as.character(drs$Regiao) #Character Regiao
# drs$Regiao[drs$Municipio == "SÃO PAULO"] <- "Cidade de São Paulo" #Set city of SP as DRS
# drs$DRS <- factor(drs$DRS,c("0","I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII",
#                             "XIII","XIV","XV","XVI","XVII")) #DRS to factor
# drs$Regiao <- factor(drs$Regiao) #Regiao to factor
# saveRDS(object = drs,"drs.rds")

#Get notification data state of SP
get_data_SP <- function(){
  file <- gzcon(url("https://github.com/seade-R/dados-covid-sp/raw/master/data/dados_covid_sp.csv")) #Data path
  txt <- readLines(file) #Read lines
  obs <- read.csv(textConnection(txt),sep = ";") #Get data
  names(obs)[c(5,1,6,10)] <- c("date","city","last_available_confirmed","last_available_deaths")
  obs <- obs %>% 
    select(city,date,last_available_confirmed,last_available_deaths) %>% na.omit() %>% filter(city != "Ignorado") #Only confirmed cases and death
  obs$city <- toupper(gsub(pattern = "'",replacement = "",x = obs$city)) #Correct names
  obs$city[obs$city == "ITAÓCA"] <- "ITAOCA" #Correct names
  obs$city[obs$city == "BIRITIBA MIRIM"] <- "BIRITIBA-MIRIM" #Correct names
  obs$date <- ymd(obs$date) #Date
  for(d in unique(as.character(obs$date))){ #Add zero to cities where and when no cases/deaths were confirmed
    tmp <- par$names[!(par$names %in% obs$city[obs$date == d])]
    tmp <- data.frame("city" = tmp,"date" = ymd(d),"last_available_confirmed" = 0,"last_available_deaths" = 0)
    obs <- rbind.data.frame(obs,tmp)
  }
  obs$key <- paste(obs$city,obs$date) #Set key
  obs_new <- obs[1,] #Create new dataframe
  obs_new$recovered <- 0 #Initialize new variables
  obs_new$infected <- 0 #Initialize new variables
  obs_new$new_infected <- 0 #Initialize new variables
  obs_new$new_death <- 0 #Initialize new variables
  obs_new$new_infected_cor <- 0 #Initialize new variables
  obs_new$new_death_cor <- 0 #Initialize new variables
  obs_new$confirmed_corrected <- 0 #Initialize new variables
  obs_new$deaths_corrected <- 0 #Initialize new variables
  obs_new$new_infected_mean <- 0 #Initialize new variables
  for(c in unique(obs$city)){ #For each city
    tmp <- data.frame(obs %>% filter(city == c)) #Get data from the city
    tmp <- tmp[order(tmp$date),] #Order by date
    tmp$recovered <- 0 #Initialize new variables
    tmp$infected <- 0 #Initialize new variables
    tmp$new_infected <- c(tmp$last_available_confirmed[1],diff(tmp$last_available_confirmed)) #Incidence of infection
    tmp$new_death <- c(tmp$last_available_deaths[1],diff(tmp$last_available_deaths)) #Incidence death
    tmp$new_infected_cor <- 0
    tmp$new_infected_cor[1:3] <- tmp$new_infected[1:3] #Initialize new variables
    tmp$new_death_cor <- 0
    tmp$new_death_cor[1:3] <- tmp$new_death[1:3] #Initialize new variables
    tmp$confirmed_corrected <- 0 #Initialize new variables
    tmp$deaths_corrected <- 0 #Initialize new variables
    tmp$new_infected_mean #Initialize new variables
    for(i in 4:(nrow(tmp)-3)){
      tmp$new_infected_cor[i] <- mean(tmp$new_infected[(i-3):(i+3)]) #New cases corrected
      tmp$new_death_cor[i] <- mean(tmp$new_death[(i-3):(i+3)]) #New deaths corrected
      tmp$confirmed_corrected <- cumsum(tmp$new_infected_cor) #Confirmed cases corrected
      tmp$deaths_corrected <- cumsum(tmp$new_death_cor) #Confirmed deaths corrected
      if(i > 7){ #Estimating recovered
        tmp$recovered[i] <- tmp$recovered[i-1] + tmp$new_infected_cor[i-7] #New corrected confirmed cases 15 days ago are recovered
        tmp$infected[i] <- tmp$confirmed_corrected[i] - tmp$recovered[i] #Delete recovered from infected
      }
      else{ #Correct for first 16 days
        tmp$recovered[i] <- 0
        tmp$infected[i] <- tmp$confirmed_corrected[i]
      }
    }
    for(i in 1:(nrow(tmp)-2))
      tmp$new_infected_mean[i] <- mean(tmp$new_infected_cor[i:(i+2)]) #Mean of new cases corrected
    obs_new <- rbind.data.frame(obs_new,tmp[-c(nrow(tmp)-1,nrow(tmp)),]) #Bind
  }
  obs <- obs_new[-1,] #Erase first extra row
  obs$recovered <- obs$recovered - obs$deaths_corrected #Delete deaths from recovered
  obs[obs < 0] <- 0 #Correct
  
  return(obs)
}

#Epidemiological curve
EPI_curve <- function(obs,end_validate,pos){
  dplot <- data.frame("date" = unique(obs$date),
                      "R" = 1+tapply(X = obs$recovered,INDEX = obs$date,FUN = sum),
                      "I" = 1+tapply(X = obs$infected,INDEX = obs$date,FUN = sum),
                      "TI" = 1+tapply(X = obs$confirmed_corrected,INDEX = obs$date,FUN = sum),
                      "TD" = 1+tapply(X = obs$deaths_corrected,INDEX = obs$date,FUN = sum)) #Get data
  dplot <- dplot %>% gather("var","values",-date) %>% filter(date <= end_validate & date >= ymd("2020-04-01")) #Gather
  dplot$date <- ymd(dplot$date)
  p <- ggplot(dplot,aes(x = ymd(date),y = values,color = var)) + geom_line() +
    theme_solarized(light = FALSE) +  
    theme(legend.title = element_text(face = "bold"),legend.position = "bottom") + ylab("Confirmed Cases Corrected") +
    xlab("Date") + scale_colour_discrete("",labels = c("Infected","Recovered","Deaths",
                                                       "Total Confirmed")) +
    theme(plot.title = element_text(face = "bold",size = 25,color = "white",hjust = 0.5),
          axis.text.x = element_text(size = 15,face = "bold",color = "white"),
          legend.text = element_text(size = 15,face = "bold",color = "white"),
          axis.text.y = element_text(size = 15,face = "bold",color = "white"),
          legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_text(color = "white",size = 20),
          plot.caption = element_text(face = "bold",color = "white",hjust = 0,size = 15)) +
    theme(plot.margin = unit(c(1,1,1,1), "lines")) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 20,face = "bold",color = "white")) +
    labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
    ggtitle(paste("Epidemiological Curve for State of São Paulo until",end_validate))
  pdf(file = paste("/storage/SEIR/",pos,"/EPCurve_",end_validate,".pdf",sep = ""),width = 15,height = 10)
  suppressWarnings(suppressMessages(print(p))) #Save plot
  dev.off()
}

#Get data API
get_data_API <- function(){
  library(httr)
  dados <- GET("https://brasil.io/api/dataset/covid19/caso_full/data/")
  dados <- content(dados)
  n <- dados$'next'
  dados <- dados$results
  dados <- lapply(dados,function(x) data.frame(rbind(unlist(x))))
  dados <- bind_rows(dados)
  cat(n)
  cat("\n")
  while(!is.null(n)){
    tmp <- GET(n)
    tmp <- content(tmp)
    n <- tmp$'next'
    cat(n)
    cat("\n")
    tmp <- tmp$results
    tmp <- lapply(tmp,function(x) data.frame(rbind(unlist(x))))
    tmp <- bind_rows(tmp)
    dados <- rbind.data.frame(dados,tmp)
  }
  return(dados)
}
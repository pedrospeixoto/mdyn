#Calculate Growth Rate of DRSs and cities with 1000+ cases
growth_rate <- function(obs,obs_drs,drs,par,pos,init_validate,end_validate,day_validate){
  lambda <- vector()
  
  #For each DRS
  gI <- obs_drs %>% filter(date %in% seq.Date(from = ymd(init_validate),to = ymd(end_validate),1)) %>%
    select(date,DRS,infected) %>% unique() #Infected by DRS in validation week
  names(gI)[3] <- "I_drs"
  
  for(d in unique(drs$DRS)){ #For each DRS
    tmp <- gI %>% filter(DRS == d) #Data of DRS
    tmp <- data.frame("t" = 0:6,"y" = tmp$I_drs) #Data to fit
    mod <- lm(log(y+1) ~ t,data = tmp) #lm
    lambda[par$names %in% drs$Municipio[drs$DRS == d]] <- mod$coefficients[2] #Lambda
    p <- ggplot(tmp,aes(x = t,y = y)) + geom_point(color = "white") + 
      stat_function(fun = function(t) exp(mod$coefficients[1])*exp(mod$coefficients[2]*t),color = "white") +
      theme_solarized(light = FALSE) +  
      theme(legend.title = element_text(face = "bold"),legend.position = "none") + ylab("Casos Confirmados") +
      xlab("Data") + scale_x_continuous(breaks = 0:6,labels = paste(day(day_validate),"/0",
                                                                    month(day_validate),sep = "")) +
      theme(plot.title = element_text(face = "bold",size = 25,color = "white",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold",color = "white"),
            axis.text.y = element_text(size = 15,face = "bold",color = "white"),
            legend.box.margin = unit(x=c(20,0,0,0),units="mm"),
            legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title = element_text(color = "white",size = 20),
            plot.caption = element_text(face = "bold",color = "white",hjust = 0,size = 15)) +
      theme(plot.margin = unit(c(1,1,1,1), "lines")) +
      theme(strip.background = element_blank(),
            strip.text = element_text(size = 20,face = "bold",color = "white")) +
      labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
      ggtitle(paste("Crescimento exponencial na semana de validação para a DRS",d,"-",unique(drs$Regiao[drs$DRS == d])))
    pdf(file = paste("/storage/SEIR/",pos,"/AjusteRate/DRS_",gsub(" ","",unique(drs$Regiao[drs$DRS == d])),"_rate_",pos,".pdf",sep = ""),
        width = 15,height = 10)
    suppressWarnings(suppressMessages(print(p))) #Save plot
    dev.off()
  }
  
  #For each city with 1000+ cases in init_validate
  c_1000 <- obs %>% filter(date == ymd(init_validate) & confirmed_corrected > 500)
  c_1000 <- c_1000$city
  for(c in c_1000){
    tmp <- obs %>% filter(city == c & date >= ymd(init_validate) & date <= ymd(end_validate)) #Data of city
    tmp <- data.frame("t" = 0:6,"y" = tmp$infected) #Data
    mod <- lm(log(y) ~ t,data = tmp) #lm
    lambda[par$names == c] <- mod$coefficients[2] #Lambda
    p <- ggplot(tmp,aes(x = t,y = y)) + geom_point(color = "white") + 
      stat_function(fun = function(t) exp(mod$coefficients[1])*exp(mod$coefficients[2]*t),color = "white") +
      theme_solarized(light = FALSE) +  
      theme(legend.title = element_text(face = "bold"),legend.position = "none") + ylab("Casos Confirmados") +
      xlab("Data") + scale_x_continuous(breaks = 0:6,labels = paste(day(day_validate),"/0",
                                                                    month(day_validate),sep = "")) +
      theme(plot.title = element_text(face = "bold",size = 25,color = "white",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold",color = "white"),
            axis.text.y = element_text(size = 15,face = "bold",color = "white"),
            legend.box.margin = unit(x=c(20,0,0,0),units="mm"),
            legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title = element_text(color = "white",size = 20),
            plot.caption = element_text(face = "bold",color = "white",hjust = 0,size = 15)) +
      theme(plot.margin = unit(c(1,1,1,1), "lines")) +
      theme(strip.background = element_blank(),
            strip.text = element_text(size = 20,face = "bold",color = "white")) +
      labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
      ggtitle(paste("Crescimento exponencial na semana de validação para",c,"- SP"))
    pdf(file = paste("/storage/SEIR/",pos,"/AjusteRate/",gsub(" ","",c),"_rate_",pos,".pdf",sep = ""),
        width = 15,height = 10)
    suppressWarnings(suppressMessages(print(p))) #Save plot
    dev.off()
  }
  
  return(lambda)
}
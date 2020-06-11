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
    theme(legend.title = element_text(face = "bold"),legend.position = "bottom") + ylab("Indivíduos") +
    xlab("Date") + scale_colour_discrete("",labels = c("Infectados","Recuperados","Mortes",
                                                       "Casos Confirmados")) +
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
    ggtitle(paste("Curva epidemiológica para o Estado de São Paulo até",end_validate))
  pdf(file = paste("/storage/SEIR/",pos,"/EPCurve_",end_validate,".pdf",sep = ""),width = 15,height = 10)
  suppressWarnings(suppressMessages(print(p))) #Save plot
  dev.off()
}
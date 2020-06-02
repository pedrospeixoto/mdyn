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
drs <- read.csv("/home/dmarcondes/mdyn/SEIR/dados/DRS.csv",sep = ";") #Read drs table
drs$Municipio <- gsub("'","",drs$Municipio) #Correct names 
drs <- drs[match(par$names,drs$Municipio),] #Order cities
tmp <- data.frame("Municipio" = par$names,"pop" = par$pop) #Get population of each city
drs <- data.table(merge(drs,tmp)) #Merge to get population
drs <- drs[,N := sum(pop),by = DRS] #Population by DRS
drs <- drs %>% select(DRS,Regiao,Municipio,N) %>% data.frame() #Clean
drs$DRS <- as.character(drs$DRS) #Character DRS
drs$DRS[drs$Municipio == "SÃO PAULO"] <- "0" #Set city of SP as DRS
drs$Regiao <- as.character(drs$Regiao) #Character Regiao
drs$Regiao[drs$Municipio == "SÃO PAULO"] <- "Cidade de São Paulo" #Set city of SP as DRS
drs$DRS <- factor(drs$DRS,c("0","I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII",
                            "XIII","XIV","XV","XVI","XVII")) #DRS to factor
drs$Regiao <- factor(drs$Regiao) #Regiao to factor


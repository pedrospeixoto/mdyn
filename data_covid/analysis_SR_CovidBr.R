library(tidyverse)
library(stats)
library(reshape2)
library(tseries)
library(DCCA)
library(TTR)
library(forecastML)
library(DT)
library(ggplot2)
library(gridExtra)
library(repmis)
library(mgcv)
library(igraph)
library(GGally)
library(centiserve)




load("/media/cecilia/5863A2E93FFF454C/crazy_people/data/agregado.new.cases.RData")
load("/media/cecilia/5863A2E93FFF454C/crazy_people/data/leitos.br.RData")


load("serie.historica.covid.br.RData")
condicao<-dados.cota$state=="TOTAL"
dados.cota<-dados.cota[condicao==F,] #retira totalizacoes

condicao2<-dados.cota$ibgeID%in%head(sort(dados.cota$ibgeID),2574) #retira casos sem localizacao definida
dados.cota<-dados.cota[condicao2==F,]
dados.cota$newCases[which(dados.cota$newCases<0)] <- 0 # coloca valores negativos igual a zero
dados.cota$newDeaths[which(dados.cota$newDeaths<0)] <- 0 # coloca valores negativos igual a zero

IDS<-sort(unique(dados.cota$ibgeID)) 

capitais_suscode <- c("110020", "120040", "130260", "140010", "150140", "160030", "172100", "211130", "221100", "230440", "240810", "250750", "261160", "270430", "280030", "292740",
                      "310620", "320530", "330455", "355030", "410690", "420540", "431490", "500270", "510340", "520870", "530010")


#### Dados internacao
internacao <- read.csv2("/media/cecilia/5863A2E93FFF454C/crazy_people/data/SRAGHospitalizado_2020_09_14.csv")
internacao_all <- internacao[, c(71, 8, 9, 26, 27)]
internacao_all$DT_INTERNA <- as.Date(internacao_all$DT_INTERNA, "%d/%m/%Y")
Date1<-as.Date("2020-02-25")  
Date2<-as.Date("2020-09-12")
internacao_all <- internacao_all[internacao_all$DT_INTERNA >= Date1 & internacao_all$DT_INTERNA <= Date2,]
edge_intern <- data.frame(with(internacao_all, table(CO_MUN_NOT, CO_MUN_RES)))

edge_intern <- edge_intern[which(edge_intern$Freq>0),]
#write.csv(edge_intern, file="edgelist_internacoes.csv", row.names = FALSE)
colnames(edge_intern) <- c("to", "from", "weight")
edge_intern <- edge_intern[,c(2,1,3)]
g <- as.matrix(edge_intern)

g1=graph.edgelist(g[,1:2])
E(g1)$weight=as.numeric(g[,3])
indegree <- degree(g1, mode="in", loops=FALSE)
outdegree <- degree(g1, mode="out", loops=FALSE)
weight_indegree <- strength(g1, mode="in", loops=FALSE)
weight_outdegree <- strength(g1, mode="out", loops=FALSE)
weight_alldegree <- strength(g1, mode="all")
closeness <- closeness(g1, mode="all")
betweenness_g <- betweenness(g1)
entropia <- diversity(g1)
names(weight_indegree) == names(weight_outdegree)
degrees_intercacao <- data.frame(city=names(weight_indegree), indegree=indegree, outdegree=outdegree, weight_indegree=weight_indegree, weight_outdegree=weight_outdegree, closeness=closeness, betweenness_g=betweenness_g, entropia=entropia)

dados_setembro <- read.csv("/media/cecilia/5863A2E93FFF454C/crazy_people/data/covid_hosp_cities2020-09-12.csv")

joined_degrees_leitos <- merge(degrees_intercacao, leitos.br, by.x = "city", by.y = "cod", all.x = TRUE, all.y = TRUE)
joined_degrees_leitos <- joined_degrees_leitos[,-15]


joined_degrees_leitos_obitos <- merge(dados_setembro, joined_degrees_leitos, by.y = "City", by.x = "mun_id", all.x = FALSE, all.y = FALSE)

joined_degrees_leitos_obitos$flux <- (joined_degrees_leitos_obitos$indegree - joined_degrees_leitos_obitos$outdegree)

joined_degrees_leitos_obitos$scaledflux <- scale(joined_degrees_leitos_obitos$flux, center = TRUE, scale = TRUE)
joined_degrees_leitos_obitos$flux[which(!is.finite(joined_degrees_leitos_obitos$flux))] <- 0

joined_degrees_leitos_obitos$closeness[which(!is.finite(joined_degrees_leitos_obitos$closeness))] <- 0
joined_degrees_leitos_obitos$betweenness_g[which(!is.finite(joined_degrees_leitos_obitos$betweenness_g))] <- 0

joined_degrees_leitos_obitos$closenessnorm <- joined_degrees_leitos_obitos$closeness/joined_degrees_leitos_obitos$pop.x
joined_degrees_leitos_obitos$betweennessnorm <- joined_degrees_leitos_obitos$betweenness_g/joined_degrees_leitos_obitos$pop.x

joined_degrees_leitos_obitos$weightflux <- (joined_degrees_leitos_obitos$weight_indegree - joined_degrees_leitos_obitos$weight_outdegree)
joined_degrees_leitos_obitos$normout <-joined_degrees_leitos_obitos$outdegree/joined_degrees_leitos_obitos$pop.x
joined_degrees_leitos_obitos$normout[which(!is.finite(joined_degrees_leitos_obitos$normout))] <- 0

joined_degrees_leitos_obitos$normin <-joined_degrees_leitos_obitos$weight_indegree/joined_degrees_leitos_obitos$pop.x
joined_degrees_leitos_obitos$normin[which(!is.finite(joined_degrees_leitos_obitos$normin))] <- 0

joined_degrees_leitos_obitos$normpop <- (joined_degrees_leitos_obitos$indegree - joined_degrees_leitos_obitos$outdegree)/joined_degrees_leitos_obitos$pop.x

joined_degrees_leitos_obitos$normean <- (joined_degrees_leitos_obitos$indegree - joined_degrees_leitos_obitos$outdegree)/((joined_degrees_leitos_obitos$indegree + joined_degrees_leitos_obitos$outdegree)/2)
joined_degrees_leitos_obitos$normean[which(!is.finite(joined_degrees_leitos_obitos$normean))] <- 0


joined_degrees_leitos_obitos$scaledwflux <- scale(joined_degrees_leitos_obitos$weightflux, center = TRUE, scale = TRUE)
joined_degrees_leitos_obitos$scaledindegree <- scale(joined_degrees_leitos_obitos$weight_indegree, center = TRUE, scale = TRUE)
joined_degrees_leitos_obitos$scaledoutdegree <- scale(joined_degrees_leitos_obitos$weight_outdegree, center = TRUE, scale = TRUE)
joined_degrees_leitos_obitos$logdeaths <- log(joined_degrees_leitos_obitos$last_available_deaths) 
joined_degrees_leitos_obitos$logdeaths[which(!is.finite(joined_degrees_leitos_obitos$logdeaths))] <- 0
joined_degrees_leitos_obitos$logindegree <- log(joined_degrees_leitos_obitos$indegree) 
joined_degrees_leitos_obitos$logindegree[which(!is.finite(joined_degrees_leitos_obitos$logindegree))] <- 0

joined_degrees_leitos_obitos$logoutdegree <- log(joined_degrees_leitos_obitos$outdegree) 
joined_degrees_leitos_obitos$logoutdegree[which(!is.finite(joined_degrees_leitos_obitos$logoutdegree))] <- 0


joined_degrees_leitos_obitos$logwflux <- log(joined_degrees_leitos_obitos$weightflux) 
joined_degrees_leitos_obitos$logwflux[which(!is.finite(joined_degrees_leitos_obitos$logwflux))] <- 0
joined_degrees_leitos_obitos$regiao <- as.factor(joined_degrees_leitos_obitos$Region) 
levels(joined_degrees_leitos_obitos$regiao) <- c("North", "Northeast", "Southeast", "South", "Midwest")


#joined_degrees_leitos_obitos[is.na(joined_degrees_leitos_obitos)] <- 0

plot(leitos.x~ log(pop.x), data=joined_degrees_leitos_obitos)
log(joined_degrees_leitos_obitos$deaths_per_100k_inhabitants)

boxplot(closenessnorm ~ joined_degrees_leitos_obitos$Region, data=joined_degrees_leitos_obitos)

abline(v=log(14))

summary(lm(logdeaths~ closenessnorm, data=joined_degrees_leitos_obitos))
summary(lm(logdeaths~ normin, data=joined_degrees_leitos_obitos))
summary(lm(logdeaths~ logoutdegree, data=joined_degrees_leitos_obitos))

summary(lm(normin ~ leitos_inc, data=joined_degrees_leitos_obitos))


plot(x=(joined_degrees_leitos_obitos$deaths/joined_degrees_leitos_obitos$pop)*100000, y=joined_degrees_leitos_obitos$deaths_per_100k_inhabitants)
ggpairs(joined_degrees_leitos_obitos, columns = c(3,4,5,6,12,13,14,23,25))

ggplot(joined_degrees_leitos_obitos,aes(y=deaths,x=outdegree,color=factor(regiao)))+geom_point()+xlim(0,25)+ylim(0,1200)

ggplot(joined_degrees_leitos_obitos,aes(y=logdeaths,x=logoutdegree,color=factor(regiao)))+geom_point(aes(size=pop))

ggplot(joined_degrees_leitos_obitos,aes(y=last_available_deaths,x=leitos.x,size=pop.x, color=factor(regiao)))+geom_point()+geom_smooth(method=lm, se=FALSE)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggplot(joined_degrees_leitos_obitos,aes(y=last_available_deaths,x=leitos.x,size=pop.x, color=factor(regiao)))+geom_point()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black")) + scale_y_log10() + scale_x_log10()+geom_smooth(method=lm, se=FALSE)+
                  ylab("Total number of deaths (log)") + xlab("Number of ICU beds (log)") + guides(size=FALSE) + theme(legend.title=element_blank())


edge_capitais <- data.frame(with(intercacao_capitais, table(CO_MUN_RES, CO_MUN_NOT)))
edge_capitais <- edge_capitais[which(edge_capitais$Freq>0),]
edge_capitais_list <- split(edge_capitais, list(edge_capitais$CO_MUN_NOT))



intercacao_capitais$categoria <- rep(NA, nrow(intercacao_capitais))


intercacao_capitais$categoria[which(intercacao_capitais$CO_MUN_NOT==intercacao_capitais$CO_MUN_RES)] <- "residente"
intercacao_capitais$categoria[which(intercacao_capitais$CO_MUN_NOT!=intercacao_capitais$CO_MUN_RES)] <- "nao_residente"
intercacao_capitais$categoria <- as.factor(intercacao_capitais$categoria)
#intercacao_capitais$DT_INTERNA <- as.Date(intercacao_capitais$DT_INTERNA, "%d/%m/%Y")

intercacao_capitais <- na.omit(intercacao_capitais)
table_internacoes <- data.frame(with(intercacao_capitais, table(DT_INTERNA, ID_MUNICIP, categoria)))
table_internacoes <- table_internacoes[!(table_internacoes$DT_INTERNA == ""), ]
table_internacoes <- table_internacoes[order(as.Date(table_internacoes$DT_INTERNA, format="%d/%m/%Y")),]
table_internacoes$DT_INTERNA <- as.Date(table_internacoes$DT_INTERNA, "%d/%m/%Y")
Date1<-as.Date("2020-02-25")  
Date2<-as.Date("2020-08-01")
TEST <- table_internacoes[table_internacoes$DT_INTERNA >= Date1 & table_internacoes$DT_INTERNA <= Date2,]

p <- ggplot(data = TEST, aes(x=DT_INTERNA, y=Freq)) 
p <- p + geom_line(aes(color = categoria), size = 1) 
p <- p + facet_wrap( ~ ID_MUNICIP, scales="free")
p <- p + ylab("Número de internações") + xlab("data")
#p <- p + guides(fill=guide_legend(title="Sexo"))
p 

table_internacoes2 <- data.frame(with(intercacao_capitais, table(DT_INTERNA, CO_MUN_NOT, categoria)))
table_internacoes2 <- table_internacoes2[!(table_internacoes2$DT_INTERNA == ""), ]
table_internacoes2 <- table_internacoes2[order(as.Date(table_internacoes2$DT_INTERNA, format="%d/%m/%Y")),]
table_internacoes2$DT_INTERNA <- as.Date(table_internacoes2$DT_INTERNA, "%d/%m/%Y")
Date1<-as.Date("2020-02-25")  
Date2<-as.Date("2020-08-01")
table_internacoes2 <- table_internacoes2[table_internacoes2$DT_INTERNA >= Date1 & table_internacoes2$DT_INTERNA <= Date2,]

inter_estados_list <- split(table_internacoes2, list(table_internacoes2$CO_MUN_NOT, table_internacoes2$categoria))
inter_estados_list <- lapply(inter_estados_list, function(df){df[order(df$DT_INTERNA),]})
inter_estados_list_sma <- lapply(inter_estados_list, function(df){SMA(df$Freq, n=7)})
inter_estados_sma <- Map(cbind, inter_estados_list, sma = inter_estados_list_sma)
inter_estados_sma_dataf <- data.frame(do.call("rbind",inter_estados_sma))
inter_estados_sma_dataf$cidade <- intercacao_capitais$ID_MUNICIP[match(inter_estados_sma_dataf$CO_MUN_NOT, intercacao_capitais$CO_MUN_NOT)]

p <- ggplot(data = inter_estados_sma_dataf, aes(x=DT_INTERNA, y=sma))
p <- p + geom_line(aes(color = categoria), size = 0.7) 
p <- p + facet_wrap( ~ cidade, scales="free")
p <- p + ylab("Número de internações") + xlab("data")
#p <- p + guides(fill=guide_legend(title="Sexo"))
p <- p + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 12))  


ggsave(filename="interna.pdf", plot=p, width=20, height=11.25, units="in")

#write.csv(inter_estados_sma_dataf, file="sma_internacoes.csv", row.names = FALSE)

matriz_capitais_casos_sma_date <- as.data.frame(t(matriz_capitais_casos_sma))
colnames(matriz_capitais_casos_sma_date) <- capitais_suscode 
matriz_capitais_casos_sma_date$date <- as.Date(unique(dados.cota$date))
melt_matriz_capitais_casos_sma_date <- melt(matriz_capitais_casos_sma_date, id.vars = "date", measure.vars=names(matriz_capitais_casos_sma_date[,1:26]))
melt_matriz_capitais_casos_sma_date$DT_INTERNA <- melt_matriz_capitais_casos_sma_date$date
melt_matriz_capitais_casos_sma_date$CO_MUN_NOT <- melt_matriz_capitais_casos_sma_date$variable
melt_matriz_capitais_casos_sma_date$categoria <- rep("casos_capital", nrow(melt_matriz_capitais_casos_sma_date))
melt_matriz_capitais_casos_sma_date$sma <- melt_matriz_capitais_casos_sma_date$value

dados2 <- data.frame(rbind(inter_estados_sma_dataf[,c(1,2,3,5)], melt_matriz_capitais_casos_sma_date[,c(4,5,6,7)]))
dados2$cidade <- intercacao_capitais$ID_MUNICIP[match(dados2$CO_MUN_NOT, intercacao_capitais$CO_MUN_NOT)]


p <- ggplot(data = dados2, aes(x=DT_INTERNA, y=sma))
p <- p + geom_line(aes(color = categoria), size = 1) 
#p <- p + geom_line(data = dados2, aes(x = DT_NOTIFIC, y = value), color = "black") +
p <- p + facet_wrap( ~ cidade, scales="free")
p <- p + ylab("Número de internações e casos") + xlab("data")
p 




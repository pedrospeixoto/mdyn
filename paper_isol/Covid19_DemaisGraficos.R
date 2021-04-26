packages <- c("tidyverse", "modeest", "geobr", "ggplot2", "crul", "ggpubr", "gridExtra" )
install.packages(setdiff(packages, rownames(installed.packages())))



library('ggpubr')
library("ggrepel")
library("gridExtra")

library("readxl")

library(tidyverse)

library("gridBase")
library(plyr)
library(cowplot)
library (ROCR) 
library(caret) 

library(ggrepel)


########################################
## Gráfico do Brasil
########################################
library("modeest")
library("ggplot2")

geo_ufs <- geobr::read_state(code_state = 'all', year = 2020)
head(geo_ufs)


maps <- ggplot(geo_ufs) +
    geom_sf(aes(geometry = geom, fill = as.character(name_region))) +
    ggrepel::geom_label_repel(size=9,data = geo_ufs,aes(label = abbrev_state, geometry = geom),
    stat = "sf_coordinates", min.segment.length = 0 )+
    theme_linedraw()+
    theme(axis.text.x=element_text(size=16), axis.title.x=element_text(size=18,face="bold"))+
    theme(axis.text.y=element_text(size=16), axis.title.y=element_text(size=18,face="bold"))+
    theme(plot.title=element_text(size=28))+
    theme(legend.position = "bottom", legend.text = element_text(size = 18)) +
    labs(x=element_blank(), y=element_blank()) +
    labs(fill = "Region") +
    theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white")) + 
    theme(legend.title = element_text(face = "bold", size = 20)) + 
    scale_fill_discrete(name="Region", breaks=c("Centro Oeste","Nordeste","Norte","Sudeste","Sul"), labels=c("Midwest","Northest","North", "Southeast", "South"))
 
ggsave(plot=maps, filename="Mapa.jpg", dpi="retina", width = 15,height = 10)




########################################
## Gráfico de Estagios, cidade de Aracaju
########################################
library("ggplot2")
library("ggpubr")

dadosGraf <- read.table("Aracaju_phases.txt", sep="\t", dec=".", header=TRUE)
head(dadosGraf)
 
limitey <- 35

grafico <- ggplot(dadosGraf, aes(x=Tempo, y=suavizados)) + 
    geom_line(size=2)+	
    coord_cartesian(ylim=c(0,limitey))+
    annotate("text", x=3.5, y = 29, label="S\nT\nA\nG\nE\n\n1", colour="blue", size = 7, fontface = 2) + 
    annotate("text", x=9, y = 29, label="S\nT\nA\nG\nE\n\n2", colour="blue", size = 7, fontface = 2) + 
    annotate("text", x=17, y = 29, label="S\nT\nA\nG\nE\n\n3", colour="blue", size = 7, fontface = 2) + 
    annotate("text", x=30.5, y = 29, label="S\nT\nA\nG\nE\n\n4", colour="blue", size = 7, fontface = 2) + 
    annotate("text", x=46, y = 29, label="S\nT\nA\nG\nE\n\n5", colour="blue", size = 7, fontface = 2) + 
    annotate("text", x=62.5, y = 29, label="S\nT\nA\nG\nE\n\n6", colour="blue", size = 7, fontface = 2) + 
    annotate("text", x=82.5, y = 29, label="S\nT\nA\nG\nE\n\n7", colour="blue", size = 7, fontface = 2) + 
    scale_y_continuous(breaks=seq(0,limitey,10))+		
    scale_x_continuous(limits=c(0,96),expand=c(0,0), breaks=seq(0,94,10))+		
    ggtitle("Aracaju")	+
    labs(x = "initial time: 15/03/2020") +
    labs(y = "smoothed daily cases") +
    grids(linetype = "dashed") +	
    geom_vline(aes(xintercept=7), color="blue", linetype="dashed", size=1.3)+ 	
    geom_vline(aes(xintercept=11), color="blue", linetype="dashed", size=1.3)+
    geom_vline(aes(xintercept=23), color="blue", linetype="dashed", size=1.3)+
    geom_vline(aes(xintercept=38), color="blue", linetype="dashed", size=1.3)+
    geom_vline(aes(xintercept=54), color="blue", linetype="dashed", size=1.3)+
    geom_vline(aes(xintercept=71), color="blue", linetype="dashed", size=1.3)+
    geom_vline(aes(xintercept=94), color="blue", linetype="dashed", size=1.3)+
    annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = +Inf, alpha=0.1) +
    annotate("rect", xmin = 11, xmax = 23, ymin = -Inf, ymax = +Inf, alpha=0.1) +
    annotate("rect", xmin = 38, xmax = 54, ymin = -Inf, ymax = +Inf, alpha=0.1) +
    annotate("rect", xmin = 71, xmax = 94, ymin = -Inf, ymax = +Inf, alpha=0.1) +
    theme_linedraw()+
    theme(legend.position = "bottom", legend.title=element_blank(), legend.text = element_text(size = 18)) +
    theme(axis.text.x=element_text(size=20), axis.title.x=element_text(size=24))+
    theme(axis.text.y=element_text(size=20), axis.title.y=element_text(size=24))+
    theme(plot.title=element_text(size=28, hjust = 0.5))+
    theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white") )



ggsave(plot=grafico, filename="Grafico_Estagios_Aracaju.jpg", dpi="retina", width = 20,height = 10)




#####################################################
# Gráfico duplo de Porto Alegre
#####################################################
library('dplyr')
library("tidyverse")
library("gridExtra")



################################
# Parâmetros de entrada
##############################
ttempo <- 8
numCasosPop <- 1000/12325232 	#Número de casos acumulados por fator populacional de 1 unico dia em SP
nPontosDif <- 3 			#Número de pontos que diferenciam a direção da curva (Subida / Descida)
percent.min <- 1.0		#Coeficiente que define a classificação como Platô (Poda superior)


###########################################################################################################
# Cria tabela de cores para padronização
###########################################################################################################
bgColors <- c(	"Green", 
			"Gold", 
			"Darkorange", 
			"Red", 
			"Gray25", 

			"violetred", 
			"lightsalmon2", 
			"gray80",

			"palevioletred2", 
			"palegreen4", 
			"tomato2", 
			"skyblue2", 
			"lightgoldenrod3",
			"deepskyblue",
			"darkgray",
			"darkolivegreen4",
			"darkmagenta",
			"seashell2",

			"grey70",
			"grey70",
			"grey70",
			"grey70",
			"grey70",
			"grey70",
			"grey70",
			"grey70",
			"grey70",
			"grey70",

			"palevioletred2",
			"lavenderblush2",
			"tomato2",
			"khaki4",
			"honeydew",
			"skyblue2",
			"chocolate2",
			"slateblue2",
			"seagreen1",
			"goldenrod3",

			"black" )

names(bgColors) <- c(	"Verde", 
				"Amarelo", 
				"Laranja", 
				"Vermelho", 
				"Lockdown", 

				"Upward", 
				"Plateau", 
				"Downward", 

				"Upward 01", 
				"Upward 02", 
				"Upward 03", 
				"Upward 04", 
				"Upward 05",
				"Upward 06", 
				"Upward 07", 
				"Upward 08", 
				"Upward 09", 
				"Upward 10",

				"Downward 01",
				"Downward 02",
				"Downward 03",
				"Downward 04",
				"Downward 05",
				"Downward 06",
				"Downward 07",
				"Downward 08",
				"Downward 09",
				"Downward 10",

				"Plateau 01",
				"Plateau 02",
				"Plateau 03",
				"Plateau 04",
				"Plateau 05",
				"Plateau 06",
				"Plateau 07",
				"Plateau 08",
				"Plateau 09",
				"Plateau 10",

				"After Peak")

###########################################################################################################
# Carrega objetos:
#
#	- cidades  		# lista de cidades
#	- maximos		# Tabela de valores máximos por cidade
#	- cidadeStatus	# Tabela de lockdowns e mudanças de cores do plano São Paulo
#	- dados		# Dados por cidades
###########################################################################################################
load("Covid19.RData")
###########################################################################################################

	#Faz gráfico apenas para a cidade de Porto Alegre
	i <- 34


	#determina o tempo inicial com base nos casos acumulados por 100k habitantes
	ttempo <- min(dados[	dados$UF == cidades[i, "UF"] & 
					dados$Cidade == cidades[i, "Cidade"] & 
					(dados$CasosSuavAcumulado / dados$Populacao) >=  numCasosPop, "Tempo"])
	
	#Determina o tempo de ocorrência do pico
	tempoRef <- min(dados[	dados$UF == cidades[i, "UF"] & 
					dados$Cidade == cidades[i, "Cidade"] & 
					round(dados$CasosSuav, 8) == round(maximos[maximos$UF == cidades[i, "UF"] & maximos$Cidade == cidades[i, "Cidade"], "MaxCasosSuav"], 8), "Tempo"])


	print(paste("UF:", cidades[i, "UF"], "| Cidade:", cidades[i, "Cidade"], "| Tempo max:", maximos[maximos$UF == cidades[i, "UF"] & maximos$Cidade == cidades[i, "Cidade"], "MaxTempo"]))
	flush.console()

	dados_cidade <- dados[	dados$UF == cidades[i, "UF"] & 
					dados$Cidade == cidades[i, "Cidade"] & 
					dados$Tempo >= ttempo, ]
		
	dados_cidade$CasosDivPop <- (dados_cidade$CasosSuav/dados_cidade$Populacao)*100000

	dados_cidade$Status <- "Downward"
	dados_cidade$Status[dados_cidade$CasosSuav > percent.min * maximos[1,"MaxCasosSuav"]] <- "Plateau"
	dados_cidade$Status[dados_cidade$Tempo < tempoRef] <- "Upward"
	

	#Seta todo o intervalo entre os platos como um único plato
	results_plato <- dados_cidade %>% filter (Status == "Plato")

	if (nrow(results_plato) > 0) {
		p.ini <- min(results_plato$Periodo)
		p.fim <- max(results_plato$Periodo)
		dados_cidade$Status[dados_cidade$Periodo >= p.ini & dados_cidade$Periodo <= p.fim] <- "Plateau"
	}

	#Filtra ocorrências da cidade e seta seus respectivos status (Cores de Fases)
	tmpStatus <- data.frame(arrange(filter(cidadeStatus, Estado == cidades[i, "UF"], Cidade == cidades[i, "Cidade"], !is.na(TempoIni)), TempoIni))

	dados_cidade$Status2 <- dados_cidade$Status

	if (nrow(tmpStatus) > 0){
		for (j in 1:nrow(tmpStatus))
		{
			dados_cidade[dados_cidade$Tempo >= tmpStatus[[j,"TempoIni"]] & dados_cidade$Tempo <= tmpStatus[[j,"TempoFim"]], "Status2"] = tmpStatus[[j,"Status"]]			
		}
	}
	
	
	#Distinção das diferentes fases
	dados_cidade$Status3 <- NA
	dados_cidade[1, "Status3"] <- ""
	statusAnt <- ""
	
	for (j in 2:nrow(dados_cidade)){
		if (dados_cidade[j, "CasosSuav"] > dados_cidade[j-1, "CasosSuav"]){			
			status <- "Upward"
		} else if (dados_cidade[j, "CasosSuav"] < dados_cidade[j-1, "CasosSuav"]){
			status <- "Downward"
		} else {
			status <- "Plateau"
		}

		if( j == 2) {
			statusAnt <- status
		}

		#print(paste("j =", j, "Status proposto =", status, "Status anterior =", statusAnt, "Valor = ", dados_cidade[j, "CasosSuav"], "Valor Anterior =", dados_cidade[j-1, "CasosSuav"] ))

		if (status != statusAnt){

			altera <- TRUE
			nAnteriores <- j-nPontosDif+1

			if (nAnteriores <= 0){
				nAnteriores <- 1
			}
	
			if (j > 2) {

				if (nAnteriores == 1) {
					pIni <- 2
				} else {
					pIni <- nAnteriores
				}

				if (status == "Upward"){
					for (k in (j-1):pIni ){
						if (dados_cidade[k, "CasosSuav"] < dados_cidade[k-1, "CasosSuav"]) {
							status <- statusAnt 
							altera <- FALSE
							break
						}
					}
				} else if (status == "Downward"){
					for (k in (j-1):pIni ){
						if (dados_cidade[k, "CasosSuav"] > dados_cidade[k-1, "CasosSuav"]) {
							status <- statusAnt 
							altera <- FALSE
							break
						}
					}
				} else{
					for (k in (j-1):pIni ){
						if (dados_cidade[k, "CasosSuav"] != dados_cidade[k-1, "CasosSuav"]) {
							status <- statusAnt 
							altera <- FALSE
							break
						}
					}

				}
			} 

		} else {
			altera <- FALSE
		}

		#print(paste("J =", j, "Status =", status, "StatusAnt =", statusAnt, "Altera =", altera))

		if (altera){
			for (k in (j-1):nAnteriores){
				dados_cidade[k, "Status3"] <- status
			}
		} 

		dados_cidade[j, "Status3"] <- status

		statusAnt <- status
	}

	dados_cidade[1, "Status3"] <- dados_cidade[2, "Status3"]


	#Enumera as diferentes fases de cada status
	controle <- c(0,0,0)
	names(controle) <- c("Upward", "Downward", "Plateau")

	statusAnt <- dados_cidade[1,"Status3"]
	controle[dados_cidade[1,"Status3"]] <- controle[dados_cidade[1, "Status3"]] + 1
	
	if (dados_cidade[1,"Status3"] != "Downward") {
		dados_cidade[1, "Status3"] <- paste(dados_cidade[1, "Status3"], str_sub(paste("0", controle[dados_cidade[1, "Status3"]], sep=""), -2, -1))
	}

	for (j in 2:nrow(dados_cidade)){
	
		if (dados_cidade[j, "Status3"] != statusAnt){
			controle[dados_cidade[j, "Status3"]] <- controle[dados_cidade[j, "Status3"]] + 1
		}

		statusAnt <- dados_cidade[j,"Status3"]

		if (dados_cidade[j,"Status3"] != "Downward") {
			dados_cidade[j, "Status3"] <- paste(dados_cidade[j,"Status3"], str_sub(paste("0", controle[dados_cidade[j,"Status3"]], sep=""), -2, -1))
		}
	}


	dados_cidade$Status3[dados_cidade$Tempo > tempoRef] <- "After Peak"

	grafStatus <- 	ggplot(dados_cidade) +
				geom_point(aes(x=Tempo, y=CasosDivPop, color=Status3), size=2) +				
				labs(title = paste(cidades[i, "Cidade"])) +
				labs(y="incidence/100k", x=paste("Initial time", ":", format.Date(dados_cidade[1,"Data"], "%d/%m/%Y"))) +
   				scale_x_continuous(limits=c(0,185),expand=c(0,0), breaks=seq(0,180,20))+		
				scale_color_manual(values = bgColors) + 
     			      theme_linedraw()+
   	  	            theme(legend.position = "bottom", legend.title=element_blank()) + 
				theme(legend.text = element_text(size = 28)) +
				theme(axis.text.x=element_text(size=20), axis.title.x=element_text(size=22))+
				theme(axis.text.y=element_text(size=20), axis.title.y=element_text(size=22))+
				theme(plot.title=element_text(size=28, hjust = 0.5))+
    			      theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white") )


	
	#Insere cores de fundo conforme ocorrências da cidade
	if (nrow(tmpStatus) > 0){
		for (j in 1:nrow(tmpStatus)) {
			grafStatus <- grafStatus + geom_rect(data=data.frame(xmin=tmpStatus[[j,"TempoIni"]],  xmax=tmpStatus[[j,"TempoFim"]] , ymin=-Inf, ymax=Inf), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=bgColors[tmpStatus[[j,"Status"]]], alpha=0.15)
		}
	}




	results_cidade1 <-  dados_cidade %>% filter (Tempo <=  tempoRef)
	results_cidade1 <-  results_cidade1 %>% filter (Status3 != "Downward")

	
	maxCas <- max(dados_cidade$CasosSuav)
	grafCoefAngMod1 <-  ggplot(results_cidade1, aes(x=MediaIsolamentoRelativo, y=round(CasosDivPop, 2), color=Status3)) + #, label=TempoIni)) +
   				  geom_point(size=4) +
    				  scale_x_continuous(limits=c(0.04,0.38),expand=c(0,0), breaks=seq(0.04,0.36,0.04))+		
				  labs(title = paste(cidades[i, "Cidade"])) +
				  labs(size=14, y="incidence/100k", x=paste("isolation index")) +
				  scale_color_manual(values = bgColors) +
				  theme_linedraw()+
			  	  theme(legend.position = "bottom", legend.title=element_blank()) + 
				  theme(legend.text = element_text(size = 28)) +
				  theme(axis.text.x=element_text(size=20), axis.title.x=element_text(size=22))+
				  theme(axis.text.y=element_text(size=20), axis.title.y=element_text(size=22))+
				  theme(plot.title=element_text(size=28, hjust = 0.5))+
				  theme(panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white") )


	a <- grid.arrange(grafStatus, grafCoefAngMod1, nrow = 1)
	ggsave(plot=a, filename=paste(cidades[i, "UF"], cidades[i, "Cidade"], "_S", 6, "J", 3, "_GrafCasosRel.jpg", sep=""), dpi="retina", width = 20,height = 10)
	
      rm(grafCoefAngMod1, dados_cidade, results_cidade1, tempoRef, tmpStatus)


packages <- c("dplyr", "tidyverse")
install.packages(setdiff(packages, rownames(installed.packages())))


library('dplyr')
library("tidyverse")


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

			"chartreuse3", 
			"palegreen4", 
			"skyblue", 
			"palevioletred2", 
			"lightgoldenrod3",
			"deepskyblue",
			"darkgray",
			"tomato2",
			"darkmagenta",
			"seashell2",

			"palevioletred2",
			"brown1",
			"tomato2",
			"khaki4",
			"darkgoldenrod1",
			"skyblue2",
			"brown1",
			"slateblue2",
			"seagreen1",
			"darksalmon",

			"palevioletred2",
			"lavenderblush2",
			"tomato2",
			"khaki4",
			"honeydew",
			"skyblue2",
			"chocolate2",
			"slateblue2",
			"seagreen1",
			"goldenrod3")

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
				"Plateau 10")


###########################################################################################################
# Carrega objetos:
#
#	- cidades  		# lista de cidades
#	- maximos		# Tabela de valores máximos por cidade
#	- cidadeStatus	# Tabela de lockdowns e mudanças de cores do plano São Paulo
#	- dados			# Dados por cidades
###########################################################################################################
load("Covid19.RData")
###########################################################################################################


#Itera sobre a lista de cidades
for (i in 1:nrow(cidades)[1])
{
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




	results_cidade1 <-  dados_cidade %>% filter (Tempo <=  tempoRef)
	results_cidade1 <-  results_cidade1 %>% filter (Status3 != "Downward")


	maxCas <- max(dados_cidade$CasosSuav)
	grafCoefAngMod1 <-  ggplot(results_cidade1, aes(x=MediaIsolamentoRelativo, y=round(CasosDivPop, 2), color=Status3)) + #, label=TempoIni)) +
   				  geom_point(size=5) +		  
    				  scale_x_continuous(limits=c(0.04,0.38),expand=c(0,0), breaks=seq(0.04,0.36,0.04))+		
   				  scale_y_continuous(breaks=seq(0,15,1))+
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



	ggsave(plot=grafCoefAngMod1 , filename=paste(cidades[i, "Cidade"], "_S", 6, "J", 3, "_GrafCasosRel.jpg", sep=""), dpi="retina", width = 12,height = 10)
	
      rm(grafCoefAngMod1, dados_cidade, results_cidade1, tempoRef, tmpStatus)
}



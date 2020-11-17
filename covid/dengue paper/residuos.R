#ANALISE DE RESIDUOS PARA O MODELO LINEAR GERAL
library(xtable)

resanalysis <- function(model){
  #TEMA DO GRAFICO (PADRAO)
  tema <- list(theme_linedraw())
  titulos <- theme(strip.text = element_text(size = 18), axis.text = element_text(size = 12,
                                                                                 color = "black"), 
                  axis.title = element_text(size = 16), legend.text = element_text(size = 14),
                  plot.title = element_text(size = 18,face = "bold"),
                  legend.title = element_text(size = 16,face = "bold"), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), panel.border = element_blank(),
                  panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                  legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                  legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
  
  #RESIDUOS, DISTANCIA DE COOK E PONTOS INFLUENTES
  res <- rstandard(model)
  fit <- model$fitted.values
  cook <- cooks.distance(model)
  influ <- influence(model)$hat
   
  # #TESTES DAS SUPOSISOES DO MODELO
  if(length(res) > 5000)
    shap <- shapiro.test(x = res[sample(1:length(res),5000,F)])$p.value
  else
    shap <- shapiro.test(x = res)$p.value
  durbin <- durbinWatsonTest(model)$p
  # if(var == "BP")
  #ncv <- ncvTest(model)$p
  # else
  #   ncv <- NA
  # #TABELA COM OS TESTES DAS SUPOSICOES DO MODELO
  name <- c("Shapiro-Wilkes","Durbin-Watson")#"Breusch-Pagan")
  type <- c("Normalidade dos erros","Independência do erros")#,"Homoscedasticidade dos erros")
  pvalue <- c(shap,durbin)#,ncv)
  tab <- data.frame(name,type,pvalue)
  colnames(tab) <- c("Teste","Tipo","p-valor")
  print(xtable(tab))
  
  #GRAFICO DE RESIDUOS
  p1 <- {ggplot(data.frame(fit,res),aes(x = fit,y = res)) + geom_point() + tema + titulos + geom_hline(yintercept = c(-2,0,2)) +
  xlab("Fitted") + ylab("Standardized Residuals") + ggtitle("Standardized Residuals versus Fitted Values")}
  
  #QQ-PLOT
  g1 <- ggplot(data.frame(res), aes(sample = res))
  y <- quantile(res[!is.na(res)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p2 <- {g1 + stat_qq() + tema + geom_abline(slope = slope, intercept = int) + 
  titulos +
  xlab("Theoretical Quantiles") + ylab("Standardized Residuals Quantiles") + ggtitle("QQ-Plot")}
  
  #GRAFICO DAS DISTANCIAS DE COOK
  obs <- 1:length(res)
  g3 <- data.frame(1:length(res),cook)
  colnames(g3) <- c("obs","cook")
  p3 <- {ggplot(g3,aes(x = obs,y = cook)) + geom_point() + tema + titulos + xlab("Observação") + 
           ylab("Distância de Cook") + ggtitle("Distância de Cook") + geom_hline(yintercept = 4/length(res)) +
  annotate("text",size = 3,x = obs[rank(cook) == length(res)]+log(log(length(res)))*(0.75),y = cook[rank(cook) == length(res)],label = as.character(obs[rank(cook) == length(res)])) +
  annotate("text",size = 3,x = obs[rank(cook) == length(res)-1]+log(log(length(res)))*(0.75),y = cook[rank(cook) == length(res)-1],label = as.character(obs[rank(cook) == length(res)-1]))+
  annotate("text",size = 3,x = obs[rank(cook) == length(res)-2]+log(log(length(res)))*(0.75),y = cook[rank(cook) == length(res)-2],label = as.character(obs[rank(cook) == length(res)-2]))}
  
  #GRAFICO DE INFLUENCIA
  g4 <- data.frame(1:length(res),influ)
  colnames(g4) <- c("obs","influ")
  p4 <- {ggplot(g4,aes(x = obs,y = influ)) + geom_point() + tema + titulos + xlab("Observação") + 
           ylab("Hat matrix diagonal") + ggtitle("Influência")+
           annotate("text",size = 3,x = obs[rank(influ) == length(res)]+log(log(length(res)))*(0.75),y = influ[rank(influ) == length(res)],label = as.character(obs[rank(influ) == length(res)])) +
           annotate("text",size = 3,x = obs[rank(influ) == length(res)-1]+log(log(length(res)))*(0.75),y = influ[rank(influ) == length(res)-1],label = as.character(obs[rank(influ) == length(res)-1]))+
           annotate("text",size = 3,x = obs[rank(influ) == length(res)-2]+log(log(length(res)))*(0.75),y = influ[rank(influ) == length(res)-2],label = as.character(obs[rank(influ) == length(res)-2]))}
  
  p <- grid.arrange(p1,p2,nrow = 1,ncol = 2)
  r <- list(p)
  names(r) <- c("plot")
  return(r)
}

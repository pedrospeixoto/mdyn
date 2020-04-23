###########################################
######Relatório Índice de Isolamento#######
######Abril de 2020                 #######
######Diego Marcondes               #######
######dmarcondes@ime.usp.br         #######
###########################################

#wd
setwd("~/mdyn")

#libraries
library(ggplot2)
library(tidyverse)
library(lubridate)
library(data.table)
library(DescTools)
library(rgdal)
source("./Rcodes/utils.R")
#options(encoding = "Latin1")
#rc <- colorRampPalette(colors = c("red", "green"), space = "Lab")(180)

titles <- theme(strip.text = element_text(size = 12), 
                axis.text = element_text(size = 12,color = "black"), axis.title = element_text(size = 14), 
                legend.text = element_text(size = 14),
                legend.title = element_text(size = 14), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
themes <- list(theme_linedraw())

#Input
ini_quar <- "2020-03-01"
end_quar <- "2020-04-13"
dias_quar <- seq.Date(from = ymd(ini_quar),to = ymd(end_quar),by = 1)
dias_padrao_pan <- seq.Date(from = ymd("2020-03-29"),to = ymd(end_quar),by = 1)

ini_padrao <- "2019-06-01"
end_padrao <- "2020-02-29"
dias_padrao <- seq.Date(from = ymd(ini_padrao),to = ymd(end_padrao),by = 1)

dic_estados <- list('AC' = 'Acre','AL' = 'Alagoas','AP' = 'Amapá','AM' = 'Amazonas','BA' = 'Bahia',
                    'CE' = 'Ceará','DF' = 'Distrito Federal','ES' = 'Espírito Santo','GO' = 'Goiás',
                    'MA' = 'Maranhão','MT' = 'Mato Grosso','MS' = 'Mato Grosso do Sul',
                    'MG' = 'Minas Gerais','PA' = 'Pará','PB' = 'Paraíba','PR' = 'Paraná',
                    'PE' = 'Pernambuco','PI' = 'Piauí','RJ' = 'Rio de Janeiro',
                    'RN' = 'Rio Grande do Norte','RS' = 'Rio Grande do Sul','RO' = 'Rondônia',
                    'RR' = 'Roraima','SC' = 'Santa Catarina','SP' = 'São Paulo','SE' = 'Sergipe',
                    'TO' = 'Tocantins')
dic_shp <- list("AC" = "12MUE250GC_SIR","AL" = "27MUE250GC_SIR","AP" = "16MUE250GC_SIR",
                "AM" = "13MUE250GC_SIR","BA" = "29MUE250GC_SIR",
                "CE" = "23MUE250GC_SIR","DF" = "53MUE250GC_SIR","ES" = "32MUE250GC_SIR",
                "GO" = "52MUE250GC_SIR","MA" = "21MUE250GC_SIR","MT" = "51MUE250GC_SIR",
                "MS" = "50MUE250GC_SIR","MG" = "31MUE250GC_SIR","PA" = "15MUE250GC_SIR",
                "PB" = "25MUE250GC_SIR","PR" = "41MUE250GC_SIR","PE" = "26MUE250GC_SIR",
                "PI" = "22MUE250GC_SIR","RJ" = "33MUE250GC_SIR","RN" = "24MUE250GC_SIR",
                "RS" = "43MUE250GC_SIR","RO" = "11MUE250GC_SIR","RR" = "14MUE250GC_SIR",
                "SC" = "42MUE250GC_SIR","SP" = "35MUE250GC_SIR","SE" = "28MUE250GC_SIR",
                "TO" = "17MUE250GC_SIR")
dic_pronome <- list('AC' = 'do','AL' = 'de','AP' = 'do','AM' = 'do','BA' = 'da','CE' = 'do',
                    'DF' = 'do','ES' = 'do','GO' = 'de','MA' = 'do','MT' = 'do','MS' = 'do',
                    'MG' = 'de','PA' = 'do','PB' = 'da','PR' = 'do','PE' = 'de','PI' = 'do',
                    'RJ' = 'do','RN' = 'do','RS' = 'do','RO' = 'de','RR' = 'de','SC' = 'de',
                    'SP' = 'de','SE' = 'de','TO' = 'do')
estados <- names(dic_estados)
dir_data <- "/home/pedrosp/mdyn/dump/mdyn_params_mun_index/"
posfix_data <- "_Municipios_2019-06-01_2020-04-14_iso_index.csv"

#Organizando os dados de cada estado
for(s in estados){
  cat("Estado: ")
  cat(s)
  cat("\n")
  file <- paste(dir_data,toupper(dic_estados[s]),posfix_data,sep = "")
  dados <- data.frame(read.csv(file)) 
  dados$iso <- 1 - dados$left_home/dados$active_users_in_month
  saveRDS(object = dados,file = paste("./dataR/original_",s,".rds",sep = ""))
  dados <- dados %>% select("reg_name","iso","day")
  dados$day <- ymd(dados$day)
  dados$weekday <- weekdays(dados$day)
  dados$key <- paste(dados$reg_name,dados$weekday)
  dados <- dados %>% filter(reg_name != "-1")
  
  #Calcular padrão pre-pandemia
  padrao_pre <- data.table(dados %>% filter(day %in% dias_padrao))
  padrao_pre <- padrao_pre[,mean_pre := mean_trim(iso),by = key]    
  padrao_pre <- data.frame(padrao_pre[,sd_pre := sd_trim(iso),by = key])  
  padrao_pre <- padrao_pre %>% select("key","mean_pre","sd_pre") %>% unique()
  
  #Calcular padrao durante pandemia
  padrao_pan <- data.table(dados %>% filter(day %in% dias_quar))
  padrao_pan <- padrao_pan[,mean_pan := mean_pan(iso,day),by = key]    
  padrao_pan <- padrao_pan[,sd_pan := sd_pan(iso,day),by = key]
  padrao_pan <- data.frame(padrao_pan[,last_week := last_pan(iso,day),by = key])
  padrao_pan$mean_pan[!(padrao_pan$day %in% dias_padrao_pan)] <- NA
  padrao_pan$sd_pan[!(padrao_pan$day %in% dias_padrao_pan)] <- NA
  padrao_pan$last_week[!(padrao_pan$day %in% dias_padrao_pan)] <- NA
  padrao_pan$key <- paste(padrao_pan$reg_name,padrao_pan$day)
  padrao_pan <- padrao_pan %>% select("key","mean_pan","sd_pan","last_week") %>% unique()
  
  #Dias de quarentena
  dados <- dados %>% filter(day %in% dias_quar)
  
  #Juntando e calculando indice
  dados <- merge(dados,padrao_pre)
  dados$key <- paste(dados$reg_name,dados$day)
  dados <- merge(dados,padrao_pan)
  dados$weekday <- factor(dados$weekday)
  dados$key <- NULL
  
  #Calculando Indices
  dados$indice_pre <- indice_pre(dados$iso,dados$mean_pre,dados$sd_pre)
  dados$indice_pan <- indice_pan(dados$iso,dados$mean_pan,dados$sd_pan)
  dados$indice_week <- indice_week(dados$iso,dados$last_week,dados$sd_pan)
  
  #Salvando
  saveRDS(object = dados,file = paste("./dataR/",s,".rds",sep = ""))
  
  #Apagando
  rm(padrao_pan,padrao_pre,dados)
}

#Relatório por estado
for(s in estados){
  cat("Estado: ")
  cat(s)
  cat("\n")
  
  #Lendo os dados
  cat("Lendo os dados...")
  dados <- readRDS(file = paste("./Dados/",s,".rds",sep = ""))
  shp <- readOGR(paste("./Dados/maps/",tolower(s),"_municipios/",dic_shp[[s]],".shp",
                             sep = ""),verbose = F)
  shp <- fortify(shp,region = "NM_MUNICIP")
  cat(" OK!\n")
  
  #Gerando mapas de isolamento diários
  cat("Criando mapas...")
  mapas <- lapply(X = as.list(dias_quar),FUN = gerar_mapas(dados = dados,shp = shp,s = s))
  names(mapas) <- dias_quar
  tab <- lapply(X = mapas,FUN = function(x) x$tab)
  tabela <- rbindlist(l = tab)
  dir.create(path = paste("./Workspace/",s,sep = ""))
  write.csv(x = tabela,file = paste("./Workspace/",s,"/",s,".csv",sep = ""),row.names = F,
            fileEncoding = "UTF-8")
  cat("Ok!\n")
  
  #Gerando relatório
  cat("Gerando relatório...")
  
  dir.create(path = paste("./Workspace/",s,"/Relatorios",sep = ""))
  sink(paste("./Workspace/",s,"/Relatorios/",s,"_Relatório sobre isolamento durante a pandemia.tex",sep = ""),split = F)
  #Cabeçalho
  cat("\\documentclass[12pt,a4paper]{article}
  \\usepackage{marginnote}
  \\usepackage{wallpaper}
  \\usepackage{lastpage}
  \\usepackage[left=1.3cm,right=2.0cm,top=1.8cm,bottom=5.0cm,marginparwidth=3.4cm]{geometry}
  \\usepackage{amsmath}
  \\usepackage{amssymb}
  \\usepackage{float}
  \\usepackage{xcolor}
  \\usepackage{fancyhdr}
  \\usepackage[brazil]{babel}
  \\usepackage[latin1]{inputenc}
  \\usepackage[T1]{fontenc}
  \\usepackage{graphicx}
  \\usepackage{pstricks}
  \\usepackage{subfigure}
  \\usepackage{caption}
  \\captionsetup{justification=centering,labelfont=bf}
  \\usepackage{textcomp}
  \\setlength{\\headheight}{80pt}
  \\pagestyle{fancy}\\fancyhf{}
  \\renewcommand{\\headrulewidth}{0pt}
  \\setlength{\\parindent}{1cm}
  \\newcommand{\\tab}{\\hspace*{2em}}
  \\newcommand\\BackgroundStructure{
  \\setlength{\\unitlength}{1mm}
  \\setlength\\fboxsep{0mm}
  \\setlength\\fboxrule{0.5mm}
  \\put(10, 20pr){\fcolorbox{black}{gray!5}{\\framebox(155,247){}}}
  \\put(165, 20){\\fcolorbox{black}{gray!10}{\\framebox(37,247){}}}
  \\put(10, 262){\\fcolorbox{black}{white!10}{\framebox(192, 25){}}}
  \\put(175, 263){\\includegraphics[height=23mm,keepaspectratio]{}} 
  }
  \\fancyhead[L]{\\begin{tabular}{l r | l r}
  \\multicolumn{4}{c}{\\textbf{Relat\\'orio T\\'ecnico: \\'Indice de Isolamento Social - Pandemia da COVID-19}} \\\\
  \\multicolumn{4}{l}{P. Peixoto$^{1}$; D. Marcondes$^{1}$; C. Peixoto$^{1}$; S. Oliva$^{1}$; L. Queiroz$^{2}$, R. Gouveia$^{2}$, A. Delgado$^{2}$} \\\\")
  cat(paste("\\multicolumn{4}{l}{1. Instituto de Matemática e Estatística - USP; 2. In Loco} \\\\ \n"))
  cat(paste("\\multicolumn{4}{l}{\\textbf{Estado:}",dic_estados[s],"} \\\\ \n"))
  cat("\\end{tabular}}\n
  \\begin{document}\n")

  #Introdução
  cat("\\section{Índice de Isolamento Social durante a pandemia da COVID-19}\n")
  
  cat(paste("\nNeste relatório técnico apresentamos um Indice de Isolamento Social calculado diariamente durante a pandemia da COVID-19 para cada cidade do Estado",dic_pronome[s],dic_estados[s],"a partir de dados agregados de geolocalização de celulares."))
  cat(" Os dados foram fornecidos de forma anonimizada pela empresa In Loco (www.inloco.com.br), que desenvolve   \\textit{software development kits} (SDK) para aplicativos de celular. O conjunto de dados contém, para cada dia da pandemia, o número de usuários dos aplicativos com o sistema da empresa na cidade que visitaram uma localidade longe de sua residência. Esses dados são uma medida de isolamento, pois um alto número de usuários se movimentando para longe de suas residências é um indicativo de alta mobilidade de pessoas na cidade. Da mesma forma, um baixo número de usuários se movimentando para longe de suas residências é um indicativo de baixa mobilidade de pessoas na cidade, o que é uma evidência de eficácia de medidas de isolamento social.\n\n O índice desenvolvido nesse relatório é um número entre zero e um. Um índice igual a zero indica que o nível de mobilidade dos usuários está dentro do padrão para um dia comum, isto é, antes da pandemia da COVID-19. Já quanto mais próximo de um estiver o índice, mais abaixo do padrão está a mobilidade na cidade, o que é uma evidência de que há mais pessoas em isolamento. Assim, uma alta ao longo do tempo nesse índice representa um aumento no isolamento social dentro da cidade, enquanto que uma queda nesse índice representa uma diminuição no isolamento social na cidade. Mais informações sobre como esse índice é calculado são apresentadas no Apêndice.
\n\n")
  
  cat("\nO objetivo desse índice é subsidiar a tomada de decisões a nível estadual e municipal sobre políticas de isolamento durante a pandemia da COVID-19.\n")
  cat("\n\\textbf{IMPORTANTE:} Todas as decisões devem ser baseadas em informações de várias fontes, sendo esta apenas mais uma que busca auxiliar administradores públicos e privados nesse momento de crise. Ressaltamos que o valor do índice não representa a proporção de pessoas em isolamento social, mas trata-se de uma medida da intensidade do isolamento, podendo ser utilizado para determinar se esse isolamento aumentou ou diminuiu ao longo do tempo. Além disso, esse índice representa o isolamento de apenas uma amostra da população, e não de todos os seus habitantes.\n")

  cat("\n")
  cat(paste("\\section{Índice de Isolamento Social entre os dias",format.Date(ini_quar,"%d/%m/%Y"),"e",format.Date(end_quar,"%d/%m/%Y"),"}"))
  cat("\n")

  cat(paste("Abaixo apresentamos mapas com os índices calculados para os dias ",format.Date(ini_quar,"%d/%m/%Y")," a ", format.Date(end_quar,"%d/%m/%Y")," para as cidades do Estado ",dic_pronome[s]," ",dic_estados[s],".",sep = ""))
cat(" Quanto mais verde a cor da cidade, maior é o seu índice de isolamento, e quanto mais vermelha, menor é o seu Indice de Isolamento Social. Cidades para as quais o índice não está disponível são pintadas de branco. O valor do índice para cada cidade em cada dia está disponível em uma planilha disponibilizada em conjunto com esse relatório. Disponibilizamos também um vídeo em que essa sequência de gráficos é apresentada em uma animação.\n")
  
  dir.create(path = paste("./Workspace/",s,"/Figuras",sep = ""))
  for(d in as.character(dias_quar)){
    #Salvar gráfico
    if(!file.exists(paste("./Workspace/",s,"/Figuras/",s,"_",gsub(pattern = "-",replacement = "_",d),".pdf",sep = ""))){
      pdf(file = paste("./Workspace/",s,"/Figuras/",s,"_",gsub(pattern = "-",replacement = "_",d),".pdf",sep = ""),width = 10,height = 7)
      print(mapas[[d]]$p)
      dev.off()
    }
    cat("\n")
    cat("\\begin{figure}[H]
    \\centering\n")
    cat(paste("\\includegraphics[width = \\linewidth]{","./Figuras/",s,"_",gsub(pattern = "-",replacement = "_",d),".pdf",sep = ""),
                   "}\n",sep = "")
    cat("\\end{figure}")
    cat("\n")
  }

  #Apendice
  cat("\\input{apendice.tex}")
  cat("\\end{document}")
  sink()
  cat("Ok!\n")
}

#Para cada cidade
for(s in estados){
  cat("Estado: ")
  cat(s)
  cat("\n")
  
  #Lendo os dados
  cat("Lendo os dados...")
  dados <- readRDS(file = paste("./Dados/",s,".rds",sep = ""))
  cat(" OK!\n")
  problemas <- vector()
  
  cat("Construindo relatórios para as cidades...")
  for(c in unique(dados$reg_name)){
    tmp <- dados %>% filter(day %in% dias_quar & reg_name == c)
    if(nrow(na.omit(tmp)) != nrow(tmp))
      problemas <- c(problemas,c)
    else{
      cat(c)
      cat("\n")
      tmp$day <- dmy(format.Date(tmp$day,"%d/%m/%Y"))
      p <- ggplot(tmp,aes(x = day,y = indice)) + themes + titles + geom_line(colour = "salmon") + geom_point(colour = "salmon") +
        xlab("Dia") + ylab("Índice de Isolamento Social") +
        scale_x_date(date_breaks = "1 week") +
        ggtitle(paste("Índice de Isolamento Social\n",c,"-",s,"\n","De ",format.Date(ini_quar,"%d/%m/%Y")," a ",format.Date(end_quar,"%d/%m/%Y"),"\nPara mais informações acesse www.ime.usp.br/~pedrosp/covid19/",
                      sep = "")) +
        theme(plot.title = element_text(face = "bold"),
              axis.title = element_text(face = "bold",size = 20))
      
      #Relatório
      sink(paste("./Workspace/",s,"/Relatorios/",s,"_",acento(c),"_Relatório sobre isolamento durante a pandemia.tex",sep = ""),split = F)
      #Cabeçalho
      cat("\\documentclass[12pt,a4paper]{article}
  \\usepackage{marginnote}
  \\usepackage{wallpaper}
  \\usepackage{lastpage}
  \\usepackage[left=1.3cm,right=2.0cm,top=1.8cm,bottom=5.0cm,marginparwidth=3.4cm]{geometry}
  \\usepackage{amsmath}
  \\usepackage{amssymb}
  \\usepackage{float}
  \\usepackage{xcolor}
  \\usepackage{fancyhdr}
  \\usepackage[brazil]{babel}
  \\usepackage[latin1]{inputenc}
  \\usepackage[T1]{fontenc}
  \\usepackage{graphicx}
  \\usepackage{pstricks}
  \\usepackage{subfigure}
  \\usepackage{caption}
  \\captionsetup{justification=centering,labelfont=bf}
  \\usepackage{textcomp}
  \\setlength{\\headheight}{80pt}
  \\pagestyle{fancy}\\fancyhf{}
  \\renewcommand{\\headrulewidth}{0pt}
  \\setlength{\\parindent}{1cm}
  \\newcommand{\\tab}{\\hspace*{2em}}
  \\newcommand\\BackgroundStructure{
  \\setlength{\\unitlength}{1mm}
  \\setlength\\fboxsep{0mm}
  \\setlength\\fboxrule{0.5mm}
  \\put(10, 20pr){\fcolorbox{black}{gray!5}{\\framebox(155,247){}}}
  \\put(165, 20){\\fcolorbox{black}{gray!10}{\\framebox(37,247){}}}
  \\put(10, 262){\\fcolorbox{black}{white!10}{\framebox(192, 25){}}}
  \\put(175, 263){\\includegraphics[height=23mm,keepaspectratio]{}} 
  }
  \\fancyhead[L]{\\begin{tabular}{l r | l r}
  \\multicolumn{4}{c}{\\textbf{Relat\\'orio T\\'ecnico: \\'Indice de Isolamento Social - Pandemia da COVID-19}} \\\\
  \\multicolumn{4}{l}{P. Peixoto$^{1}$; D. Marcondes$^{1}$; C. Peixoto$^{1}$; S. Oliva$^{1}$; L. Queiroz$^{2}$, R. Gouveia$^{2}$, A. Delgado$^{2}$} \\\\")
      cat(paste("\\multicolumn{4}{l}{1. Instituto de Matemática e Estatística - USP; 2. In Loco} \\\\ "))
      cat(paste("\\multicolumn{4}{l}{\\textbf{Cidade:}",c," - ",toupper(dic_estados[s]),"} \\\\ \n"))
      cat("\\end{tabular}}\n
  \\begin{document}\n")
      
    #Introdução
    cat("\\section{Índice de Isolamento Social durante a pandemia da COVID-19}\n")
    cat(paste("Neste relatório técnico apresentamos um Índice de Isolamento Social calculado diariamente durante a pandemia da COVID-19 para Cidade de",c,"-",s,"a partir de dados agregados de geolocalização de celulares."))
    cat(" Os dados foram fornecidos de forma anonimizada pela empresa In Loco (www.inloco.com.br), que desenvolve   \\textit{software development kits} (SDK) para aplicativos de celular. O conjunto de dados contém, para cada dia da pandemia, o número de usuários dos aplicativos com o sistema da empresa na cidade que visitaram uma localidade longe de sua residência. Esses dados são uma medida de isolamento, pois um alto número de usuários se movimentando para longe de suas residências é um indicativo de alta mobilidade de pessoas na cidade. Da mesma forma, um baixo número de usuários se movimentando para longe de suas residências é um indicativo de baixa mobilidade de pessoas na cidade, o que é uma evidência de eficácia de medidas de isolamento social.\n\n O índice desenvolvido nesse relatório é um número entre zero e um. Um índice igual a zero indica que o nível de mobilidade dos usuários está dentro do padrão para um dia comum, isto é, antes da pandemia da COVID-19. Já quanto mais próximo de um estiver o índice, mais abaixo do padrão está a mobilidade na cidade, o que é uma evidência de que há mais pessoas em isolamento. Assim, uma alta ao longo do tempo nesse índice representa um aumento no isolamento social dentro da cidade, enquanto que uma queda nesse índice representa uma diminuição no isolamento social na cidade. Mais informações sobre como esse índice é calculado são apresentadas no Apêndice.
\n\n")
    
    cat("\nO objetivo desse índice é subsidiar a tomada de decisões a nível estadual e municipal sobre políticas de isolamento durante a pandemia da COVID-19.\n")
    cat("\n\\textbf{IMPORTANTE:} Todas as decisões devem ser baseadas em informações de várias fontes, sendo esta apenas mais uma que busca auxiliar administradores públicos e privados nesse momento de crise. Ressaltamos que o valor do índice não representa a proporção de pessoas em isolamento social, mas trata-se de uma medida da intensidade do isolamento, podendo ser utilizado para determinar se esse isolamento aumentou ou diminuiu ao longo do tempo. Além disso, esse índice representa o isolamento de apenas uma amostra da população, e não de todos os seus habitantes.\n")
      cat("\n")
      cat(paste("\\section{Índice de Isolamento Social entre os dias",format.Date(ini_quar,"%d/%m/%Y"),"e",format.Date(end_quar,"%d/%m/%Y"),"}"))
      cat("\n")
      
      cat(paste("Abaixo apresentamos o índice calculado para os dias ",format.Date(ini_quar,"%d/%m/%Y")," a ",format.Date(end_quar,"%d/%m/%Y")," para a Cidade de ",c," - ",s,".",sep = ""))
      cat(" Quanto mais alto o valor do índice, maior é o nível de isolamento na cidade no dia.")
      
      cat("\n")
      cat("\n")
      pdf(file = paste("./Workspace/",s,"/Figuras/",s,"_",gsub(pattern = " ",replacement = "",acento(c)),".pdf",sep = ""),width = 10,height = 7)
      print(p)
      dev.off()
      cat("\n")
      cat("\\begin{figure}[H]
    \\centering\n")
      cat(paste("\\includegraphics[width = \\linewidth]{","./Figuras/",s,"_",gsub(pattern = " ",replacement = "",acento(c)),".pdf",sep = ""),
          "}\n",sep = "")
      cat("\\end{figure}")
      cat("\n")
      
      #Tabela
      tmp <- tmp %>% select(day,indice)
      names(tmp) <- c("Dia","Índice de Isolamento Social")
      tmp$Dia <- format.Date(tmp$Dia,"%d/%m/%Y")
      cat("\n")
      print(xtable(x = tmp,align = c("c","c","c")),table.placement = "H",include.rownames = F,caption.placement = "top")
      cat("\n")
      
      #Apendice
      cat("\\input{apendice.tex}")
      cat("\\end{document}")
      sink()
      cat("Ok!\n")
    }
  }
  write.csv(x = cbind(problemas),file = paste("./Workspace/",s,"/",s,"_cidades_sem_relatorio.csv",sep = ""))
}

#latexmk -interaction=nonstopmode -pdf


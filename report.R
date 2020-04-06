#Build plots for report



titles <- theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12,color = "black"), 
                axis.title = element_text(size = 14), legend.text = element_text(size = 14),
                legend.title = element_text(size = 14), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
themes <- list(theme_linedraw())

######Descriptive Analysis####
#Number of users
data <- read_ods("./tabulatedData/user_per_month.ods")
names(data) <- c("day","São Paulo - 2019","São Paulo - 2020","Rio de Janeiro - 2019",
                 "Rio de Janeiro - 2020")
data <- data[-c(1:2,33),]
data <- data %>% gather("local_ano","Users",-day)
data$City <- unlist(lapply(strsplit(data$local_ano,split = "-"),function(x) x[1]))
data$Year <- unlist(lapply(strsplit(data$local_ano,split = "-"),function(x) x[2]))
data$Weekend <- ifelse(wday(ymd(paste(data$Year,"-03-",data$day,sep = "")),label = T) %in% c("sáb","dom"),"Weekend","Weekday")
for(i in c(2,4:6))
  data[,i] <- factor(data[,i])
data$day <- as.numeric(data$day)
data$Users <- as.numeric(data$Users)
data$City <- mapvalues(data$City,c("São Paulo ","Rio de Janeiro "),c("SP","RJ"))
data$half <- factor(ifelse(data$day >= 15,"Second","First"))
data$key <- factor(paste(data$City,data$Year,data$halfs,data$Weekend))
tapply(data$Users,data$key,summary)
summary(data)
scales_y <- list(
  SP = scale_y_continuous(breaks = seq(2e6,8e6,1e6),
                                   label = c("2,000,000","3,000,000","4,000,000","5,000,000",
                                             "6,000,000","7,000,000","8,000,000")),
  RJ = scale_y_continuous(breaks = seq(300000,1600000,200000),
                                        label = c("300,000","500,000","700,000","900,000",
                                                  "1,100,000","1,300,000","1,500,000"))
)

p <- ggplot(data,aes(x = day,y = Users,pch = Weekend,linetype = Year,group = Year)) + themes + titles +
  ylab("Total Number of Recordings") + xlab("Day of March") +
  geom_point(size = 3) + geom_line() + scale_shape_manual("",values = c(19,0)) + 
  facet_grid_sc(rows = vars(City),scales = list(y = scales_y)) + 
  scale_x_continuous(breaks = seq(1,30,2))
pdf(file = "total_use.pdf",width = 10,height = (2/3)*10)
p
dev.off()

data$key2 <- factor(paste(data$half,data$Weekend))
p <- ggplot(data,aes(x = key2,y = Users,fill = Year)) + themes + titles + geom_boxplot() +
  facet_grid_sc(rows = vars(City),scales = list(y = scales_y)) + 
  xlab("Half of March and Day of the Week") + ylab("Total Number of Recordings") +
  scale_fill_manual(values = c("white","gray")) +
  geom_vline(xintercept = 2.5,linetype = "dashed")
pdf(file = "total_use_boxplot.pdf",width = 10,height = (2/3)*10)
p
dev.off()

#Plot relation lambda x transition probabilities
dados <- data.frame(x = c(0,1))
f <- function(x){
  (x < 0.25)*3 + (x >= 0.25)*(3/0.75)*(1 - x)
}

p <- ggplot(dados,aes(x = x)) + themes + titles + stat_function(fun = f) + 
  scale_x_continuous(breaks = c(0,0.25,1),labels = c("0",expression(hat(p)[ii]^{M}),"1")) +
  xlab(expression(hat(p)[ii]^{n})) + ylab(expression(lambda~"(i,t)")) +
  scale_y_continuous(breaks = c(0,3),labels = c("0",expression(lambda)),limits = c(0,3.5)) +
  geom_segment(aes(x = 0.25,xend = 0.25,y = 0,yend = 3),linetype = "dashed")
pdf(file = "relation_lambda.pdf",width = 10,height = (2/3)*10)
p
dev.off()

#######Population Dynamics#####

setwd("~/mobilidade/mdyn/output/")

#Params
states <- c("rio","sao_paulo")
f_mat_new <- list(rio = "move_mat_RIO DE JANEIRO_RJ-Municipios_model.csv",sao_paulo = "move_mat_SÃO PAULO_SP-Municipios_model.csv")
f_mat <- list(rio = "move_mat_RIO DE JANEIRO_RJ-Municipios.csv",sao_paulo = "move_mat_SÃO PAULO_SP-Municipios.csv")
shape_rj <- readOGR(dsn = "~/mobilidade/mdyn/maps/rj_municipios/33MUE250GC_SIR.shp",stringsAsFactors = F)
shape_sp <- readOGR(dsn = "~/mobilidade/mdyn/maps/sp_municipios/35MUE250GC_SIR_mdyn.shp",stringsAsFactors = F)
n_mat <- list(sao_paulo = c(shape_sp$NM_MUNICIP,"MINAS GERAIS","RIO DE JANEIRO","PARANÁ","MATO GROSSO DO SUL"),rio = c(shape_rj$NM_MUNICIP,"MINAS GERAIS","ESPIRITO SANTO","SÃO PAULO"))
pos_mat <- list(sao_paulo = 269,rio = 18)
pop_rj <- read.csv("~/mobilidade/mdyn/maps/population/population_rj.csv",sep = ";")
pop_rj$municipio <- toupper(pop_rj$municipio)
pop_sp <- read.csv("~/mobilidade/mdyn/maps/population/population_sp.csv",sep = ";")
pop_sp$municipio <- toupper(pop_sp$municipio)
pop <- list(rio = pop_rj,sao_paulo = pop_sp )

#Rank study by city
d <- list("2018" = 1:31,"2020" = 1:30)
tab <- list(sao_paulo = data.frame(),rio = data.frame())

for(city in states)
  for(year in c("2018","2020"))
    for(day in d[[year]]){
      cat("\n")
      cat(paste(city,"-",year,"-03-",day,sep = ""))
      if(as.numeric(day) < 10)
        day_mod <- paste("0",day,sep = "")
      r_year <- ifelse(year == "2018","2019",year)
      f <- paste("/storage/inloco/data/visit_journey_",city,"_",year,"/dt=",r_year,"-03-",day_mod,"/",f_mat[[city]],sep = "")
      mat <- fread(file = f,data.table = F)
      colnames(mat) <- n_mat[[city]]
      tmp <- data.frame(rbind(rank(mat[,pos_mat[[city]]],ties.method = "average")))
      tmp$day <- day
      tmp$year <- r_year
      tmp$city <- city
      tmp$month <- "March"
      colnames(tmp)[1:length(n_mat[[city]])] <- n_mat[[city]]
      if(nrow(tab[[city]]) > 0)
        tab[[city]] <- rbind.data.frame(tab[[city]],tmp)
      else
        tab[[city]] <- tmp
            
      #Matriz por model
      for(col in 1:ncol(mat)){
        mat[,col] <- mat[,col]/pop[[city]]$populacao_estimada[pop[[city]]$municipio == colnames(mat)[col]]
        mat[col,col] <- 1 - sum(mat[-col,col])
      }
      if(sum(is.na(mat)) > 0)
        cat("Problem at new matrix!!")
      colnames(mat) <- NULL
      rownames(mat) <- NULL
      f2 <- paste("/storage/inloco/data/visit_journey_",city,"_",year,"/dt=",r_year,"-03-",day_mod,"/",f_mat_new[[city]],sep = "")
      write.csv(mat,file = f2,row.names = F,col.names = F)
    }
write.csv(x = tab[["sao_paulo"]],file = "rank_sao_paulo_municipios_March_2019_2020.csv")
write.csv(x = tab[["rio"]],file = "rank_rio_municipios_March_2019_2020.csv")

#Statistics
tab <- list()

tab[["sao_paulo"]] <- read.csv(file = "rank_sao_paulo_municipios_March_2019_2020.csv")
tab[["sao_paulo"]]$X <- NULL
w <- vector()
for(i in 1:(ncol(tab[["sao_paulo"]])-4))
  w[i] <- mean(tab[["sao_paulo"]][tab[["sao_paulo"]]$year == "2020",i])
names(w) <- names(tab[["sao_paulo"]])[c(1:(ncol(tab[["sao_paulo"]])-4))]
w <- w[order(w,decreasing = T)]
tab[["sao_paulo"]] <- tab[["sao_paulo"]][,colnames(tab[["sao_paulo"]]) %in% c(names(w)[2:16],"day","year","city","month")]
tab[["sao_paulo"]] <- tab[["sao_paulo"]] %>% gather("City","Rank",-day,-year,-city,-month)
View(tab[["sao_paulo"]])

tab[["rio"]] <- read.csv(file = "rank_rio_municipios_March_2019_2020.csv")
tab[["rio"]]$X <- NULL
w <- vector()
for(i in 1:(ncol(tab[["rio"]])-4))
  w[i] <- mean(tab[["rio"]][tab[["rio"]]$year == "2020",i])
names(w) <- names(tab[["rio"]])[c(1:(ncol(tab[["rio"]])-4))]
w <- w[order(w,decreasing = T)]
tab[["rio"]] <- tab[["rio"]][,colnames(tab[["rio"]]) %in% c(names(w)[2:16],"day","year","city","month")]
tab[["rio"]] <- tab[["rio"]] %>% gather("City","Rank",-day,-year,-city,-month)
View(tab[["rio"]])

library(autoAnalise)
a <- auto_resumo(x = factor(paste(tab[["rio"]]$City,"-",tab[["rio"]]$year)),y = tab[["rio"]]$Rank,excel = T,
                 arquivo = "rank_rj.xlsx")
a <- auto_resumo(x = factor(paste(tab[["sao_paulo"]]$City,"-",tab[["sao_paulo"]]$year)),y = tab[["sao_paulo"]]$Rank,excel = T,
                 arquivo = "rank_sp.xlsx")




#Build plots for report

library(ggplot2)
library(readODS)
library(tidyverse)
library(lubridate)
library(facetscales)
library(plyr)

titles <- theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12,
                                                                               color = "black"), axis.title = element_text(size = 14), legend.text = element_text(size = 14),
                legend.title = element_text(size = 14), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
themes <- list(theme_linedraw())

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
  geom_point(size = 3) + geom_line() + scale_shape_manual(values = c(19,0)) + 
  facet_grid_sc(rows = vars(City),scales = list(y = scales_y)) + 
  scale_x_continuous(breaks = seq(1,30,2))
pdf(file = "total_use.pdf",width = 10,height = (2/3)*10)
p
dev.off()

#Plot lambda
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

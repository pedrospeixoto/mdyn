# author: Rafael H. M. Pereira

library(ggplot2)
library(dplyr)
library(Hmisc)
library(ggtext)
library(lubridate)
library(data.table)
options(datatable.optimize = Inf)
options(scipen = 999999) # do not show numbers in scientific notation

`%nin%` <- Negate(`%in%`)




############# read data  -----------------------

# social isolation data
iso_raw <- fread('./database/iso_serasa/Datalake-EstadoSP-extract2020-08-03.csv')
head(iso_raw)
# table(iso_raw$date)


# income data from census tracts
tract <- fread('./database/census_tract/census_tracts2010_sp.csv')
head(tract)



############# filters and recode MOBILITY data -----------------------

iso_raw[, code_muni := substring(sector_code_ibge, 1, 7)]

# trips under 50 km
summary(iso_raw$mean_distance)
summary(iso_raw$median)
iso <- subset(iso_raw, mean_distance < 50000)
density(iso$mean_distance) %>% plot()

# select period of March and April
iso <- iso[ date >= as.Date('2020-03-01') & date <= as.Date('2020-04-30'), ]
summary(iso$date)

# add week number
setDT(iso)[, week := lubridate::week(date)]
setDT(iso)[, weekday := weekdays(date)]

# add isolation
#> iso1km = quantidade de oessias que se deslocara até 1 Km
setDT(iso)[, iso1km := total_id_1km / total_id ]
setDT(iso)[, iso5km := total_id_5km / total_id ]

summary(iso$iso1km)
summary(iso$iso5km)




############# filter and recode TRACT data -----------------------

# calculate census tract groups
tract[, income_per_capita := income_total/pop_total]

tract[, decile := cut(x = income_per_capita, 
                     breaks = Hmisc::wtd.quantile(x = income_per_capita, weights=pop_total, probs=0:10/10, 
                                                  type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                                                  normwt=FALSE, na.rm=T), labels = FALSE, include.lowest = TRUE) ]

tract[, quintile := cut(x = income_per_capita, 
                      breaks = Hmisc::wtd.quantile(x = income_per_capita, weights=pop_total, probs=0:5/5, 
                                                   type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), 
                                                   normwt=FALSE, na.rm=T), labels = FALSE, include.lowest = TRUE) ]


# proportion of black people in census tract
tract[, share_black := pop_preta /pop_total]
tract[, share_mixed := pop_parda /pop_total]
tract[, share_white := pop_branca/pop_total]
summary(tract$share_black)

tract[, black_proportion := (pop_preta+pop_parda) /pop_total]
summary(tract$black_proportion)
tract[, black_proportion_d := cut(x = black_proportion, breaks = c(0, .4, .6, 1), labels = c('0-40%', '40-60%', '60-100%'))]
tract[, .(pop=sum(pop_total, na.rm=T)), by=black_proportion_d]
tract[, .(pop=sum(pop_preta+pop_parda, na.rm=T)/sum(tract$pop_preta, tract$pop_parda, na.rm=T)), by=black_proportion_d]


# replace NA with 0
tract$share_black[is.na(tract$share_black)] <- 0
tract$share_mixed[is.na(tract$share_mixed)] <- 0
tract$share_white[is.na(tract$share_white)] <- 0
tract$black_proportion[is.na(tract$black_proportion)] <- 0



############# Merge census tract and mobility data -----------------------

iso2 <- iso[tract, on=c('sector_code_ibge'='code_tract'),
          c('code_muni', 'pop_total', 'quintile','decile', 'black_proportion', 'black_proportion_d',
            'share_white', 'share_black', 'share_mixed' ) := 
            list(i.code_muni, i.pop_total, i.quintile, i.decile, i.black_proportion, i.black_proportion_d,
                 i.share_white, i.share_black, i.share_mixed) ]



# drop census tracts with very few pings
iso2[, ping_prop := total_id / pop_total]
summary(iso2$ping_prop) *100
iso2 <- subset(iso2, ping_prop >= .01)

iso2$sector_code_ibge %>% unique() %>% length() /
iso$sector_code_ibge %>% unique() %>% length()




############# Plot black  -----------------------

# colors
black_col <- poor_col <- '#ffa600'
white_col <- rich_col <- '#003f5c'

# summarize by date and group
df_black_proportion_d <- iso2[, .(dist= median(x=median, w=pop_total, na.rm=T),
                                  iso1= median(x=iso1km, w=pop_total),
                                  iso5= median(x=iso5km, w=pop_total)), by= .(black_proportion_d, date)]


ggplot() +
  geom_line(data= subset(df_black_proportion_d, black_proportion_d=='40-60%'), aes(x=date, y=dist), color= 'gray', alpha=.2) +
  geom_line(data= subset(df_black_proportion_d, black_proportion_d=='0-40%'), aes(x=date, y=dist), color=white_col, alpha=1) +
  geom_line(data= subset(df_black_proportion_d, black_proportion_d=='60-100%'), aes(x=date, y=dist), color= black_col, alpha=1) +
  scale_x_date(date_breaks='2 weeks', date_labels = "%b/%d") +
  scale_y_log10() +
  labs( x='', y='Median distance (log)',
        title = "Daily mobility of predominantly **<span style='color:#003f5c'>White</span>** and 
   **<span style='color:#ffa600'>Black</span>** population",
         subtitle = 'At least 60% black vs 60% white tracts'
  ) +
  theme_minimal()+
  theme(plot.title = element_markdown(size = 11))

ggsave(filename  = './figures/mobility_black_median-median.png', dpi=200, width = 16, height = 8, units='cm')





############# Plot quintile -----------------------


# summarize by date and group
df_quintile <- iso2[, .(dist=median(x=median, na.rm=T),
                       stdv = sd(median, na.rm=T)), by= .(quintile, date)]

ggplot() +
  geom_line(data= subset(df_quintile, quintile==4), aes(x=date, y=dist), color= rich_col, alpha=.2) +
  geom_line(data= subset(df_quintile, quintile==3), aes(x=date, y=dist), color= 'gray', alpha=.2) +
  geom_line(data= subset(df_quintile, quintile==2), aes(x=date, y=dist), color= poor_col, alpha=.2) +
  geom_line(data= subset(df_quintile, quintile==5), aes(x=date, y=dist), color=rich_col, alpha=1) +
  geom_line(data= subset(df_quintile, quintile==1), aes(x=date, y=dist), color= poor_col, alpha=1) +
  # geom_smooth(data= subset(df_quintile, quintile==1), aes(x=date, y=dist), color=NA, fill=poor_col, span = 0.1, alpha=.2) +
  # geom_smooth(data= subset(df_quintile, quintile==5), aes(x=date, y=dist), color=NA, fill=rich_col, span = 0.1, alpha=.2) +
  scale_x_date(date_breaks='2 weeks', date_labels = "%b/%d") +
  scale_y_log10() +
  labs( x='', y='Mean distance (log)',
        title = "Daily mobility of **<span style='color:#ffa600'>Low</span>** and
    **<span style='color:#003f5c'>High</span>** income population",
        subtitle = '20% poorest vs 20% wealthiest'
        #subtitle = 'median of mean distance'
  ) +
  theme_minimal()+
  theme(plot.title = element_markdown(size = 11))

ggsave(filename  = './figures/mobility_quintile_median-medianc.png', dpi=200, width = 16, height = 8, units='cm')





# ########## Selected cities
# 
# # São Paulo - 3550308
# # Campinas - 3509502
# # Ribeirão Preto - 3543402
# # São José dos Campos - 3549904
# geobr::lookup_muni('São José dos Campos')
# 
# cities_list <- c(3550308, 3509502, 3543402, 3549904)
# 
# # summarize by date and group
# df_decile <- iso2[code_muni %in% cities_list, .(dist=median(x=mean_distance, w=pop_total, na.rm=T),
#                      stdv = sd(mean_distance, na.rm=T)), by= .(code_muni, decile, date)]
# 
# ggplot() + 
#   geom_line(data= subset(df_decile, decile==5), aes(x=date, y=dist), color= poor_col, alpha=.2) +
#   geom_line(data= subset(df_decile, decile==4), aes(x=date, y=dist), color= poor_col, alpha=.2) +
#   geom_line(data= subset(df_decile, decile==3), aes(x=date, y=dist), color= poor_col, alpha=.2) +
#   geom_line(data= subset(df_decile, decile==2), aes(x=date, y=dist), color= poor_col, alpha=.2) +
#   geom_line(data= subset(df_decile, decile==6), aes(x=date, y=dist), color= rich_col, alpha=.2) +
#   geom_line(data= subset(df_decile, decile==7), aes(x=date, y=dist), color= rich_col, alpha=.2) +
#   geom_line(data= subset(df_decile, decile==8), aes(x=date, y=dist), color= rich_col, alpha=.2) +
#   geom_line(data= subset(df_decile, decile==9), aes(x=date, y=dist), color= rich_col, alpha=.2) +
#   geom_line(data= subset(df_decile, decile==10), aes(x=date, y=dist), color=rich_col, alpha=1) +
#   geom_line(data= subset(df_decile, decile==1), aes(x=date, y=dist), color= poor_col, alpha=1) +
#   scale_x_date(date_breaks='2 weeks', date_labels = "%b/%d") +
#   scale_y_log10() +
#   labs( x='', y='Average distance (log)',
#         title = "Daily mobility of **<span style='color:#ffa600'>Low</span>** and 
#     **<span style='color:#003f5c'>High</span>** income population"
#   ) +
#   theme_minimal()+
#   theme(plot.title = element_markdown(size = 11)) +
#     facet_wrap(.~code_muni, nrow = 2)
# 



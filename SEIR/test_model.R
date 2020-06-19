#Test if model fit
test_model <- function(D,I,teste_D,teste_I,drs,init_validate,end_validate){
  
  #Error in cities
  Dcity <- (D - teste_D$city[,-1])/teste_D$city[,-1]
  Dcity <- (Dcity[teste_D$city[,-1] > 100])
  Icity <- (I - teste_I$city[,-1])/teste_I$city[,-1]
  Icity <- (Icity[teste_I$city[,-1] > 1000])

  #Deaths in DRSs
  colnames(D) <- par$names
  D$date <- seq.Date(ymd(init_validate),ymd(end_validate),1) 
  D <- D %>% gather("Municipio","D",-date)
  D <- merge(D,drs)
  D$key <- paste(D$date,D$DRS)
  D <- data.table(D)
  D <- D[,D_pred := sum(D),by = key]
  D <- D %>% select(D_pred,key) %>% unique() %>% data.frame()
  D <- merge(D,teste_D$DRS)
  D$dif <- (D$D_pred - D$D_drs)/D$D_drs
  D <- D %>% filter(DRS != "0")
  dif_D <- max(c(max(abs(D$dif)[D$D_drs > 100]),quantile(abs(Dcity),0.9)))
  D <- D %>% filter(D_drs > 100)
  D <- c(D$dif,Dcity[Dcity < quantile(Dcity,0.9)])

  #Cases in DRSs
  colnames(I) <- par$names
  I$date <- seq.Date(ymd(init_validate),ymd(end_validate),1) 
  I <- I %>% gather("Municipio","I",-date)
  I <- merge(I,drs)
  I$key <- paste(I$date,I$DRS)
  I <- data.table(I)
  I <- I[,I_pred := sum(I),by = key]
  I <- I %>% select(I_pred,key) %>% unique() %>% data.frame()
  I <- merge(I,teste_I$DRS)
  I$dif <- (I$I_pred - I$I_drs)/I$I_drs
  I <- I %>% filter(DRS != "0")
  dif_I <- max(c(quantile(abs(I$dif)[I$I_drs > 1000],0.9),quantile(abs(Icity),0.9)))
  I <- I %>% filter(I_drs > 100)
  I <- c(I$dif,Icity[Icity < quantile(Icity,0.9)])
  
  return(list("dif_D" = dif_D,"dif_I" = dif_I,"error_D" = D,"error_I" = I))
}
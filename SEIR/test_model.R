#Test if model fit
test_model <- function(D,I,teste_D,teste_I,drs){

  #Deaths
  colnames(D) <- par$names
  D$date <- seq.Date(ymd(init_validate),ymd(end_validate),1) 
  D <- D %>% gather("Municipio","D",-date)
  D <- merge(D,drs)
  D$key <- paste(D$date,D$DRS)
  D <- data.table(D)
  D <- D[,D_pred := sum(D),by = key]
  D <- D %>% select(D_pred,key) %>% unique() %>% data.frame()
  D <- merge(D,teste_D)
  D$dif <- (D$D_pred - D$D_drs)/D$D_drs
  dif_D <- max(abs(D$dif)[D$D_drs > 50])

  #Cases
  colnames(I) <- par$names
  I$date <- seq.Date(ymd(init_validate),ymd(end_validate),1) 
  I <- I %>% gather("Municipio","I",-date)
  I <- merge(I,drs)
  I$key <- paste(I$date,I$DRS)
  I <- data.table(I)
  I <- I[,I_pred := sum(I),by = key]
  I <- I %>% select(I_pred,key) %>% unique() %>% data.frame()
  I <- merge(I,teste_I)
  I$dif <- (I$I_pred - I$I_drs)/I$I_drs
  dif_I <- max(abs(I$dif)[I$I_drs > 1000])
  
  return(list("dif_D" = dif_D,"dif_I" = dif_I,"error_D" = D,"error_I" = I))
}
#Read data about COVID-19 tests

testagem <- function(){
  testagem <- read.csv("./www/testagem.csv",sep = ";",dec = ",")
  testagem$Data <- ymd(testagem$Data)
  testagem$taxa <- testagem$Positivo.Acc/testagem$Total.Acc
  testagem <- testagem %>% filter(Data == max(testagem$Data)) %>% select(DRS,taxa)
  testagem$padrao <- testagem$taxa[testagem$DRS == "Estado de São Paulo"]
  testagem$lift <- testagem$taxa/testagem$padrao
  
  return(testagem[-18,])
}
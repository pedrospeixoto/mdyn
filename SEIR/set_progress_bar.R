#Set progress bar
set_progress_bar <- function(size){
  pb <- progress_bar$new(
    format = ":letter [:bar] :elapsed | eta: :eta",
    total = size,    # 100 
    width = 80)
  progress_letter <- paste(round(100*c(1:size)/size,2),"%")
  progress <- function(n){
    pb$tick(tokens = list(letter = progress_letter[n]))
  } 
  opts <- list(progress = progress)
  
  return(list(pb,progress_letter,opts))
}
#Test if date is valid
is.valid.date <- function(date){
  month_30 <- c(4,6,9,11)
  year <- as.numeric(substr(x = date,start = 1,stop = 4))
  month <- as.numeric(substr(x = date,start = 6,stop = 7))
  day <- as.numeric(substr(x = date,start = 9,stop = 10))
  if(year < 2020 || month == 0 || month > 12 || day < 0 || day > 31)
    return(FALSE)
  else if(month %in% month_30 && day == 31)
    return(FALSE)
  else if(month == 2 && day > 29)
    return(FALSE)
  else if(month == 2 && day == 29 && !is.leap(year))
    return(FALSE)
  else
    return(TRUE)
}

#Test if a year is leap year
is.leap <- function(year){
  return(year%%4 == 0)
}

#Positive lod
mod_log <- function(x){
  return(ifelse(x >= 1,log(x),0))
}


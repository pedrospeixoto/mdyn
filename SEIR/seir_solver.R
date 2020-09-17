#SEIR solver
solve_seir <- function(y,times,derivatives,parms){
  mod <- data.frame("t" = 1,rbind(y))
  if(length(names(y)) > 0)
    names(mod)[-1] <- names(y)
  for(t in times[-1]){
    y <- y + unlist(derivatives(t = t-1,Y = y,parK = parms))
    y[y < 0] <- 0
    mod[t,] <- c(t,y)
  }
  return(mod)
}


## Vectorizing the binaries
##
## Enio Gjerga, 2020

transformBinaries <- function(mt = mt, lpFile = lpFile){
  
  idx1 <- which(lpFile$`enter Problem`=="Binaries")
  idx2 <- which(lpFile$`enter Problem`=="Generals")
  
  constraintSet <- lpFile$`enter Problem`[(idx1+1):(idx2-1)]
  
  f.con <- matrix(data = 0, nrow = 2*length(constraintSet), ncol = nrow(mt))
  f.dir <- c()
  f.rhs <- c()
  
  cnt <- 1
  for(ii in 1:length(constraintSet)){
    
    currConstraint <- constraintSet[ii]
    
    f.con[cnt, which(mt[, 1]==currConstraint)] <- 1
    f.dir <- c(f.dir, ">=")
    f.rhs <- c(f.rhs, 0)
    cnt <- cnt + 1
    f.con[cnt, which(mt[, 1]==currConstraint)] <- 1
    f.dir <- c(f.dir, "<=")
    f.rhs <- c(f.rhs, 1)
    cnt <- cnt + 1
    
  }
  
  ff <- list()
  ff[[length(ff)+1]] <- f.con
  ff[[length(ff)+1]] <- f.dir
  ff[[length(ff)+1]] <- f.rhs
  
  names(ff) <- c("con", "dir", "rhs")
  
  return(ff)
  
}
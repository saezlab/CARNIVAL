#'\code{transformBounds}
#'
#' Vectorizing the bounds
#'
#'Enio Gjerga, 2020

transformBounds <- function(mt = mt, lpFile = lpFile){
  
  idx1 <- which(lpFile$`enter Problem`=="Bounds")
  idx2 <- which(lpFile$`enter Problem`=="Binaries")
  
  constraintSet <- lpFile$`enter Problem`[(idx1+1):(idx2-1)]
  
  f.con <- matrix(data = 0, nrow = 2*length(constraintSet), ncol = nrow(mt))
  f.dir <- c()
  f.rhs <- c()
  
  cnt <- 1
  for(ii in 1:length(constraintSet)){
    
    currConstraint <- strsplit(x = constraintSet[ii], split = " ", 
                               fixed = TRUE)[[1]]
    
    f.con[cnt, which(mt[, 1]==currConstraint[3])] <- 1
    f.dir <- c(f.dir, ">=")
    f.rhs <- c(f.rhs, currConstraint[1])
    cnt <- cnt + 1
    f.con[cnt, which(mt[, 1]==currConstraint[3])] <- 1
    f.dir <- c(f.dir, "<=")
    f.rhs <- c(f.rhs, currConstraint[5])
    cnt <- cnt + 1
    
  }
  
  ff <- list()
  ff[[length(ff)+1]] <- f.con
  ff[[length(ff)+1]] <- f.dir
  ff[[length(ff)+1]] <- f.rhs
  
  names(ff) <- c("con", "dir", "rhs")
  
  return(ff)
  
}
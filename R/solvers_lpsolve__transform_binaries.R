## Vectorizing the binaries
##
## Enio Gjerga, 2020

transformBinaries <- function(mt, lpFile){
  
  idx1 <- match("Binaries", lpFile)
  idx2 <- match("Generals", lpFile)
  
  constraintSet <- lpFile[seq(from = idx1 + 1, to = idx2 - 1, by = 1)]
  
  f.con <- matrix(data = 0, nrow = 2 * length(constraintSet), ncol = nrow(mt))

  f.dir <- rep("", length(constraintSet) * 2)
  f.rhs <- rep("", length(constraintSet) * 2)
  
  cnt <- 1
  for(ii in seq_len(length(constraintSet))){
    
    currConstraint <- constraintSet[ii]
    
    f.con[cnt, which(mt[, 1] == currConstraint)] <- 1
    f.dir[cnt] <- ">="
    f.rhs[cnt] <- 0
    cnt <- cnt + 1
    f.con[cnt, which(mt[, 1] == currConstraint)] <- 1
    f.dir[cnt] <- "<="
    f.rhs[cnt] <- 1
    cnt <- cnt + 1
    
  }
  
  ff <- vector(mode = "list", length = 3)
  names(ff) <- c("con", "dir", "rhs")
  ff$con <- f.con
  ff$dir <- f.dir
  ff$rhs <- f.rhs
  
  return(ff)
  
}
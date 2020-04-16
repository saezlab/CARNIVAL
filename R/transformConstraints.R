## Vectorizing the constraints
##
## Enio Gjerga, 2020

transformConstraints <- function(mt = mt, lpFile = lpFile){
  
  idx1 <- which(lpFile$`enter Problem`=="Subject To")
  idx2 <- which(lpFile$`enter Problem`=="Bounds")
  
  constraintSet <- lpFile$`enter Problem`[seq(from = idx1+1, 
                                              to = idx2-1, by = 1)]
  constraintSet <- lapply(strsplit(x = constraintSet, 
                                   split = "\t", fixed = TRUE), "[[", 2)
  
  directions <- c("=", "<", ">", "<=", ">=")
  
  f.con <- matrix(data = 0, nrow = length(constraintSet), ncol = nrow(mt))
  ## f.dir <- c()
  ## f.rhs <- c()
  f.dir <- rep("", length(constraintSet))
  f.rhs <- rep("", length(constraintSet))
  
  for(ii in seq_len(length(constraintSet))){
    
    currConstraint <- constraintSet[[ii]]
    currConstraintSplit <- strsplit(x = currConstraint, split = " ", 
                                    fixed = TRUE)[[1]]
    
    idx1 <- which(currConstraintSplit%in%mt[, 1])
    
    cnt = 1
    while(cnt<=length(idx1)){
      
      currVar <- currConstraintSplit[idx1[cnt]]
      
      if(idx1[cnt]==1){
        
        f.con[ii, which(mt[, 1]==currVar)] <- 1
        cnt <- cnt + 1
        
      } else {
        
        if(cnt==1){
          
          f.con[ii, which(mt[, 1]==currVar)] <- 101
          cnt <- cnt + 1
          
        } else {
          
          if(currConstraintSplit[idx1[cnt]-1]=="+"){
            
            f.con[ii, which(mt[, 1]==currVar)] <- 1
            cnt <- cnt + 1
            
          } else {
            
            f.con[ii, which(mt[, 1]==currVar)] <- -1
            cnt <- cnt + 1
            
          }
          
        }
        
      }
      
    }
    
    idx2 <- which(currConstraintSplit%in%directions)
    f.dir[ii] <- currConstraintSplit[idx2]
    f.rhs[ii] <- currConstraintSplit[idx2+1]
    
  }
  
  
  ff <- list()
  ff[[length(ff)+1]] <- f.con
  ff[[length(ff)+1]] <- f.dir
  ff[[length(ff)+1]] <- f.rhs
  
  names(ff) <- c("con", "dir", "rhs")
  
  return(ff)
  
}
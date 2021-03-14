## Returning LP matrix for one condition
##
## Enio Gjerga, 2020

prepareLPMatrixSingle <- function(variables = variables, 
                                  measObj = measObj){
  
  mt = transformVariables(variables = variables, measObj = measObj)
  
  lpFile = readr::read_csv(file = paste0("testFile_", 1, "_", 1, ".lp"))
  
  lpFile$`enter Problem` <- as.character(lpFile$`enter Problem`)
  
  f.obj <- transformObjectiveFunction(mt = mt, lpFile = lpFile)
  
  ff1 <- transformConstraints(mt = mt, lpFile = lpFile)
  ff2 <- transformBounds(mt = mt, lpFile = lpFile)
  ff3 <- transformBinaries(mt = mt, lpFile = lpFile)
  
  f.con <- rbind(ff1$con, ff2$con, ff3$con)
  f.dir <- c(ff1$dir, ff2$dir, ff3$dir)
  f.rhs <- c(ff1$rhs, ff2$rhs, ff3$rhs)
  
  idx1 <- which(lpFile$`enter Problem`=="Binaries")
  idx2 <- which(lpFile$`enter Problem`=="Generals")
  idx3 <- which(lpFile$`enter Problem`=="End")
  binaryVar <- lpFile$`enter Problem`[seq(from = idx1+1, to = idx2-1, by = 1)]
  bins <- c()
  for(ii in seq_len(length(binaryVar))){
    bins <- c(bins, which(mt[, 1]==binaryVar[ii]))
  }
  integerVar <- lpFile$`enter Problem`[seq(from = idx2+1, to = idx3-1, by = 1)]
  ints <- c()
  for(ii in seq_len(length(integerVar))){
    ints <- c(ints, which(mt[, 1]==integerVar[ii]))
  }
  
  res <- list()
  res[[length(res)+1]] <- mt
  res[[length(res)+1]] <- f.obj
  res[[length(res)+1]] <- f.con
  res[[length(res)+1]] <- f.dir
  res[[length(res)+1]] <- f.rhs
  res[[length(res)+1]] <- bins
  res[[length(res)+1]] <- ints
  
  names(res) <- c("mt", "obj", "con", "dir", "rhs", "bins", "ints")
  
  return(res)
  
}
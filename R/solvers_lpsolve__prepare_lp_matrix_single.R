## Returning LP matrix for one condition
##
## Enio Gjerga, 2020

prepareLPMatrixSingle <- function(variables = variables, 
                                  measurements = measurements, 
                                  carnivalOptions = carnivalOptions){
  
  mt <- transformVariables_v2(variables = variables, measurements = measurements)
  
  lpFile <- readr::read_csv(file = carnivalOptions$filenames$lpFilename, 
                            col_names = F)
  lpFile <- lpFile[[1]]
  
  f.obj <- transformObjectiveFunction(mt = mt, lpFile = lpFile)
  
  ff1 <- transformConstraints(mt = mt, lpFile = lpFile)
  ff2 <- transformBounds(mt = mt, lpFile = lpFile)
  ff3 <- transformBinaries(mt = mt, lpFile = lpFile)
  
  f.con <- rbind(ff1$con, ff2$con, ff3$con)
  f.dir <- c(ff1$dir, ff2$dir, ff3$dir)
  f.rhs <- c(ff1$rhs, ff2$rhs, ff3$rhs)
   
  idx1 <- which(lpFile == "Binaries")
  idx2 <- which(lpFile == "Generals")
  idx3 <- which(lpFile == "End")
  
  binaryVar <- lpFile[seq(from = idx1 + 1, to = idx2 - 1, by = 1)]
  bins <- c()
  for(ii in seq_len(length(binaryVar))){
    bins <- c(bins, which(mt[, 1] == binaryVar[ii]))
  }
  
  integerVar <- lpFile[seq(from = idx2+1, to = idx3-1, by = 1)]
  ints <- c()
  
  for(ii in seq_len(length(integerVar))){
    ints <- c(ints, which(mt[, 1] == integerVar[ii]))
  }
  
  res <-  list("mt"= mt, "obj" = f.obj, 
              "con" = f.con, "dir" = f.dir, 
              "rhs" = f.rhs, "bins" = bins, 
              "ints" = ints)

  return(res)
  
}
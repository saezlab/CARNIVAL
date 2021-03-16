
writeCplexCommandFile <- function(carnivalOptions){

  message("Writing cplex command file")
  
  ## write cplexCommand file
  cplexCommand <- paste0("cplexCommand", ".txt")
  
  write(paste0("read testFile", ".lp"), 
        cplexCommand, append = TRUE)
  write(paste0("set mip tolerances mipgap ", carnivalOptions$mipGap), 
        cplexCommand, append = TRUE)
  write(paste0("set mip pool relgap ", carnivalOptions$poolrelGap), 
        cplexCommand, append = TRUE)
  write(paste0("set mip pool replace ", carnivalOptions$poolReplace), 
        cplexCommand, append = TRUE)
  write(paste0("set mip limits populate ", carnivalOptions$limitPop), 
        cplexCommand, append = TRUE)
  write(paste0("set mip pool capacity ", carnivalOptions$poolCap), 
        cplexCommand, append = TRUE)
  write(paste0("set mip pool intensity ", carnivalOptions$poolIntensity), 
        cplexCommand, append = TRUE)
  write(paste0("set timelimit ", carnivalOptions$timelimit), cplexCommand, append = TRUE)
  write(paste0("set threads ", carnivalOptions$threads), cplexCommand, append = TRUE)
  write("populate", cplexCommand, append = TRUE)
  write(paste0("write results_cplex",".txt sol all"), 
        cplexCommand, append = TRUE)
  write("quit", cplexCommand, append = TRUE)
  
  message("Done: writing cplex command file")
}
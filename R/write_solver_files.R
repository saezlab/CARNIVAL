## Returning result object after solving with CARNIVAL
##
## Enio Gjerga, 2020

writeSolverFiles <- function(oF = oF,
                             allC = allC, 
                             bounds = bounds, 
                             binaries = binaries,
                             generals = generals, 
                             carnivalOptions){
  
  ## write the .lp file

  lpFilename = paste0("testFile", ".lp")
  write("enter Problem", lpFilename)
  write("", lpFilename, append = TRUE)
  write("Minimize", lpFilename, append = TRUE)
  write(oF, lpFilename, append = TRUE)
  write("Subject To", lpFilename, append = TRUE)
  write(allC, lpFilename, append = TRUE)
  write("Bounds", lpFilename, append = TRUE)
  write(bounds, lpFilename, append = TRUE)
  write("Binaries", lpFilename, append = TRUE)
  write(binaries, lpFilename, append = TRUE)
  write("Generals", lpFilename, append = TRUE)
  write(generals, lpFilename, append = TRUE)
  write("End", lpFilenames, append = TRUE)
  
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
  
}
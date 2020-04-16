## Returning result object after solving with CARNIVAL
##
## Enio Gjerga, 2020

writeSolverFiles <- function(condition=condition, repIndex=repIndex, oF=oF,
                             allC=allC, bounds=bounds, binaries=binaries,
                             generals=generals, mipGAP=mipGAP, 
                             poolrelGAP=poolrelGAP, poolReplace=poolReplace,
                             limitPop=limitPop, poolCap=poolCap,
                             threads = threads,
                             poolIntensity=poolIntensity, timelimit=timelimit){
  
  ## write the .lp file
  data = paste0("testFile_", condition,"_",repIndex,".lp")
  write("enter Problem", data)
  write("", data, append = TRUE)
  write("Minimize", data, append = TRUE)
  write(oF, data, append = TRUE)
  write("Subject To", data, append = TRUE)
  write(allC, data, append = TRUE)
  write("Bounds", data, append = TRUE)
  write(bounds, data, append = TRUE)
  write("Binaries", data, append = TRUE)
  write(binaries, data, append = TRUE)
  write("Generals", data, append = TRUE)
  write(generals, data, append = TRUE)
  write("End", data, append = TRUE)
  
  ## write cplexCommand file
  cplexCommand <- paste0("cplexCommand_", condition,"_",repIndex,".txt")
  write(paste0("read testFile_", condition,"_",repIndex,".lp"), 
        cplexCommand, append = TRUE)
  write(paste0("set mip tolerances mipgap ",mipGAP), 
        cplexCommand, append = TRUE)
  write(paste0("set mip pool relgap ",poolrelGAP), 
        cplexCommand, append = TRUE)
  write(paste0("set mip pool replace ", poolReplace), 
        cplexCommand, append = TRUE)
  write(paste0("set mip limits populate ",limitPop), 
        cplexCommand, append = TRUE)
  write(paste0("set mip pool capacity ",poolCap), 
        cplexCommand, append = TRUE)
  write(paste0("set mip pool intensity ",poolIntensity), 
        cplexCommand, append = TRUE)
  write(paste0("set timelimit ",timelimit), cplexCommand, append = TRUE)
  write(paste0("set threads ", threads), cplexCommand, append = TRUE)
  write("populate", cplexCommand, append = TRUE)
  write(paste0("write results_cplex_", condition,"_",repIndex,".txt sol all"), 
        cplexCommand, append = TRUE)
  write("quit", cplexCommand, append = TRUE)
  
}
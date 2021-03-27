## Cleanup auxilliary files
##
## Enio Gjerga, 2020

#TODO update 
cleanupCARNIVAL <- function(carnivalOptions){
  
  message("Cleaning intermediate files")
  
  lpFile <- carnivalOptions$filenames$lpFilename 
  resultFile <- carnivalOptions$filenames$resultFile
  keepLpFiles <- carnivalOptions$keepLPFiles
  
  if(!keepLpFiles & file.exists(lpFile)){
    file.remove(lpFile)
  }
  
  if(file.exists(resultFile)){
    file.remove(resultFile)
  }
  
  if (carnivalOptions$solver == supportedSolvers$cplex) {
    workdir <- carnivalOptions$workdir
    cplexCommandFile <- carnivalOptions$filenames$cplexCommandFile
    
    if(file.exists("cplex.log")){
      file.remove("cplex.log")
    }
    
    if(file.exists(cplexCommandFile)){
      file.remove(cplexCommandFile)
    }
    
    cloneFiles <- list.files(path = workdir, pattern = "$clone")
    file.remove(cloneFiles)
  }
  
  message("Done: cleaning")
  
}
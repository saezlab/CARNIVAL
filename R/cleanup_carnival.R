## Cleanup auxilliary files
##
## Enio Gjerga, 2020

cleanSolverFiles <- function(files=c("lp", "log", "txt"), folder="") {
  #TODO 
}

#TODO update 
cleanupCARNIVAL <- function(keepLPFiles=FALSE){
  if(!keepLPFiles & file.exists(paste0("testFile", ".lp"))){
    file.remove(paste0("testFile", ".lp"))
  }
  
  if(file.exists(paste0("results_cbc", ".txt"))){
    file.remove(paste0("results_cbc", ".txt"))
  }
  
  if(file.exists(paste0("results_cplex", ".txt"))){
    file.remove(paste0("results_cplex", ".txt"))
  }
  
  if(file.exists("cplex.log")){
    file.remove("cplex.log")
  }
  
  if(file.exists(paste0("cplexCommand", ".txt"))){
    file.remove(paste0("cplexCommand", ".txt"))
  }
  
  cloneFiles <- list.files(pattern = "$clone")
  file.remove(cloneFiles)
  
}
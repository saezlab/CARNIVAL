## Cleanup auxilliary files
##
## Enio Gjerga, 2020

cleanupCARNIVAL <- function(condition=condition, repIndex=repIndex){
  
  if(file.exists(paste0("testFile_",condition,"_",repIndex,".lp"))){
    file.remove(paste0("testFile_",condition,"_",repIndex,".lp"))
  }
  
  if(file.exists(paste0("results_cbc_",condition, "_",repIndex,".txt"))){
    file.remove(paste0("results_cbc_",condition,"_",repIndex,".txt"))
  }
  
  if(file.exists(paste0("results_cplex_",condition, "_",repIndex,".txt"))){
    file.remove(paste0("results_cplex_",condition,"_",repIndex,".txt"))
  }
  
  if(file.exists("cplex.log")){
    file.remove("cplex.log")
  }
  
  if(file.exists(paste0("cplexCommand_", condition,"_",repIndex,".txt"))){
    file.remove(paste0("cplexCommand_", condition,"_",repIndex,".txt"))
  }
  
  AllFiles <- list.files()
  CloneFiles <- which(grepl(pattern = "clone",x = AllFiles,fixed = TRUE))
  if (length(CloneFiles)>0) {
    for (counter in 1:length(CloneFiles)) {
      file.remove(AllFiles[CloneFiles[counter]])
    }
  }
  
}
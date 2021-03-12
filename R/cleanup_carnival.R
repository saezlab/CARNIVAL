## Cleanup auxilliary files
##
## Enio Gjerga, 2020

clean_solver_files <- function(files=c("lp", "log", "txt"), folder="") {
  #TODO 
}

cleanupCARNIVAL <- function(condition=condition, repIndex=repIndex, keepLPFiles=FALSE){
  
  if(!keepLPFiles & file.exists(paste0("testFile_", condition,"_", repIndex,".lp"))){
    file.remove(paste0("testFile_", condition,"_", repIndex,".lp"))
  }
  
  if(file.exists(paste0("results_cbc_", condition, "_", repIndex,".txt"))){
    file.remove(paste0("results_cbc_", condition,"_", repIndex,".txt"))
  }
  
  if(file.exists(paste0("results_cplex_", condition, "_", repIndex, ".txt"))){
    file.remove(paste0("results_cplex_", condition,"_", repIndex, ".txt"))
  }
  
  if(file.exists("cplex.log")){
    file.remove("cplex.log")
  }
  
  if(file.exists(paste0("cplexCommand_", condition,"_", repIndex, ".txt"))){
    file.remove(paste0("cplexCommand_", condition,"_", repIndex, ".txt"))
  }
  
  #TODO optimize
  allFiles <- list.files()
  cloneFiles <- which(grepl(pattern = "clone", x = allFiles, fixed = TRUE))
  if (length(cloneFiles) > 0) {
    for (counter in seq_len(length(cloneFiles))) {
      file.remove(allFiles[cloneFiles[counter]])
    }
  }
  
}
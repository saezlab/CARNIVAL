## Cleanup auxilliary files
##
## Enio Gjerga, 2020

clean_solver_files <- function(files=c("lp", "log", "txt"), folder="") {
  #TODO 
}

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
  
  #TODO optimize
  allFiles <- list.files()
  cloneFiles <- which(grepl(pattern = "clone", x = allFiles, fixed = TRUE))
  if (length(cloneFiles) > 0) {
    for (counter in seq_len(length(cloneFiles))) {
      file.remove(allFiles[cloneFiles[counter]])
    }
  }
  
}
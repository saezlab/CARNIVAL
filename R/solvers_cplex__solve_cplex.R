solveWithCplex <- function(carnivalOptions) {
  
  #create cplex command file
  writeCplexCommandFile(carnivalOptions)
  cplexCommandFilename <- carnivalOptions$filenames$cplexCommandFile
  cplexLog <- carnivalOptions$filenames$cplexLog
    
  if (Sys.info()[1] == "Windows") {
    # TODO: implement logging on Win machine. 
    #TODO why copying exe and not directly executing it? 
    file.copy(from = carnivalOptions$solverPath, to = getwd())
    system(paste0("cplex.exe -f", cplexCommandFilename))
    file.remove("cplex.exe")
  } else {
    system(paste0(carnivalOptions$solverPath, " -f ", cplexCommandFilename,
                  " | tee ", cplexLog)) # send output to logfile and stdout
  }
  
  ## Write result files in the results folder
  message("Saving results...") 
  
  solutionFileName <- carnivalOptions$filenames$resultFile
  solution <- read.delim(file = solutionFileName)
  
  return(solution)
}

getSolutionMatrixCplex <- function(solution) {
  
  solution[, 1] <- as.character(solution[, 1])
  idxVarStart <- which(grepl(pattern = "<variables>", x = solution[, 1]))[-1]
  idxVarEnd <- which(grepl(pattern = "</variables>", x = solution[, 1]))[-1]
  
  solMatrix <- matrix(data = , nrow = idxVarEnd[1]-idxVarStart[1]-1, 
                      ncol = length(idxVarStart))
  colnames(solMatrix) <- paste0("Solution-", seq_len(ncol(solMatrix)))
  ss1 <- sapply(strsplit(solution[seq(from = idxVarStart[1]+1, 
                                      to = idxVarEnd[1]-1, by = 1), 1], 
                         split = " "), "[", 5)
  rownames(solMatrix) <- sapply((strsplit(ss1, split = "=")), "[", 2)
  
  for(ii in seq_len(ncol(solMatrix))){
    
    ss1 <- 
      sapply(strsplit(solution[seq(from = idxVarStart[ii]+1, 
                                   to = idxVarEnd[ii]-1, by = 1), 1], 
                      split = " "), "[", 7)
    solMatrix[, ii] <- 
      gsub(pattern = "/>", replacement = "", 
           x = sapply(strsplit(ss1, split = "="), "[", 2))
    
  }
  
  return(solMatrix)
}

saveDiagnosticsCplex <- function(result, carnivalOptions){
  cplexLog <- carnivalOptions$filenames$cplexLog
  # add log to results
  if(!is.null(result) && file.exists(cplexLog)){
    cplex_out <- parseCplexLog(cplexLog)
    result$diagnostics <- cplex_out
  } else {
    result$diagnostics <- list()
  }
  
  return(result)
}

solveWithCplex <- function(solverPath, 
                           dirName, 
                           variables,
                           priorKnowledgeNetwork, 
                           perturbations, 
                           measurements) {
  
  # create temp file for logs
  cplex_log <- tempfile(pattern = "cplex_log_", tmpdir = tempdir(check = TRUE), fileext = ".txt")
  
  if (Sys.info()[1]=="Windows") {
    # TODO: implement logging on Win machine. 
    file.copy(from = solverPath,to = getwd())
    system(paste0("cplex.exe -f cplexCommand", ".txt"))
    file.remove("cplex.exe")
  } else {
    system(paste0(solverPath,
                  " -f cplexCommand", ".txt",
                  " | tee ", cplex_log)) # send output to logfile and stdout
  }
  
  
  
  ## Write result files in the results folder
  message("Saving results...")
  resList <- list()
  
  if (file.exists(paste0("results_cplex", ".txt"))) {
    #TODO several conditions per run are not supported, the loop can be removed?
    for(i in seq_len(length(variables))){
    
      res <- exportResultCplex(solutionFileName = paste0("results_cplex",
                                                    ".txt"),
                          variables = variables, 
                          conditionIDX = i,
                          pknList = priorKnowledgeNetwork, 
                          inputs = perturbations,
                          measurements = measurements)
      resList[[length(resList) + 1]] <- res
    }
    if (!is.null(res)) {
      if(!is.null(dirName)){
        if(dir.exists(dirName)){
          WriteDOTfig(res = res,
                      dir_name = dirName,
                      inputs = perturbations,
                      measurements = measurements,
                      UP2GS = FALSE)
        } else {
          warning("Specified directory does not exist. DOT figure not saved.")
        }
      }
    } else {
      message("No result to be written")
      return(NULL)
    }
  }
  
  ## Remove global variable 
  objs <- ls(pos = ".GlobalEnv")
  rm(list = objs[grep("pknList", objs)], pos = ".GlobalEnv")
  
  message(" ")
  message("--- End of the CARNIVAL pipeline ---")
  message(" ")
  
  result <- resList[[1]]
  
  # add log to results
  if(file.exists(cplex_log)){
    cplex_out <- parse_CPLEX_log(cplex_log)
    result$diagnostics = cplex_out
  }else{
    result$diagnostics = list()
  }
  
  return(result)
}
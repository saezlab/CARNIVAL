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
  result <- list()
  
  if (file.exists(paste0("results_cplex", ".txt"))) {
  
    result <- exportResultCplex(solutionFileName = paste0("results_cplex",
                                                       ".txt"),
                             variables = variables, 
                             #TODO this is a leftover from a loop (experimental condition), remove later!
                             conditionIDX = 1,
                             pknList = priorKnowledgeNetwork, 
                             inputs = perturbations,
                             measurements = measurements)
    
    #TODO might be removed later, this code is now in a separate function
    if (!is.null(result)) {
      if(!is.null(dirName)){
        if(dir.exists(dirName)){
          WriteDOTfig(res = result,
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
  
  # add log to results
  if(!is.null(result) && file.exists(cplex_log)){
    cplex_out <- parse_CPLEX_log(cplex_log)
    result$diagnostics = cplex_out
  }else{
    result$diagnostics = list()
  }
  
  return(result)
}
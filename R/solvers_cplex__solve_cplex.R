solveWithCplex <- function(solverPath, 
                           cplexCommandFilename,
                           dirName, 
                           runId,
                           outputFolder,
                           variables,
                           priorKnowledgeNetwork, 
                           perturbations, 
                           measurements) {
  
  # create temp file for logs
  cplexLog <- tempfile(pattern = "cplex_log_", tmpdir = tempdir(check = TRUE), 
                       fileext = ".txt")
  
  if (Sys.info()[1] == "Windows") {
    # TODO: implement logging on Win machine. 
    #TODO why copying exe and not directly executing it? 
    file.copy(from = solverPath, to = getwd())
    system(paste0("cplex.exe -f", cplexCommandFilename))
    file.remove("cplex.exe")
  } else {
    system(paste0(solverPath, " -f ", cplexCommandFilename,
                  " | tee ", cplexLog)) # send output to logfile and stdout
  }
  
  ## Write result files in the results folder
  message("Saving results...")
  result <- list()
  
  if (file.exists(carnivalOptions$filenames$resultFile)) {
    #TODO create these filesnames in one place
    resultFile <- arnivalOptions$filenames$resultFile
    result <- exportResultCplex(solutionFileName = resultFile,
                                variables = variables, 
                                priorKnowledgeNetwork = priorKnowledgeNetwork, 
                                perturbations = perturbations,
                                measurements = measurements)
  }
  
  # add log to results
  if(!is.null(result) && file.exists(cplexLog)){
    cplex_out <- parse_CPLEX_log(cplexLog)
    result$diagnostics <- cplex_out
  } else {
    result$diagnostics <- list()
  }
  
  return(result)
}
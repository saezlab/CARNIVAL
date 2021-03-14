## Solving CARNIVAL problem for one single experimental condition
##
## Enio Gjerga, 2020

 solveCarnivalSingleRun <- function(perturbations, 
                                   measurements, 
                                   measurementsSign, 
                                   measurementsWeights, 
                                   pathwayWeights, 
                                   priorKnowledgeNetwork, 
                                   carnivalOptions) {
  message("Writing constraints...")
  
  #Don't remove the line below, it breaks cplex runs
  priorKnowledgeNetwork <- as.data.frame(priorKnowledgeNetwork)
  
  variables <- writeLPFile(perturbations, 
                           measurements, 
                           measurementsSign, 
                           measurementsWeights, 
                           pathwayWeights, 
                           priorKnowledgeNetwork, 
                           carnivalOptions)
  
  
  message("Solving LP problem...")
  
  result <- c()
  #TODO rewrite with dep inj? 
  if(carnivalOptions$solver == supportedSolvers$cplex){
    result <- solveWithCplex(carnivalOptions$solverPath,
                             carnivalOptions$dirName, 
                             variables,
                             priorKnowledgeNetwork, 
                             perturbations, 
                             measurements) 
    
  } else if(carnivalOptions$solver == supportedSolvers$cbc) {
    #TODO add params
    result <- solveWithCbc()
  } else {
    #TODO add params
    result <- solveWithLpSolve()
  }
  
  return(result)
  
}

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
      res <- exportResult(solutionFileName = paste0("results_cplex",
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

#TODO params
solveWithCbc <- function() {
  
  resFile = paste0("results_cbc_", 1, "_", 1, ".txt")
  
  cbc_command <- paste0(solverPath, " testFile_", 1, "_", 
                        1, ".lp -seconds ", timelimit,
                        " -ratio ", poolrelGAP, 
                        " solve printi csv solu ", resFile)
  
  system(cbc_command)
  
  res <- exportResult(cplexSolutionFileName = resFile, 
                      variables = variables, 
                      conditionIDX = 1,
                      pknList = pknList, 
                      inputs = inputObj, 
                      measurements = measObj, 
                      solver = "cbc")
  
  if (!is.null(res)) {
    if(!is.null(dirName)){
      if(dir.exists(dirName)){
        WriteDOTfig(res=res,
                    dir_name = dirName,
                    inputs = inputObj,
                    measurements = measObj,
                    UP2GS = FALSE)
      } else {
        warning("Specified directory does not exist. DOT figure not saved.")
      }
    }
  } else {
    message("No result to be written")
    return(NULL)
  }
  
  return(res)
}

#TODO params
solveWithLpSolve <- function() {
  lpForm <- prepareLPMatrixSingle(variables = variables, measObj = measObj)
  
  lpSolution <- lp(direction = "min", objective.in = lpForm$obj, 
                   const.mat = lpForm$con, const.dir = lpForm$dir, 
                   const.rhs = lpForm$rhs, int.vec = lpForm$ints, 
                   binary.vec = lpForm$bins)$solution
  
  res <- exportResult(cplexSolutionFileName = NULL, variables = variables,
                      pknList = pknList, inputs = inputObj,
                      measurements = measObj, solver = solver,
                      lpSolution = lpSolution, mt = lpForm$mt,
                      conditionIDX = 1)
  
  if (!is.null(res)) {
    if(!is.null(dirName)){
      if(dir.exists(dirName)){
        WriteDOTfig(res=res,
                    dir_name=dirName,
                    inputs=inputObj,
                    measurements=measObj,
                    UP2GS=FALSE)
      } else {
        warning("Specified directory does not exist. DOT figure not saved.")
      }
    }
  } else {
    message("No result to be written")
    return(NULL)
  }
  
  return(res)
}

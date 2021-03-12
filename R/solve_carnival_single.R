## Solving CARNIVAL problem for one single experimental condition
##
## Enio Gjerga, 2020

solveCarnivalSingleRun <- function(perturbations, 
                                   measurements, 
                                   measurementsSign, 
                                   measurementsWeights, 
                                   pathwayWeights, 
                                   priorKnowledgeNetwork, 
                                   repIndex, 
                                   condition,
                                   carnivalOptions) {
  message("Creating LP file...")
  
  variables <- writeLPFile2(perturbations, 
                           measurements, 
                           measurementsSign, 
                           measurementsWeights, 
                           pathwayWeights, 
                           priorKnowledgeNetwork, 
                           repIndex, 
                           condition,
                           carnivalOptions)
  
  message("Done: Creating LP file...")
  message("Solving LP problem...")
  
  result <- c()
  #TODO rewrite with dep inj? 
  if(carnivalOptions$solver == supportedSolvers$cplex){
    result <- solveWithCplex(carnivalOptions$solverPath,
                             carnivalOptions$dirName, 
                             condition, 
                             repIndex, 
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

solveCARNIVALSingle <- function(data = data, pknList = pknList,
                                inputs = inputs, alphaWeight = alphaWeight,
                                betaWeight = betaWeight, scores = scores,
                                mipGAP = mipGAP, poolrelGAP = poolrelGAP,
                                limitPop = limitPop, poolCap = poolCap,
                                poolIntensity = poolIntensity,
                                poolReplace = poolReplace,
                                timelimit = timelimit,
                                threads = threads,
                                measWeights = measWeights, repIndex = repIndex,
                                condition = condition, solver = solver, 
                                solverPath = solverPath, variables = variables,
                                measObj = measObj, inputObj = inputObj, 
                                dir_name = dir_name){
  

  variables <- writeLPFile(data = data, pknList = pknList,
                           inputs = inputs, alphaWeight = alphaWeight,
                           betaWeight = betaWeight, scores = scores,
                           mipGAP = mipGAP, poolrelGAP = poolrelGAP,
                           limitPop = limitPop, poolCap = poolCap,
                           poolIntensity = poolIntensity,
                           threads = threads,
                           poolReplace = poolReplace, timelimit = timelimit,
                           measWeights = measWeights, repIndex = repIndex,
                           condition = condition)
  
  ## Solve ILP problem with cplex, remove temp files, 
  ## and return to the main directory
  message("Solving LP problem...")
  
  result <- c()
  #TODO rewrite with dep inj? 
  if(solver == "cplex"){
    #TODO add params
    result <- solveWithCplex(solverPath, condition, repIndex, variables,
                   pknList, inputObj, measObj, dir_name)

  } else if(solver == "cbc") {
    result <- solveWithCbc()
  } else {
    result <- solveWithLpSolve()
  }
  
  return(result)
}

solveWithCplex <- function(solverPath, 
                           dirName, 
                           condition, 
                           repIndex, 
                           variables,
                           priorKnowledgeNetwork, 
                           perturbations, 
                           measurements) {
  
  # create temp file for logs
  cplex_log <- tempfile(pattern = "cplex_log_", tmpdir = tempdir(check = TRUE), fileext = ".txt")
  
  if (Sys.info()[1]=="Windows") {
    # TODO: implement logging on Win machine. 
    file.copy(from = solverPath,to = getwd())
    system(paste0("cplex.exe -f cplexCommand_", 
                  condition, "_", repIndex, ".txt"))
    file.remove("cplex.exe")
  } else {
    system(paste0(solverPath,
                  " -f cplexCommand_", 
                  condition, "_", repIndex, ".txt",
                  " | tee ", cplex_log)) # send output to logfile and stdout
  }
  
  
  
  ## Write result files in the results folder
  message("Saving results...")
  resList <- list()
   
  if (file.exists(paste0("results_cplex_", condition, "_", repIndex,".txt"))) {
    for(i in seq_len(length(variables))){
      res <- exportResult(cplexSolutionFileName = paste0("results_cplex_",
                                                         condition,"_",
                                                         repIndex,".txt"),
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
  
  #cleanupCARNIVAL(condition = condition, repIndex = repIndex)
  
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

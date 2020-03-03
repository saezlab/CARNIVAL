## Solving CARNIVAL problem for one single experimental condition
##
## Enio Gjerga, 2020

solveCARNIVALSingle <- function(data = data, pknList = pknList,
                                inputs = inputs, alphaWeight = alphaWeight,
                                betaWeight = betaWeight, scores = scores,
                                mipGAP = mipGAP, poolrelGAP = poolrelGAP,
                                limitPop = limitPop, poolCap = poolCap,
                                poolIntensity = poolIntensity, DOTfig = DOTfig,
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
  print("Solving LP problem...")
  
  if(solver=="cplex"){
    
    if (Sys.info()[1]=="Windows") {
      file.copy(from = solverPath,to = getwd())
      system(paste0("cplex.exe -f cplexCommand_", 
                    condition,"_",repIndex,".txt"))
      file.remove("cplex.exe")
    } else {
      system(paste0(solverPath, " -f cplexCommand_", 
                    condition,"_",repIndex,".txt"))
    }
    
    ## Write result files in the results folder
    print("Saving results...")
    resList <- list()
    if (file.exists(paste0("results_cplex_",condition,"_",repIndex,".txt"))) {
      for(i in 1:length(variables)){
        res <- exportResult(cplexSolutionFileName = paste0("results_cplex_",
                                                           condition,"_",
                                                           repIndex,".txt"),
                            variables = variables, 
                            pknList = pknList, 
                            conditionIDX = i,
                            inputs=inputObj,
                            measurements=measObj)
        resList[[length(resList)+1]] <- res
      }
      if (!is.null(res)) {
        if (DOTfig) {WriteDOTfig(res=res,
                                 dir_name=dir_name,
                                 inputs=inputObj,
                                 measurements=measObj,
                                 UP2GS=FALSE)}
      }
    } else {
      print("No result to be written")
      return(NULL)
    }
    
    cleanupCARNIVAL(condition = condition, repIndex = repIndex)
    
    ## Remove global variable 
    objs <- ls(pos = ".GlobalEnv")
    rm(list = objs[grep("pknList", objs)], pos = ".GlobalEnv")
    
    print(" ")
    print("--- End of the CARNIVAL pipeline ---")
    print(" ")
    
    result = resList[[1]]
    
    return(result)
    
  } else {
    
    if(solver=="cbc"){
      
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
                          inputs=inputObj, 
                          measurements=measObj, 
                          solver = "cbc")
      
      if (!is.null(res)) {
        if (DOTfig) {WriteDOTfig(res=res,
                                 dir_name=dir_name,
                                 inputs=inputObj,
                                 measurements=measObj,
                                 UP2GS=FALSE)}
      }
      
      ## cleanupCARNIVAL(condition = condition, repIndex = repIndex)
      
      return(res)
      
    } else {
      
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
        if (DOTfig) {WriteDOTfig(res=res,
                                 dir_name=dir_name,
                                 inputs=inputObj,
                                 measurements=measObj,
                                 UP2GS=FALSE)}
      }
      
      return(res)
      
    }
    
  }
  
}

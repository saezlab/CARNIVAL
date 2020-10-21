## Returning result object after solving with CARNIVAL
##
## Enio Gjerga, 2020

solveCARNIVAL <- function(solverPath = solverPath, 
                          netObj = netObj, 
                          measObj = measObj, 
                          inputObj = inputObj, 
                          weightObj = weightObj,
                          timelimit = timelimit, 
                          mipGAP = mipGAP, 
                          poolrelGAP = poolrelGAP, 
                          limitPop = limitPop, 
                          poolCap = poolCap, 
                          poolIntensity = poolIntensity, 
                          poolReplace = poolReplace, 
                          alphaWeight = alphaWeight, 
                          betaWeight = betaWeight, 
                          dir_name = dir_name, 
                          solver = solver,
                          threads = threads,
                          experimental_conditions = experimental_conditions,
                          condition = condition,
                          repIndex = repIndex){
  
  ## Write constraints as ILP inputObj
  message("Writing constraints...")
  
  if(experimental_conditions[1]=="NULL"){experimental_conditions <- NULL}

  pknList <- as.data.frame(netObj)
  colnames(pknList) <- c("Node1", "Sign", "Node2")
  pknList$Node1 = as.character(pknList$Node1)
  pknList$Sign = as.character(as.numeric(as.character(pknList$Sign)))
  pknList$Node2 = as.character(pknList$Node2)
  
  ## Extracted sign of measurement for ILP fitting
  measurements <- sign(measObj)
  measWeights <- abs(measObj)
  
  ## Check the weight
  if(weightObj[1]=="NULL"){weightObj=NULL}
  
  pknList <<- pknList
  
  if(is.null(experimental_conditions)){
    
    result <- solveCARNIVALSingle(data = measurements, pknList = pknList, 
                                  inputs = inputObj, betaWeight = betaWeight, 
                                  scores = weightObj, mipGAP = mipGAP, 
                                  poolrelGAP = poolrelGAP, limitPop = limitPop,
                                  poolCap = poolCap, 
                                  poolIntensity = poolIntensity, 
                                  poolReplace = poolReplace, 
                                  timelimit = timelimit, 
                                  measWeights = measWeights,
                                  alphaWeight = alphaWeight,
                                  threads = threads,
                                  repIndex = repIndex, condition = condition,
                                  solver = solver, solverPath = solverPath,
                                  variables = variables, measObj = measObj,
                                  inputObj = inputObj,
                                  dir_name = dir_name)

  } else {

    stop("This version of CARNIVAL does not support analysis with multiple
         experimental conditions. Please split your data frame input objects
         into single experimental conditions.")

  }

}
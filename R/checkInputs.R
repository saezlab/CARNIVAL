## Error message in case of errors in the inputs
##
## Enio Gjerga, 2020

checkInputs <- function(solverPath=NULL,
                        netObj=NULL,
                        measObj=NULL,
                        inputObj=NULL,
                        weightObj=NULL,
                        DOTfig=TRUE,
                        timelimit=600,
                        mipGAP=0.05,
                        poolrelGAP=0.0001,
                        limitPop=500,
                        poolCap=100,
                        poolIntensity=4,
                        poolReplace=2,
                        alphaWeight=1,
                        betaWeight=0.2,
                        threads=0,
                        dir_name=paste0(getwd(), "/DOTfigures"),
                        solver="cbc"){
  
  returnList = list()
  checkSolver(solverPath = solverPath, solver = solver, dir_name = dir_name)
  netObj = checkNetwork(netObj = netObj)
  measObj = checkMeasObj(measObj = measObj, netObj = netObj)
  inputObj = checkInputObj(inputObj = inputObj, netObj = netObj)
  weightObj = checkWeightObj(weightObj = weightObj, netObj = netObj)
  pp = checkSolverParam(DOTfig=DOTfig, timelimit=timelimit, mipGAP=mipGAP,
                        poolrelGAP=poolrelGAP, limitPop=limitPop, poolCap=poolCap,
                        poolIntensity=poolIntensity, poolReplace=poolReplace,
                        threads=threads,
                        alphaWeight=alphaWeight, betaWeight=betaWeight)

  if(weightObj[1]!="NULL"){
    if(nrow(weightObj)!=nrow(measObj)){
      stop("Number of rows provided for the weightObj is different to measObj.
           Please check your inputs again.")
    }
  }

  if(!is.null(inputObj$inputs)){
    if(nrow(inputObj$inputs)!=nrow(measObj)){
      stop("Number of rows provided for the inputObj is different to measObj.
           Please check your inputs again.")
    }
  }

  if(nrow(measObj)==1){
    experimental_conditions = "NULL"
  } else {
    experimental_conditions = 1:nrow(measObj)
  }
  
  returnList[[length(returnList)+1]] = inputObj$network
  returnList[[length(returnList)+1]] = measObj
  returnList[[length(returnList)+1]] = inputObj
  returnList[[length(returnList)+1]] = weightObj
  returnList[[length(returnList)+1]] = pp$condition
  returnList[[length(returnList)+1]] = pp$repIndex
  returnList[[length(returnList)+1]] = experimental_conditions
  names(returnList) = c("network", "measurements", "inputs",
                        "weights", "condition", "repIndex", "exp")
  
  return(returnList)
  
}

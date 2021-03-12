## Error message in case of errors in the inputs
##
## Enio Gjerga, Olga Ivanova, Attila Gabor, 2020-2021

#' Check input data for CARNIVAL
#' 
#' checks the format of the main data inputs for CARNIVL. Checks the data format
#' and coverage of nodes in the PKN and data. All nodes in input_data and 
#' measured data must appear in the PKN 
#' 

#TODO adapt from COSMOS code
checkData <- function(perturbations,
                      measurements,
                      priorKnowledgeNetwork, 
                      pathwayWeights = NULL){
  
  stopifnot(is.vector(perturbations))
  stopifnot(is.vector(measurements))
  stopifnot(is.vector(pathwayWeights))
  
  stopifnot(is.data.frame(priorKnowledgeNetwork))
  stopifnot(all(c("source","interaction","target") %in% tolower(names(priorKnowledgeNetwork)))) 
  stopifnot(ncol(priorKnowledgeNetwork) == 3)
  
  #TODO maybe we need just a note, not a full stop of the run? 
  # check inputs and measurements are in the network
  stopifnot(all(names(perturbations) %in% c(priorKnowledgeNetwork$source, priorKnowledgeNetwork$target)))
  stopifnot(all(names(measurements) %in% c(priorKnowledgeNetwork$source, priorKnowledgeNetwork$target)))
  
  return(TRUE)
}


checkData2 <- function( perturbations,
                        measurements,
                        priorKnowledgeNetwork, 
                        pathwayWeights = NULL ) {

  checkPriorKnowledgeNetwork(priorKnowledgeNetwork = priorKnowledgeNetwork)
  netObj <- preprocessPriorKnowledgeNetwork(priorKnowledgeNetwork = priorKnowledgeNetwork)
  
  measObj = checkMeasurements(measurements = measurements, priorKnowledgeNetwork = priorKnowledgeNetwork)
  inputObj = checkPerturbationsData(perturbations = perturbations, priorKnowledgeNetwork = priorKnowledgeNetwork)
  weightObj = checkWeightObj(weightObj = pathwayWeights, netObj = priorKnowledgeNetwork)

  #TODO multiple experimental conditions are not going to be supported currently
  #if(nrow(measurements) == 1){
  experimentalConditions = "NULL"
  #} else {
      #experimental_conditions = seq_len(nrow(measObj))
  #}
  
  results <- list("priorKnowledgeNetwork" = inputObj$network, 
                  "measurements" = measObj, 
                  "perturbations" = inputObj, 
                  "weights" = weightObj, 
                  #TODO multiple experimental conditions are not going to be supported currently
                  "experimental_conditions" = experimentalConditions)

  return(results)
  
} 

#TODO add default options for lpSolve
checkInputs2 <- function(carnivalOptions){
  
  returnList = list() 
  checkSolver(solverPath = carnivalOptions$solverPath, 
              solver = carnivalOptions$solver, 
              dirName = carnivalOptions$dirName)
  
  pp = checkSolverParam(timelimit = carnivalOptions$timelimit, 
                        mipGAP = carnivalOptions$mipGap,
                        poolrelGAP = carnivalOptions$poolrelGap, 
                        limitPop = carnivalOptions$limitPop,
                        poolCap = carnivalOptions$poolCap,
                        poolIntensity = carnivalOptions$poolIntensity, 
                        poolReplace = carnivalOptions$poolReplace,
                        threads = carnivalOptions$threads,
                        alphaWeight = carnivalOptions$alphaWeight, 
                        betaWeight = carnivalOptions$betaWeight)
  
  
  returnList[[length(returnList)+1]] = pp$condition
  returnList[[length(returnList)+1]] = pp$repIndex
  
  names(returnList) = c("condition", "repIndex")
  
  return(returnList)
  
}

carnivalOptionsErrorChecks <- list(
  timelimit =     data.frame(func = "is.numeric",
                             param = "",
                             message="Error in parameter timelimit: set a time limit for ILP optimisation in
                             seconds, e.g. 3600"),
  
  threads =       data.frame(func =  c("is.numeric", "`>=`"),
                             param = c("","0"),
                             message=c("Error in parameter threads: set the number of threads to 0 for automatic
                                       detection or a value > 0 for a specific number of threads", 
                                       "Error in CPLEX parameter: set the number of threads above 0.")),
  
  alphaWeight =   data.frame(func = "is.numeric", 
                             param = "",
                             message = "Error in Objective Function, alphaWeight: Please set a weight for 
                             mismatch penalty (penalty will be
                             applied only when the weight of measurement is not defined)"),
  
  betaWeight =    data.frame(func = "is.numeric", 
                             param = "",
                             message = "Error in Objective Function, betaWeight: Please set a weight for node penalty")
  
)

cplexOptionsErrorChecks <- list(
  mipGap =        data.frame(func = "is.numeric", 
                             param = "", 
                             message = "Error in CPLEX parameter mipGap: set the allowed
                                       mipGAP parameter or leave it as NULL for using CPLEX
                                       default value (1e-04)"),
  
  poolrelGap =    data.frame(func = "is.numeric", 
                             param = "", 
                             message = "Error in CPLEX parameter poolrelGap: set the allowed pool relative GAP parameter
                                      or leave it as NULL for using CPLEX default value (1e75)"),
  
  limitPop =      data.frame(func = "is.numeric", 
                             param = "", 
                             message = "Error in CPLEX parameter limitPop: set the allowed population limit of solution
                                       to be generated or leave it as NULL for using CPLEX default value (20)"),
  
  poolCap =       data.frame(func = "is.numeric", 
                             param = "", 
                             message = "Error in CPLEX parameter poolCap: set the allowed number of solutions to be
                                        kept or leave it as NULL for using CPLEX default value (2.1e9)"),
  
  poolIntensity = data.frame(func = c("is.numeric", "`%in%`"), 
                             param = c("", "c(0:4)"),
                             message = c(rep("Error in CPLEX parameter poolIntensity: set the level of intensity for solution
                                         searching [0,1,2,3,4] or leave it as NULL for using CPLEX default value (0)
                                         - to be decided by CPLEX", 2))),
  
  poolReplace =   data.frame(func = c("is.numeric", "`%in%`"), 
                             param = c("", "c(0:2)"),
                             message = c(rep("Error in CPLEX parameter poolReplace: set the replacement strategy of solution
                                         [0,1,2]. CPLEX default value (0) - First In First Out", 2)))
  
)

#' check_CARNIVAL_options
#' 
#' checks options provided for CARNIVAL
#' 
checkCplexCarnivalOptions <- function(options) {
  
  if (!is.list(options))
    stop("CARNIVAL options should be a list")
  
  if (!all(requiredCarnivalCplexOptions %in% names(options))) {
    stop(
      "CARNIVAL options should contain all options.
            See/use default_carnival_options() for references. "
    )
  }
  
  if (!all(requiredCplexOptions) %in% names(options)) {
      stop(
        "CARNIVAL cplex options should contain all required cplex options.
            See/use default_carnival_options() for references."
      )
    }

  if (is.null(options$solverPath))
    stop("Path to ILP solver must be provided")

  if (options$solver == supportedSolvers$cplex &&
      options$solverPath == "")
    stop("Path to ILP solver cannot be empty")

  checkGenericFunction <- function(x, value) {
    functionToCall <- eval(parse(text = x['func']))
    if (x['param'] == "") {
      if (!functionToCall(value))
        stop(x['message'])
    } else {
      param <- eval(parse(text = x['param']))
      if (!functionToCall(value, param)) {
        stop(x['message'])
      }
    }
  }

  invisible(
    lapply(names(carnivalOptionsErrorMessages), function(x) {
            value = unlist(options[x])
            checkValue = carnivalOptionsErrorMessages[[x]]
    
            # if there are several checks, apply all
            if (is.data.frame(checkValue)) {
              apply(checkValue, 1, checkGenericFunction, value)
            } else {
              checkGenericFunction(checkValue, value)
            }
    })) 

    invisible(
      lapply(names(cplexOptionsErrorMessages), function(x) {
              value = unlist(options[x])
              checkValue = cplexOptionsErrorMessages[[x]]
              
              # if there are several checks, apply all
              if (is.data.frame(checkValue)) {
                apply(checkValue, 1, checkGenericFunction, value)
              } else {
                checkGenericFunction(checkValue, value)
              }
    }))
}



#TODO keeping now for tests and backward compatibility
checkInputs <- function(solverPath=NULL,
                        netObj=NULL,
                        measObj=NULL,
                        inputObj=NULL,
                        weightObj=NULL,
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
                        dir_name=dir_name,
                        solver="lpSolve"){
   
  returnList = list()
  checkSolver(solverPath = solverPath, solver = solver, dirName = dir_name)
  netObj = checkNetwork(netObj = netObj)
  measObj = checkMeasObj(measObj = measObj, netObj = netObj)
  inputObj = checkInputObj(inputObj = inputObj, netObj = netObj)
  weightObj = checkWeightObj(weightObj = weightObj, netObj = netObj)
  
  pp = checkSolverParam(timelimit=timelimit, mipGAP=mipGAP,
                        poolrelGAP=poolrelGAP, limitPop=limitPop, poolCap=poolCap,
                        poolIntensity=poolIntensity, poolReplace=poolReplace,
                        threads=threads,
                        alphaWeight=alphaWeight, betaWeight=betaWeight)


  if( nrow(measObj) == 1 ){
    experimental_conditions = "NULL"
  } else {
    experimental_conditions = seq_len(nrow(measObj))
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

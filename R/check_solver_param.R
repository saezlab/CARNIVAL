carnivalOptionsErrorChecks <- list(
  # solver =       data.frame( func = c("!is.null", "`%in%`"),
  #                            param = c("", "getSupportedSolvers()"),
  #                            message = paste0("Error in solver paramter: invalid value provided, you can used only", 
  #                                             getSupportedSolvers())),
  
  #solverPath = data.frame(func = c("is.character", "file.exists"), 
  #                        param = c("", ""),
  #                        message = "Error: Invalid path to solver provided.")
  
  
  betaWeight =    data.frame(func = "is.numeric", 
                             param = "",
                             message = "Error in Objective Function, betaWeight: Please set a weight for node penalty")
  
)

metaInfoOptionsErrorChecks <- list(
  
)

cplexOptionsErrorChecks <- list(
  timelimit =     data.frame(func = "is.numeric",
                             param = "",
                             message="Error in parameter timelimit: set a time limit for ILP optimisation in
                             seconds, e.g. 3600"),
  
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
                                         [0,1,2]. CPLEX default value (0) - First In First Out", 2))),
  
  threads =       data.frame(func =  c("is.numeric", "`>=`"),
                             param = c("","0"),
                             message=c("Error in parameter threads: set the number of threads to 0 for automatic
                                       detection or a value > 0 for a specific number of threads", 
                                       "Error in CPLEX parameter: set the number of threads above 0."))
  
)

### cbc
cbcOptionsErrorChecks <- list(
  timelimit = data.frame(func = "is.numeric",
                         param = "",
                         message="Error in parameter timelimit: set a time limit for ILP optimisation in
                             seconds, e.g. 3600"),
  
  poolrelGap = data.frame(func = "is.numeric", 
                          param = "", 
                          message="Error in cbc parameter poolrelGap: set the allowed pool relative GAP parameter
                                      or leave it as NULL for using cbc default value (1e75)")
  
)

getSolversCheckFunctions <- function(solver) {
  #TODO add check for the supported solvers
  
  solversCheckFunctions <- c("cplex" = cplexOptionsErrorChecks,
                             "cbc" = cbcOptionsErrorChecks, 
                             "lpSolve" = function(){})
  
  return(solversCheckFunctions$solver)
}

executeSolversChecks <- function(checksToRun) {
  
  #Executes the functions in "func" for each option with 
  #parameters in "param". The functions are expected to return 
  #logical TRUE/FALSE. Throws an error message if the function call
  #returned false. 
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
  
  print(checksToRun)
  invisible(
    lapply(names(checksToRun), function(x) {
      value = unlist(options[x])
      checkValue = checksToRun[[x]]
      
      # if there are several checks, apply all
      if (is.data.frame(checkValue)) {
        apply(checkValue, 1, checkGenericFunction, value)
      } else {
        checkGenericFunction(checkValue, value)
      }
    })) 
  
}

#'Checks options provided for CARNIVAL
#' 
#' @param options
#' @keywords internal
#' 
checkCarnivalOptions <- function(options) {
  
  if (!is.list(options))
    stop("CARNIVAL options should be a list")
  
  if (is.null(options$solver) || options$solver == "") {
    warning("Solver is not provided. Default solver will be used:", 
            getSupportedSolvers()$lpSolve)
    options$solver <- getSupportedSolvers()$lpSolve
  }
  
  missingOptions <- which(!getOptionsList(options$solver, onlyRequired = T) %in% 
                            names(options))
  
  if (length(missingOptions) > 0) {
    stop("CARNIVAL options should contain all required options.", 
          paste(missingOptions, "collapse" = ", "),
          "Check getOptionsList() for references.")
  }
  
  if ( (options$solver == getSupportedSolvers()$cplex ||
       options$solver == getSupportedSolvers()$cbc) &&
       (options$solverPath == "" || 
       is.null(options$solverPath)) )
    stop("Path to the solver cannot be empty.")
  
  solversCheckFunction <- getSolversCheckFunctions(options$solver)
  
  #Do general checks for all solvers
  executeSolversChecks(carnivalOptionsErrorChecks)
  
  #Do solver specific checks
  executeSolversChecks(solversCheckFunction)
  
}
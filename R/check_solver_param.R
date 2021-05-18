carnivalOptionsErrorChecks <- list(
  solver =       
    data.frame(
      func = c("is.character", "`%in%`"),
      param = c("", "getSupportedSolvers()"),
      message = paste0("Error in solver parameter: invalid value provided.")
      ), 
                     #  paste(getSupportedSolvers(), collapse = ", "))),
  
  betaWeight =   
    data.frame(
      func = "is.numeric", 
      param = "",
      message = "Error in betaWeight: Please set a numeric value of weight for node penalty. E.g. 0.2"
      )
  
)

metaInfoOptionsErrorChecks <- list(
  
)

cplexOptionsErrorChecks <- list(
  solverPath =   
    data.frame(
      func = c("is.character", "file.exists"), 
      param = c("", ""),
      message = "Error in solverPath: Invalid path to solver provided."
      ),
  
  timelimit =     
    data.frame(
      func = "is.numeric",
      param = "",
      message="Error in parameter timelimit: set a time limit for ILP optimisation in seconds, e.g. 3600"
      ),
  
  mipGap =
    data.frame(
      func = "is.numeric", 
      param = "", 
      message = "Error in CPLEX parameter mipGap: set the allowed
                                       mipGAP parameter or leave it as NULL for using CPLEX
                                       default value (1e-04)"
      ),
  
  poolrelGap =    
    data.frame(
      func = "is.numeric", 
      param = "", 
      message = "Error in CPLEX parameter poolrelGap: set the allowed pool relative GAP parameter
      or leave it as NULL for using CPLEX default value (1e75)"
      ),
  
  limitPop =
    data.frame(
      func = "is.numeric", 
      param = "", 
      message = "Error in CPLEX parameter limitPop: set the allowed population limit of solution
      to be generated or leave it as NULL for using CPLEX default value (20)"
      ),
  
  poolCap =
    data.frame(
      func = "is.numeric", 
      param = "", 
      message = "Error in CPLEX parameter poolCap: set the allowed number of solutions to be
      kept or leave it as NULL for using CPLEX default value (2.1e9)"
      ),
  
  poolIntensity = 
    data.frame(
      func = c("is.numeric", "`%in%`"), 
      param = c("", "c(0:4)"),
      message = c(rep("Error in CPLEX parameter poolIntensity: set the level of intensity for solution
      searching [0,1,2,3,4] or leave it as NULL for using CPLEX default value (0)
      - to be decided by CPLEX", 2))
      ),
  
  poolReplace =
    data.frame(
      func = c("is.numeric", "`%in%`"), 
      param = c("", "c(0:2)"),
      message = c("Error in CPLEX parameter poolReplace: set the replacement strategy of solution
                  [0,1,2]. CPLEX default value (0) - First In First Out")
      ),
  
  threads =
    data.frame(
      func =  c("is.numeric", "`>=`"),
      param = c("", "0"),
      message=c("Error in parameter threads: set the number of threads to 0 for automatic 
                detection or a value > 0 for a specific number of threads", 
                "Error in CPLEX parameter: set the number of threads above 0.")
      )
  
)

### cbc
cbcOptionsErrorChecks <- list(
  solverPath =   
    data.frame(
      func = c("is.character", "file.exists"), 
      param = c("", ""),
      message = "Error in solverPath: Invalid path to solver provided."
      ),
  
  timelimit = 
    data.frame(
      func = "is.numeric",
      param = "",
      message="Error in parameter timelimit: set a time limit for ILP optimisation in
      seconds, e.g. 3600"
      ),
  
  poolrelGap = 
    data.frame(
      func = "is.numeric", 
      param = "", 
      message="Error in cbc parameter poolrelGap: set the allowed pool relative GAP parameter
      or leave it as NULL for using cbc default value (1e75)"
      )
  
)

getSolversSpecificChecks <- function(solver) {
  #TODO add check for the supported solvers
  
  solversCheckFunctions <- list("cplex" = cplexOptionsErrorChecks,
                                "cbc" = cbcOptionsErrorChecks, 
                                "lpSolve" = c())
  
  return(solversCheckFunctions[[solver]])
}

executeSolversChecks <- function(carnivalOptions, checksToRun) {
  
  #Executes the functions in "func" for each option with 
  #parameters in "param". The functions are expected to return 
  #logical TRUE/FALSE. Returns an error message if the function call
  #returned false. 
  checkGenericFunction <- function(x, value) {
    stopMessage <- ""
    functionToCall <- eval(parse(text = x['func']))
    if (x['param'] == "") {
      if (!functionToCall(value))
        stopMessage <- x['message']
    } else {
      param <- eval(parse(text = x['param']))
      if (!functionToCall(value, param)) {
        stopMessage <- x['message']
      }
    }
    stopMessage <- gsub("[\r\n]", "", stopMessage)
    return(stopMessage)
  }
  
  errorMessages <- 
  invisible(
    lapply(names(checksToRun), function(x) {
      errorMessages <- list()
      value <- unlist(carnivalOptions[x])
      checkValue <- checksToRun[[x]]
      
      # if there are several checks for a single parameter, apply all
      if (is.data.frame(checkValue)) {
        errorMessages <- apply(checkValue, 1, checkGenericFunction, value)
      } else {
        errorMessages <- checkGenericFunction(checkValue, value)
      }
      #clean empty error messages
      errorMessages <- errorMessages[errorMessages != ""]
      errorMessages <- gsub("[\r\n]", "", errorMessages)
      
      return(errorMessages)
    })) 
  
  names(errorMessages) <- names(checksToRun)
  
  return(errorMessages)
}

#'Checks options provided for CARNIVAL
#' 
#' @param carnivalOptions all available carnival options 
#' @keywords internal
checkCarnivalOptions <- function(carnivalOptions) {
  
  if (!is.list(carnivalOptions))
    stop("CARNIVAL options should be a list")
  
  if (is.null(carnivalOptions$solver) || carnivalOptions$solver == "") {
    warning("Solver is not provided. Default solver will be used:", 
            getSupportedSolvers()$lpSolve)
    carnivalOptions$solver <- getSupportedSolvers()$lpSolve
  }
  
  missingOptions <- which(!getOptionsList(carnivalOptions$solver, 
                                          onlyRequired = T) %in% 
                                          names(carnivalOptions))
  
  if (length(missingOptions) > 0) {
    stop("CARNIVAL options should contain all required options.", 
          paste(missingOptions, "collapse" = ", "),
          "Check getOptionsList() for references.")
  }
  
  solversSpecificChecks <- getSolversSpecificChecks(carnivalOptions$solver)
  
  errorMessages <- executeSolversChecks(carnivalOptions, carnivalOptionsErrorChecks)
  errorMessages <- c(executeSolversChecks(carnivalOptions, solversSpecificChecks), 
                     errorMessages)
  
  collectedMessages <- unlist(errorMessages)
  
  if (length(collectedMessages) > 0) {
    stop("Incorrect parameters setup \n", paste(collectedMessages, collapse = "\n"))  
  }
  
  return(TRUE)
}
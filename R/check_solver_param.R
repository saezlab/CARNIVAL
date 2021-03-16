
carnivalOptionsErrorChecks <- list(
  #TODO solver and solverPath checks are not tested
  #solver =       data.frame( func = c("!is.null", "`%in%`")
  #                           param = c("", "supportedSolvers")
  #                           message = paste0("Error in solver paramter: invalid value provided, you can used only", 
  #                                            supportedSolvers))
  
  #solverPath = data.frame(func = c("is.character", "file.exists"), 
  #                        param = c("", ""),
  #                        message = "Error: Invalid path to solver provided.")
  
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

#TODO this function must be rewritten: separated to two, and other check for cbc and lpSolve should be added

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
  
  if (!all(requiredCplexOptions %in% names(options))) {
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
    lapply(names(carnivalOptionsErrorChecks), function(x) {
      value = unlist(options[x])
      checkValue = carnivalOptionsErrorChecks[[x]]
      
      # if there are several checks, apply all
      if (is.data.frame(checkValue)) {
        apply(checkValue, 1, checkGenericFunction, value)
      } else {
        checkGenericFunction(checkValue, value)
      }
    })) 
  
  invisible(
    lapply(names(cplexOptionsErrorChecks), function(x) {
      value = unlist(options[x])
      checkValue = cplexOptionsErrorChecks[[x]]
      
      # if there are several checks, apply all
      if (is.data.frame(checkValue)) {
        apply(checkValue, 1, checkGenericFunction, value)
      } else {
        checkGenericFunction(checkValue, value)
      }
    }))
}
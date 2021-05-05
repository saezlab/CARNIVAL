#' Returns the list of supported solvers.
#'
#' @return list of currently supported solvers.
#' @export
getSupportedSolvers <- function() {
  supportedSolvers <- list(cplex = "cplex", cbc = "cbc", lpSolve = "lpSolve")
  
  return(supportedSolvers)
}

#' Returns the list of options needed/supported for each solver.
#'
#' @param solver 
#' @param onlyRequired logic, set to TRUE if you want to obtain only
#' required options for the run
#'
#' @return
#' @export
#'
getOptionsList <- function(solver = "", onlyRequired = F) {
  requiredGeneralCarnivalOptions <- c("solver", "betaWeight")
  optionalCarnivalOptions <- c("lpFilename", "outputFolder", "cleanTmpFiles", 
                               "keepLPFiles")
  
  requiredCplexOptions <- c("solverPath", 
                            "timelimit", "mipGap", "poolrelGap", 
                            "limitPop", "poolCap", "poolIntensity", 
                            "poolReplace", "threads")
  optionalCplexOptions <- c("clonelog", "workdir", "cplexMemoryLimit")
  
  requiredLpSolveOptions <- c()
  requiredCbcOptions <- c("solverPath", "timelimit", "poolrelGap")
  
  if (onlyRequired) {
    validOptions <- list("cplex" = c(requiredGeneralCarnivalOptions, requiredCplexOptions),
                         "cbc" = c(requiredGeneralCarnivalOptions, requiredCbcOptions), 
                         "lpSolve" = c(requiredGeneralCarnivalOptions, requiredLpSolveOptions))
  } else {
    validOptions <- list("cplex" = c(requiredGeneralCarnivalOptions, requiredCplexOptions, 
                                     optionalCplexOptions, optionalCarnivalOptions), 
                         "cbc" = c(requiredGeneralCarnivalOptions, requiredCbcOptions,
                                   optionalCarnivalOptions),  
                         "lpSolve" = c(requiredGeneralCarnivalOptions, requiredLpSolveOptions,
                                       optionalCarnivalOptions))
  }
  
  if (solver != "") {
    if (solver %in% getSupportedSolvers()) {
      validOptions <- validOptions[[solver]]  
    } else {
      stop("Provided solver is not supported. List of supported solvers: ", 
           paste(getSupportedSolvers(), collapse=", "))
    }
  }
  
  return(validOptions)
}

#' Sets CARNIVAL options for the solver.
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
setCarnivalOptions <- function(solver = getSupportedSolvers()$lpSolve, ...) {
  if(length(list(...)) > 0) {
    if (checkOptionsValidity(solver = solver, ...)) {
      options <- list(..., solver = solver)
    } else {
      stop("Please correct options names. Use getOptionsList() to see all available options.")
    }  
  } else {
    options <- list(solver = solver)
  }
  
  return(options)
}


#' Checks if provided option names are valid. 
#'
#' @param solver 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
checkOptionsValidity <- function(solver = getSupportedSolvers()$lpSolve, ...) {
  options <- list(..., solver = solver)
  allOptionsValid <- TRUE

  invalidOptions <- which(!(names(options) %in% getOptionsList(solver)))
  
  if(is.null(names(options))) {
    warning("Empty options names provided. Use getOptionsList() to see all available options.")
    allOptionsValid <- FALSE
  }
  
  prepareWarningMessage <- paste0("#", invalidOptions, ":", 
                                  names(options[invalidOptions]), 
                                  "=", options[invalidOptions], collapse=", ")
  if(length(invalidOptions) > 0) {
    warning("Invalid options names provided (#:name=value): ", prepareWarningMessage,
            ". \nUse getOptionsList() to see all available options.")
    allOptionsValid <- FALSE
  }

  return(allOptionsValid)
}

#' Sets default CARNIVAL options for cplex.
#' 
#'@param solverPath Path to executable cbc/cplex file - default set to NULL, in
#'which case the solver from lpSolve package is used.
#'@param solver Solver to use: lpSolve/cplex/cbc (Default set to lpSolve).
#'@param timelimit CPLEX/Cbc parameter: Time limit of CPLEX optimisation in
#'seconds (default set to 3600).
#'@param mipGAP CPLEX parameter: the absolute tolerance on the gap between the
#'best integer objective and the objective of the best node remaining. When this
#'difference falls below the value of this parameter, the linear integer
#'optimization is stopped (default set to 0.05)
#'@param poolrelGAP CPLEX/Cbc parameter: Allowed relative gap of accepted
#'solution comparing within the pool of accepted solution (default: 0.0001)
#'@param limitPop CPLEX parameter: Allowed number of solutions to be generated
#'(default: 500)
#'@param poolCap CPLEX parameter: Allowed number of solution to be kept in the
#'pool of solution (default: 100)
#'@param poolIntensity CPLEX parameter: Intensity of solution searching
#'(0,1,2,3,4 - default: 4)
#'@param betaWeight Objective function: weight for node penalty (defaul: 0.2)
#'@param threads CPLEX parameter: Number of threads to use
#'default: 0 for maximum number possible threads on system
#'@param dirName Specify directory name to store results. by default set to
#'NULL
#'
#' 
#' @return returns a list with all possible options implemented in CARNIVAL.
#' see the documentation on \code{\link{runCARNIVAL}}.
#' @export
#' 
defaultCplexCarnivalOptions <- function(...){
    
    if ( "solver" %in% names(list(...)) ) {
      stop("Don't try to redefine solver in this function.",
            " Use other default functions for other solvers.")
    }  
      
    options <- list(
         solverPath = "",
         solver = getSupportedSolvers()$cplex,
         lpFilename = "",
         cplexCommandFilename = "",
         outputFolder = "",
         betaWeight = 0.2,
         cleanTmpFiles = TRUE,
         keepLPFiles = TRUE,
         dirName = NULL)
    
    options <- c(options, suggestedCplexSpecificOptions())
    manuallySetOptions <- setCarnivalOptions(solver = options$solver, ...)
    options <- options[!names(options) %in% names(manuallySetOptions)]
    options <- c(options, manuallySetOptions)
    
    return(options)
}


#' Sets default CARNIVAL options for lpSolve.
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
defaultLpSolveCarnivalOptions <- function(...){
  
  if ( "solver" %in% names(list(...)) ) {
    stop("Don't try to redefine solver in this function.",
         " Use other default functions for other solvers.")
  } 
  
  options <- list(
    solver = getSupportedSolvers()$lpSolve, 
    lpFilename = "",
    outputFolder = "",
    betaWeight = 0.2,
    cleanTmpFiles = TRUE,
    keepLPFiles = TRUE
  )
  
  manuallySetOptions <- setCarnivalOptions(solver = options$solver, ...)
  options <- options[!names(options) %in% names(manuallySetOptions)]
  options <- c(options, manuallySetOptions)
  
  return(options)
}


#'  Sets default CARNIVAL options for cbc.
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
defaultCbcSolveCarnivalOptions <- function(...){
  
  if ( "solver" %in% names(list(...)) ) {
    stop("Don't try to redefine solver in this function.",
         " Use other default functions for other solvers.")
  } 
  
  options <- list(
    solver = getSupportedSolvers()$cbc, 
    solverPath = solverPath,
    lpFilename = "",
    outputFolder = "",
    betaWeight = 0.2,
    cleanTmpFiles = TRUE,
    keepLPFiles = TRUE
  )
  
  manuallySetOptions <- setCarnivalOptions(solver = options$solver, ...)
  options <- options[!names(options) %in% names(manuallySetOptions)]
  options <- c(options, manuallySetOptions)
  
  return(options)
}

#' Suggests cplex specific options.s
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
suggestedCplexSpecificOptions <- function(...) {
  
  if ( "solver" %in% names(list(...)) ) {
    stop("Don't try to redefine solver in this function.",
         " Use other default functions for other solvers.")
  } 
  
  options <- list(
    threads = 1,
    clonelog = -1,
    workdir = ".",
    mipGap = 0.05,
    timelimit = 3600, 
    poolrelGap = 0.0001,
    limitPop = 500,
    poolCap = 100,
    poolIntensity = 4,
    poolReplace = 2,
    cplexMemoryLimit = 8192
  )
  
  manuallySetOptions <- setCarnivalOptions(solver = getSupportedSolvers()$cplex, ...)
  options <- options[!names(options) %in% names(manuallySetOptions)]
  options <- c(options, manuallySetOptions)
  
  return(options)
}

#' Suggests cbc specific options.
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
suggestedCbcSpecificOptions <- function(...) {
  
  if ( "solver" %in% names(list(...)) ) {
    stop("Don't try to redefine solver in this function.",
         " Use other default functions for other solvers.")
  } 
  
  options <- list(
    timelimit = 3600, 
    poolrelGap = 0.0001
  )
  
  manuallySetOptions <- setCarnivalOptions(solver = getSupportedSolvers()$cbc, ...)
  options <- options[!names(options) %in% names(manuallySetOptions)]
  options <- c(options, manuallySetOptions)
  
  return(options)
}


#' Sets default options from cplex documentation.
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
defaultCplexSpecificOptions <- function(...) {
  #TODO N.B. careful with scientific notation, it is switched off at another place in the code 
  # (look up for scipen) - options(scipen = 0) to switch it on
  
  if ( "solver" %in% names(list(...)) ) {
    stop("Don't try to redefine solver in this function.",
         " Use other default functions for other solvers.")
  } 
  
  options <- list(
        threads = 1,
        mipGap = 1e-04, 
        poolrelGap = 1e75,
        limitPop = 20,
        poolCap = 2.1e9,
        poolIntensity = 0,
        poolReplace = 0,
        timelimit = 1e+75)
  
  manuallySetOptions <- setCarnivalOptions(solver = getSupportedSolvers()$cplex, ...)
  options <- options[!names(options) %in% names(manuallySetOptions)]
  options <- c(options, manuallySetOptions)
  
  return(options)
}


#' Read options from json file.
#'
#' @param jsonFileName 
#'
#' @return
#' @export
#'
#' @examples
readOptions <- function(jsonFileName = "parameters/carnival_cplex_parameters.json") {
  if ("rjson" %in% (.packages())) {
    message("Loading parameters file for CARNIVAL:", jsonFileName)
    parameters <- fromJSON(file = jsonFileName)  
    return(parameters)  
  } else {
    stop("Cannot read parameters from json: rjson package should be installed and loaded.")
  }
}



## Solving CARNIVAL problem 
##
## Enio Gjerga, Olga Ivanova 2020-2021

supportedSolversFunctions <- list("cplex" = c("solve" = solveWithCplex, 
                                              "getSolutionMatrix" = getSolutionMatrixCplex,
                                              "export" = exportIlpSolutionFromSolutionMatrix, 
                                              "saveDiagnostics" = saveDiagnosticsCplex),
                                  
                                  "cbc" =   c("solve" = solveWithCbc, 
                                              "getSolutionMatrix" = getSolutionMatrixCbc, 
                                              "export" = exportIlpSolutionFromSolutionMatrix), 
                                  
                                  "lpSolve" = c("solve" = solveWithLpSolve, 
                                                "getSolutionMatrix" = getSolutionMatrixLpSolve,
                                                "export" = exportIlpSolutionFromSolutionMatrix)) 


prepareForCarnivalRun <- function(dataPreprocessed,
                                  carnivalOptions, 
                                  newDataRepresentation = F) {
  
  intDataRep <- createInternalDataRepresentation( dataPreprocessed, newDataRepresentation )
  writeParsedData( intDataRep, dataPreprocessed, carnivalOptions )
  
  if(newDataRepresentation) {
    lpFormulation <- createLpFormulation_v2( intDataRep, dataPreprocessed, 
                                             carnivalOptions )
    variables <- intDataRep 
    
    if(carnivalOptions$solver == supportedSolvers$lpSolve) {
      stop("Runs of LpSolve with new data representation are not supported yet. Please use newDataRepresentation=T for lpSolve.")
    }
    
  } else {
    lpFormulation <- createLpFormulation( intDataRep, dataPreprocessed, 
                                          carnivalOptions )
    variables <- intDataRep[[2]]
    
    if(carnivalOptions$solver == supportedSolvers$lpSolve) {
      variables <- transformVariables(varialbes, dataPreprocessed$measurements)
    }
  }
  
  writeSolverFile(objectiveFunction = lpFormulation$objectiveFunction,
                  allConstraints = lpFormulation$allConstraints,
                  bounds = lpFormulation$bounds,
                  binaries = lpFormulation$binaries,
                  generals = lpFormulation$generals,
                  carnivalOptions = carnivalOptions)
  
  return(variables)
}


solveCarnivalFromLp <- function(lpFile = "", 
                                parsedDataFile = "",
                                carnivalOptions) {
  load(parsedDataFile) 
  dataPreprocessed$measurements <- measurements
  dataPreprocessed$priorKnowledgeNetwork <- priorKnowledgeNetwork
  dataPreprocessed$perturbations <- perturbations
  dataPreprocessed$weights <- weights
  
  carnivalOptions$lpFilename <- lpFile
  solutionMatrix <- sendTaskToSolver( variables, dataPreprocessed, carnivalOptions )
  result <- processSolution( solutionMatrix, variables, carnivalOptions )
  
  return(result)
}

solveCarnival <- function( dataPreprocessed,
                           carnivalOptions, 
                           newDataRepresentation = F) {
  
  variables <- prepareForCarnivalRun(dataPreprocessed, carnivalOptions, newDataRepresentation)
  solutionMatrix <- sendTaskToSolver( variables, dataPreprocessed, carnivalOptions )
  result <- processSolution( solutionMatrix, variables, carnivalOptions )
  
  #TODO results with diagnostics is never null, think how to implement it better
  #if (!is.null(result)) {
  #  writeDotFigure(result = result,
  #              dir_name = outputFolder,
  #              inputs = perturbations,
  #              measurements = measurements,
  #              UP2GS = FALSE)
  #}
  
  return(result)
}

sendTaskToSolver <- function( variables,
                              dataPreprocessed, 
                              carnivalOptions ) {
  
  message("Solving LP problem...")
  
  solversFunctions <- supportedSolversFunctions[[carnivalOptions$solver]]
  
  #TODO remove variables, we don't need them for everything except lpSolver
  lpSolution <- solversFunctions$solve( variables = variables, 
                                        carnivalOptions = carnivalOptions,
                                        dataPreprocessed )
  
  message("Done: Solving LP problem.")
  
  message("Writing the solution matrix")
  solutionMatrix <- solversFunctions$getSolutionMatrix( lpSolution )
  message("Done: Writing the solution matrix")
  
  return(solutionMatrix)
}


processSolution <- function(solutionMatrix, 
                            variables,
                            carnivalOptions) {
  message("Exporting solution matrix...")
  solversFunctions <- supportedSolversFunctions[[carnivalOptions$solver]]
  
  result <- solversFunctions$export( solutionMatrix = solutionMatrix, 
                                     variables = variables, 
                                     dataPreprocessed )
  
  if (carnivalOptions$solver == supportedSolvers$cplex) {
    result <- solversFunctions$saveDiagnostics(result, carnivalOptions)
  } 
  
  print(result)
  message("Done: exporting solution matrix.")
  return(result)
}


createInternalDataRepresentation <- function( dataPreprocessed, newDataRepresentation = F ) {
  if (newDataRepresentation) {
    variables <- createVariablesForIlpProblem(dataPreprocessed)
    return(variables)
    
  } else {
    dataVector <- buildDataVector(measurements = dataPreprocessed$measurements, 
                                  priorKnowledgeNetwork = dataPreprocessed$priorKnowledgeNetwork, 
                                  perturbations = dataPreprocessed$perturbations)
    
    variables <- createVariables(priorKnowledgeNetwork = dataPreprocessed$priorKnowledgeNetwork, 
                                 dataVector = dataVector)
    return(list("dataVector" = dataVector, "variables" = variables))
  }
}

writeParsedData <- function ( variables = variables, 
                              dataPreprocessed = dataPreprocessed, 
                              carnivalOptions = carnivalOptions,
                              filename="parsedData.RData") {
  message("Saving parsed data")
  
  outputFolder <- carnivalOptions$outputFolder
  parsedDataFilename <-carnivalOptions$filenames$parsedData
  save(variables, 
       dataPreprocessed,
       file = parsedDataFilename)
}
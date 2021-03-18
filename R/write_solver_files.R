## Returning result object after solving with CARNIVAL
##
## Enio Gjerga, 2020

writeSolverFile <- function(objectiveFunction = objectiveFunction,
                            allConstraints = allConstraints, 
                            bounds = bounds, 
                            binaries = binaries,
                            generals = generals, 
                            carnivalOptions = carnivalOptions){
  
  ## write the .lp file

  lpFilename = paste0("lpFile", "_", carnivalOptions$runId, ".lp")
  
  #TODO Bartosz and Olga: why do we need enter Problem here? 
  write("enter Problem", lpFilename)
  write("", lpFilename, append = TRUE)
  write("Minimize", lpFilename, append = TRUE)
  write(objectiveFunction, lpFilename, append = TRUE)
  write("Subject To", lpFilename, append = TRUE)
  write(allConstraints, lpFilename, append = TRUE)
  write("Bounds", lpFilename, append = TRUE)
  write(bounds, lpFilename, append = TRUE)
  write("Binaries", lpFilename, append = TRUE)
  write(binaries, lpFilename, append = TRUE)
  write("Generals", lpFilename, append = TRUE)
  write(generals, lpFilename, append = TRUE)
  write("End", lpFilename, append = TRUE)
}

writeParsedData <- function ( variables = variables, 
                              priorKnowledgeNetwork = priorKnowledgeNetwork, 
                              perturbations = perturbations,
                              measurements = measurements,
                              carnivalOptions = carnivalOptions,
                              filename="parsedData.RData") {
  
  filename <- paste0("parsedData_", carnivalOptions$runId, ".RData")
  save(variables, priorKnowledgeNetwork, perturbations, measurements, file=filename)
}


## Returning result object after solving with CARNIVAL
##
## Enio Gjerga, 2020

writeSolverFile <- function(objectiveFunction = objectiveFunction,
                            allConstraints = allConstraints, 
                            bounds = bounds, 
                            binaries = binaries,
                            generals = generals, 
                            carnivalOptions){
  
  ## write the .lp file

  lpFilename = paste0("testFile", ".lp")
  
  #TODO Olga c(...)
  
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
                              filename="parsedData.RData") {
  save(variables, priorKnowledgeNetwork, perturbations, measurements, file=filename)
}


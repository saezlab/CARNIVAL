## Returning result object after solving with CARNIVAL
##
## Enio Gjerga, 2020

writeSolverFile <- function(objectiveFunction = objectiveFunction,
                            allConstraints = allConstraints, 
                            bounds = bounds, 
                            binaries = binaries,
                            generals = generals, 
                            carnivalOptions = carnivalOptions){
  message("Saving LP file")
  
  ## write the .lp file
  lpFilename <- carnivalOptions$filenames$lpFilename
  
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
  
  message("Done: Saving LP file: ", carnivalOptions$filenames$lpFilename)
}



writeCplexCommandFile <- function(carnivalOptions){

  message("Writing cplex command file")
  
  lpFilename <- carnivalOptions$filenames$lpFilename
  resultCplexFile <- carnivalOptions$filenames$resultFile
  cplexCommandFilename <- carnivalOptions$filenames$cplexCommandFile
  
  write(paste0("read ", lpFilename), 
        cplexCommandFilename, append = TRUE)
  write(paste0("set mip tolerances mipgap ", carnivalOptions$mipGap), 
        cplexCommandFilename, append = TRUE)
  write(paste0("set mip pool relgap ", carnivalOptions$poolrelGap), 
        cplexCommandFilename, append = TRUE)
  write(paste0("set mip pool replace ", carnivalOptions$poolReplace), 
        cplexCommandFilename, append = TRUE)
  write(paste0("set mip limits populate ", carnivalOptions$limitPop), 
        cplexCommandFilename, append = TRUE)
  write(paste0("set mip pool capacity ", carnivalOptions$poolCap), 
        cplexCommandFilename, append = TRUE)
  write(paste0("set mip pool intensity ", carnivalOptions$poolIntensity), 
        cplexCommandFilename, append = TRUE)
  write(paste0("set timelimit ", carnivalOptions$timelimit), cplexCommandFilename, append = TRUE)
  write(paste0("set threads ", carnivalOptions$threads), cplexCommandFilename, append = TRUE)
  write(paste0("set output clonelog ", carnivalOptions$clonelog), cplexCommandFilename, append = TRUE)
  write(paste0("set workdir ", carnivalOptions$workdir), cplexCommandFilename, append = TRUE)
  write("populate", cplexCommandFilename, append = TRUE)
  write(paste0("write ", resultCplexFile, " sol all"), 
        cplexCommandFilename, append = TRUE)
  write("quit", cplexCommandFilename, append = TRUE)
  
  message("Done: writing cplex command file")
  return(cplexCommandFilename)
}

writeCplexCommandFileFromJson <- function(carnivalOptions, 
                                          jsonFileName = "parameters/cplex_parameters_cmd_file.json") {
  message("Writing cplex command file")
  
  message("Loading parameters file for cplex command file:", jsonFileName)
  cplexCommands <- fromJSON(file = jsonFileName)   
  cplexCommandsFilename <- carnivalOptions$cplexCommandFilename 
  
  params <- lapply(seq(1:length(cplexCommands)), function(i) {
    parameterName <- names(cplexCommands[i])
    if (parameterName == "") {
      write(cplexCommands[[i]], cplexCommandsFilename, append=TRUE)
    } else {
      write(paste(cplexCommands[parameterName], carnivalOptions[parameterName]), 
            cplexCommandsFilename, append=TRUE)
    }
  })
  
}

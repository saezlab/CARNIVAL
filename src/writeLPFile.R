writeLPFile <- function(data = data, pknList = pknList, inputs = inputs, cutoff = 0.1, alphaWeight=100, betaWeight=20, scores=scores, mipGAP=0.1, poolrelGAP=0.01, timelimit=1800, nodeWeights=nodeWeights) {
  dataMatrix <- buildDataMatrix(data = data, pknList = pknList, inputs = inputs, cutoff = 0.1)
  variables <- create_variables_all(pknList = pknList, dataMatrix = dataMatrix)
  # distVariables <- write_dist_variables(pknList = pknList)
  oF <- write_objective_function_all(dataMatrix = dataMatrix, variables = variables, alphaWeight = alphaWeight, betaWeight = betaWeight, scores=scores, nodeWeights=nodeWeights)
  bounds <- write_boundaries(variables = variables, oF=oF)
  binaries <- write_binaries(variables = variables)
  generals <- write_generals(variables = variables, oF = oF)
  c0 <- write_constraints_objFunction_all(variables = variables, dataMatrix = dataMatrix)
  c1 <- write_constraints_1_all(variables = variables)
  c2 <- write_constraints_2_all(variables = variables)
  c3 <- write_constraints_3_all(variables = variables)
  c4 <- write_constraints_4_all(variables = variables)
  c5 <- write_constraints_5_all(variables = variables)
  c6 <- write_constraints_6(variables = variables, dataMatrix = dataMatrix, inputs = inputs)
  c7 <- write_constraints_7(variables = variables, dataMatrix = dataMatrix, inputs = inputs)
  c8 <- write_constraints_8(variables = variables, inputs = inputs, pknList = pknList)
  c9 <- write_loop_constraints(variables = variables, pknList = pknList, inputs = inputs)
  allC <- all_constraints_wLoop(c0 = c0, c1 = c1, c2 = c2, c3 = c3, c4 = c4, c5 = c5, c6 = c6, c7 = c7, c8 = c8, c9 = c9)
  
  # write the .lp file
  data = "testFile.lp"
  write("enter Problem", data)
  write("", data, append = TRUE)
  write("Minimize", data, append = TRUE)
  write(oF, data, append = TRUE)
  write("Subject To", data, append = TRUE)
  write(allC, data, append = TRUE)
  write("Bounds", data, append = TRUE)
  write(bounds, data, append = TRUE)
  write("Binaries", data, append = TRUE)
  write(binaries, data, append = TRUE)
  write("Generals", data, append = TRUE)
  write(generals, data, append = TRUE)
  write("End", data, append = TRUE)
  
  # write cplexCommand file
  cplexCommand <- "cplexCommand.txt"
  write("read testFile.lp", cplexCommand, append = TRUE)
  write(paste0("set mip tolerances mipgap ",mipGAP), cplexCommand, append = TRUE)
  write(paste0("set mip pool relgap ",poolrelGAP), cplexCommand, append = TRUE)
  write(paste0("set timelimit ",timelimit), cplexCommand, append = TRUE)
  write("optimize", cplexCommand, append = TRUE)
  write("populate", cplexCommand, append = TRUE)
  # write(paste0("write results_cplex.txt sol"), cplexCommand, append = TRUE)
  write(paste0("write results_cplex.txt sol all"), cplexCommand, append = TRUE)
  write("quit", cplexCommand, append = TRUE)
  
  # for(i in 1:length(variables)){
  #   
  #   write(variables[[i]]$variables, data, append = TRUE)
  # 
  #   for(j in 1:ncol(inputs)){
  #     
  #     write(variables[[i]]$dist[[j]], data, append = TRUE)
  #     
  #   }
  #   
  # }
  # write("End", data, append = TRUE)
  
  return(variables)
}
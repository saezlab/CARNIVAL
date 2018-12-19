#'\code{writeLPFile}
#'
#' Write a list of linear programming (LP) constraints into a file while will be read by interactive cplex solver to perform network optimisation.
#' 
#' @param data The discretised observations (here transcription factor activities) of values [-1,0,1]
#' @param pknList The prior knowledge network
#' @param inputs The list of known or potential target of perturbation 
#' @param alphaWeight The weight for mismatch penalty between discretised observations and predicted model states
#' @param betaWeight The weight for network size (node) penalty
#' @param scores The continuous pathway scores from PROGENy
#' @param mipGAP The minimal integer programming percentage gap to be accepted as a solution
#' @param poolrelGAP The allowed relative percentage gap between the best solution and the equivalent solutions in the solution pool
#' @param limitPop The number of allowed solutions to be populated
#' @param poolCap The number of solutions to be kept in the pool of solution 
#' @param poolIntensity The intensity of the search in solution space
#' @param poolReplace The replacement strategy of the solutions in the solution pool
#' @param timelimit The allowed amount of time (in seconds) for the optimisation
#' @param measWeights The countinous weight of observations (here transcription factor activities) - to replace the default alphaWeight if assigned; Note: take only positive values!
#' @param repIndex The indexing of optimisation - useful in case more than one experiment is performed
#' @param condition The free variable which could be assigned for additional study e.g. to vary the efect of betaWeight in a loop
#' 
#' @return An integer programming file containing the description of ILP optimisation problem and a cplex command file to communicate with the interactive version of cplex solver
#'
#' @export

writeLPFile <- function(data = data, pknList = pknList, inputs = inputs, alphaWeight=1, betaWeight=0.2, scores=scores, mipGAP=0.1, poolrelGAP=0.01, limitPop=100, poolCap=100, poolIntensity=0, poolReplace=2,timelimit=1800,measWeights=NULL, repIndex, condition="") {
  dataMatrix <- buildDataMatrix(data = data, pknList = pknList, inputs = inputs)
  variables <- create_variables_all(pknList = pknList, dataMatrix = dataMatrix)
  # distVariables <- write_dist_variables(pknList = pknList)
  oF <- write_objective_function_all(dataMatrix = dataMatrix, variables = variables, alphaWeight = alphaWeight, betaWeight = betaWeight, scores=scores, measWeights=measWeights)
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
  data = paste0("testFile_", condition,"_",repIndex,".lp")
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
  cplexCommand <- paste0("cplexCommand_", condition,"_",repIndex,".txt")
  write(paste0("read testFile_", condition,"_",repIndex,".lp"), cplexCommand, append = TRUE)
  write(paste0("set mip tolerances mipgap ",mipGAP), cplexCommand, append = TRUE)
  write(paste0("set mip pool relgap ",poolrelGAP), cplexCommand, append = TRUE)
  write(paste0("set mip pool replace ", poolReplace), cplexCommand, append = TRUE)
  write(paste0("set mip limits populate ",limitPop), cplexCommand, append = TRUE)
  write(paste0("set mip pool capacity ",poolCap), cplexCommand, append = TRUE)
  write(paste0("set mip pool intensity ",poolIntensity), cplexCommand, append = TRUE)
  write(paste0("set timelimit ",timelimit), cplexCommand, append = TRUE)
  # write("optimize", cplexCommand, append = TRUE)
  write("populate", cplexCommand, append = TRUE)
  # write(paste0("write results_cplex.txt sol"), cplexCommand, append = TRUE)
  write(paste0("write results_cplex_", condition,"_",repIndex,".txt sol all"), cplexCommand, append = TRUE)
  write("quit", cplexCommand, append = TRUE)

  return(variables)
}

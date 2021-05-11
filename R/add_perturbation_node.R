#' Introduces a perturbation node connecting periphery nodes without a target in 
#' the prior knowledge network.
#'
#' @param priorKnowledgeNetwork data.frame with priorKnowledgeNetwork with source, interaction, target columns
#' @return data.frame with prior knowledge network with added perturbations
#' @author Panuwat Trairatphisan, 2020
#' 
#' @keywords internal
addPerturbationNodes <- function(priorKnowledgeNetwork) {
  
  priorKnowledgeNetwork <- as.matrix(priorKnowledgeNetwork)
  
  sourceList <- sort(unique(priorKnowledgeNetwork[, 1]))
  targetList <- sort(unique(priorKnowledgeNetwork[, 3]))
  inputList <-  setdiff(sourceList, targetList)
  
  #Create a set of rows for each possible input with all possible activities
  #(1/-1, activation/inhibiton)
  addToNet <- data.frame(matrix(NA, length(inputList) * 2, 3))
  addToNet[, 1] <- "Perturbation"
  
  addToNet[seq_len(length(inputList)), 2] <- "1";
  addToNet[seq_len(length(inputList)), 3] <- inputList
  addToNet[seq(from = length(inputList) + 1, 
               to = length(inputList) * 2, by = 1), 2] <- "-1";
  addToNet[seq(from = length(inputList) + 1,
               to = length(inputList) * 2, by = 1), 3] <- inputList
  
  colnames(addToNet) <- colnames(priorKnowledgeNetwork)
  changedPriorKnowledgeNetwork <- rbind(priorKnowledgeNetwork, addToNet)
  
  return(changedPriorKnowledgeNetwork)
}

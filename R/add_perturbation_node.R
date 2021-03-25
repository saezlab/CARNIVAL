## Introduces a perturbation node connecting periphery nodes without a target in 
## the prior knowledge network.
## 
## Panuwat Trairatphisan, 2020

addPerturbationNodes <- function(priorKnowledgeNetwork) {
  
  priorKnowledgeNetwork <- as.matrix(priorKnowledgeNetwork)
  
  sourceList <- sort(unique(priorKnowledgeNetwork[, 1]))
  targetList <- sort(unique(priorKnowledgeNetwork[, 3]))
  inputList <-  setdiff(sourceList, targetList)
  
  #Create a set of rows for each possible input with all possible activities (1/-1, activation/inhibiton)
  addToNet <- data.frame(matrix(NA, length(inputList) * 2, 3))
  addToNet[, 1] <- "perturbation"
  
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

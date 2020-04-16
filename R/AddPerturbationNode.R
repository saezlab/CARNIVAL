## Introduces a perturbation node connecting periphery nodes without a target in 
## the prior knowledge network.
## 
## Panuwat Trairatphisan, 2020

AddPerturbationNode <- function(network) {
  
  sourceList <- sort(unique(network[,1]))
  targetList <- sort(unique(network[,3]))
  inputList <-  setdiff(sourceList,targetList)
  
  AddToNet <- data.frame(matrix(NA,length(inputList)*2,3))
  AddToNet[,1] <- "Perturbation"
  AddToNet[seq_len(length(inputList)),2] <- "1";
  AddToNet[seq_len(length(inputList)),3] <- inputList
  AddToNet[seq(from = length(inputList)+1, 
               to = length(inputList)*2, by = 1),2] <- "-1";
  AddToNet[seq(from = length(inputList)+1,
               to = length(inputList)*2, by = 1),3] <- inputList
  colnames(AddToNet) <- colnames(network)
  network <- rbind(network,AddToNet)
  inputs <- data.frame("NaN"); colnames(inputs) <- "Perturbation"
  MappedPertNode <- list(inputs=inputs,network=network)
  
  return(MappedPertNode)
}

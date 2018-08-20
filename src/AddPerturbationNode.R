AddPerturbationNode <- function(inputs,network) {
  NameInput <- names(inputs)
  AddToNet <- data.frame(matrix(NA,length(NameInput)*2,3))
  AddToNet[,1] <- "Perturbation"
  AddToNet[1:length(NameInput),2] <- "1";AddToNet[1:length(NameInput),3] <- NameInput
  AddToNet[(length(NameInput)+1):(length(NameInput)*2),2] <- "-1";AddToNet[(length(NameInput)+1):(length(NameInput)*2),3] <- NameInput
  colnames(AddToNet) <- colnames(network)
  network <- rbind(network,AddToNet)
  inputs <- data.frame("NaN"); colnames(inputs) <- "Perturbation"
  MappedPertNode <- list(inputs=inputs,network=network)
  return(MappedPertNode)
}

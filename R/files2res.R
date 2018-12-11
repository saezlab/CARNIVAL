#'\code{files2res}
#'
#' Read in a list of exported/written network and node activities files into a variable for plotting
#' 
#' @param counterlist Indices of model solutions to be read-in
#'
#' @return A list of variables for plotting functions
#'
#' @export

files2res <- function(counterlist) {

  res <- list()

  res$weightedSIF <- as.matrix(read.table("weightedModel_1.txt",header = T,sep = "\t",stringsAsFactors = F)%>%filter(Node1!='Perturbation'))
  res$nodeAttributes <- as.matrix(read.table("nodesAttributes_1.txt",header = T,sep = "\t",stringsAsFactors = F)%>%filter(Node!='Perturbation'))
  res$sifAll <- list()
  res$attributesAll <- list()

  for (counter in counterlist) { # please define the number solutions
    if(file.exists(paste0("interactions_1_model",counter,".tsv"))){res$sifAll[[counter]] <- as.matrix(read.table(paste0("interactions_1_model",counter,".tsv"),header = T,sep = "\t",stringsAsFactors = F)%>%filter(Node1!='Perturbation'))}
    if(file.exists(paste0("nodesActivity_1_model",counter,".txt"))){res$attributesAll[[counter]] <- as.matrix(read.table(paste0("nodesActivity_1_model",counter,".txt"),header = T,sep = "\t",stringsAsFactors = F)%>%filter(Nodes!='Perturbation'))}
  }

  return(res)

}

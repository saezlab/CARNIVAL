files2res <- function() {

  res <- list()
  res$weightedSIF <- as.matrix(read.table("weightedModel_1.txt",header = T,sep = "\t",stringsAsFactors = F))
  res$nodeAttributes <- as.matrix(read.table("nodesAttributes_1.txt",header = T,sep = "\t",stringsAsFactors = F))
  res$sifAll <- list()
  res$attributesAll <- list()
  
  for (counter in 1:10) { # please define the number solutions
    res$sifAll[[counter]] <- as.matrix(read.table(paste0("interactions_1_model",counter,".tsv"),header = T,sep = "\t",stringsAsFactors = F))
    res$attributesAll[[counter]] <- as.matrix(read.table(paste0("nodesActivity_1_model",counter,".txt"),header = T,sep = "\t",stringsAsFactors = F))
  }
  
  return(res)

}